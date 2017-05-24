# transform passenger flow to data frame
pfToDf <- function(pf, min_date, max_date) {
  wd <- weekdays(as.Date(names(pf)), abbreviate = TRUE)[1]
  time_seq <- seq(as.Date(min_date), as.Date(max_date), by = 'day')
  time_seq <- time_seq[weekdays(time_seq, abbreviate = TRUE) == wd]
  
  lapply(time_seq, function(x) {
    if (as.character(x) %in% names(pf)) {
      temp <- pf[[as.character(x)]]
      temp <- stats::filter(temp$n, rep(1/25, 25)) %>% na.omit() %>% as.numeric()
      data.frame(time = seq_along(temp), 
                 n = temp, 
                 date = x, 
                 stringsAsFactors = FALSE) %>% 
        return()
    } else {
      return(data.frame(time = NA, n = NA, date = x, stringsAsFactors = FALSE))
    }
  }) %>% 
    do.call(rbind, .)
}

# use holtwinter method to model and forecast passenger flow data frame
hwForecast <- function(df, predict_date) {
  require(forecast)
  sapply(unique(df$time), function(x) {
    filter(df, date < predict_date & time == x) %>% 
      select(n) %>% 
      extract2(1) %>% 
      ts() %>% 
      HoltWinters(beta = FALSE, gamma = FALSE) %>%
      forecast.HoltWinters(h = 1) %>%
      extract2('mean')
  }, USE.NAMES = FALSE) %>% 
    unname()
}

# plot passenger flow one-day forecast plot
hwForecastPlot <- function(pf, min_date = '2017-02-01', max_date = '2017-04-30') {
  df <- pf %>% 
    pfToDf(min_date, max_date) %>% 
    filter(!is.na(n))
  predict_date <- names(pf)[length(pf)]
  n_fore <- hwForecast(df, predict_date)
  
  Sys.setlocale("LC_TIME", "C")
  plot(df[df$date == predict_date, 'n'], 
       type = 'l', 
       ylab = 'n', 
       main = sprintf('%s %s', predict_date, weekdays(as.Date(predict_date))))
  points(n_fore, col = 'red')
  mape <- mean(abs(df[df$date == predict_date, 'n'] - n_fore) / 
                 df[df$date == predict_date, 'n']) * 100
  text(x = 100, y = 50, labels = sprintf('mape: %s', round(mape, 2)))
}

# pf_list <- list(pf_mon = pf_mon, pf_tue = pf_tue, pf_wed = pf_wed,
#                 pf_thu = pf_thu, pf_fri = pf_fri, pf_sat = pf_sat,
#                 pf_sun = pf_sun)
hwForecastMultPlot <- function(pf_list, min_date = '2017-02-01', max_date = '2017-04-30') {
  require(gridExtra)
  require(pbapply)
  plot <- list()
  i <- 1
  pb <- startpb(0, length(pf_list))
  on.exit(closepb(pb))
  for(n in names(pf_list)) {
    pf <- pf_list[[n]]
    df <- pf %>% 
      pfToDf(min_date, max_date) %>% 
      filter(!is.na(n))
    predict_date <- names(pf)[length(pf)]
    n_fore <- hwForecast(df, predict_date)
    n_avg <- df %>% group_by(time) %>% summarise(n = mean(n)) %>% extract2('n')
    
    Sys.setlocale("LC_TIME", "C")
    mape_fore <- mean(abs(df[df$date == predict_date, 'n'] - n_fore) / 
                   df[df$date == predict_date, 'n']) * 100
    mape_avg <- mean(abs(df[df$date == predict_date, 'n'] - n_avg) / 
                        df[df$date == predict_date, 'n']) * 100
    
    plot[[i]] <- 
      ggplot(filter(df, date == predict_date), 
             aes(x = time, y = n)) + 
      geom_line() + 
      ggtitle(label = sprintf('%s %s', predict_date, weekdays(as.Date(predict_date)))) + 
      geom_point(data = data.frame(time = seq_along(n_fore), n = n_fore), 
                 aes(x = time, y = n), col = 'red', alpha = 0.2) + 
      geom_point(data = data.frame(time = seq_along(n_avg), n = n_avg), 
                 aes(x = time, y = n), col = 'blue', alpha = 0.2) + 
      annotate('text', x = 100, y = 50, label = sprintf('forecast mape: %s', round(mape_fore, 2))) + 
      annotate('text', x = 100, y = 25, label = sprintf('average mape: %s', round(mape_avg, 2)))
    
    if (i %% 9 == 0) {
      print(do.call(grid.arrange, plot))
      i <- 0
      plot <- list()
    }
    i <- i + 1
    setpb(pb, i - 1)
  }
  if (length(plot) != 0) {
    print(do.call(grid.arrange, plot))
  }
}

###############################################################################
# forecast in one day 
###############################################################################

# weightFind <- function(df, train_end, test_start, test_end, n_fore, validation_date) {
#   weight <- seq(0.01, 0.99, 0.02)
#   pfts <- ts(filter(df, date == validation_date)$n)
#   trainset <- window(pfts, end = train_end)
#   testset <- window(pfts, start = test_start, end = test_end)
#   hw_gamma <- forecast.HoltWinters(HoltWinters(trainset, gamma = FALSE), h = test_end - train_end)
#   idx <- sapply(weight, function(x) {
#     temp <- as.numeric(hw_gamma$mean) * x + n_fore[test_start:test_end] * (1 - x)
#     return(mean(abs(as.numeric(testset) - temp) / as.numeric(testset)))
#   }) %>% 
#     which.min()
#   return(weight[idx])
# }


# forecast one day passenger flow at specific time point, use n_fore for adjusting
singleDayForecastPlot <- function(df, end_prop, n_fore, weight = 0.67, 
                                  train_len = NULL, 
                                  predict_date = NULL, h = 6, is_plot = TRUE) {
  require(dplyr)
  require(magrittr)
  require(forecast)
  
  if (is.null(predict_date)) {
    pfts <- ts(filter(df, date == max(df$date))$n)
  } else {
    pfts <- ts(filter(df, date == predict_date)$n)
  }
  train_end <- floor(length(pfts) * end_prop)
  if (is.null(train_len)) {
    train_start <- 1
  } else {
    train_start <- max(1, train_end - train_len)
  }
  test_start <- train_end + 1
  test_end <- ifelse((test_start + h - 1) <= length(pfts), 
                     test_start + h - 1, 
                     length(pfts))
  # trainset <- window(pfts, end = train_end)
  trainset <- window(pfts, start = train_start, end = train_end)
  testset <- window(pfts, start = test_start, end = test_end)
  # trainset_plot <- window(trainset, start = max(1, length(trainset) - 3*h))
  
  hw_gamma <- forecast.HoltWinters(HoltWinters(trainset, gamma = FALSE), h = h)
  # hw_beta_gamma <- forecast.HoltWinters(HoltWinters(trainset, beta = FALSE, gamma = FALSE), h = h)
  # arima_model <- forecast.Arima(auto.arima(trainset), h = h)
  
  hw_gamma$mean <- ts(as.numeric(hw_gamma$mean) * weight + 
                        n_fore[test_start:test_end] * (1 - weight), 
                      start = test_start, end = test_end)
  # hw_beta_gamma$mean <- ts(as.numeric(hw_beta_gamma$mean) * weight + n_fore[test_start:test_end] * (1 - weight), 
  #                     start = test_start, end = test_end)
  # arima_model$mean <- ts(as.numeric(arima_model$mean) * weight + n_fore[test_start:test_end] * (1 - weight), 
  #                     start = test_start, end = test_end)
  
  models <- list(hw_gamma = hw_gamma
                 # hw_beta_gamma = hw_beta_gamma, 
                 # arima_model = arima_model
                 )
  
  p <- autoplot(cbind(trainset, testset)) + 
    autolayer(hw_gamma, PI = FALSE, series = 'forecast') + 
    theme(legend.position = 'none')
    # autolayer(hw_beta_gamma, PI = FALSE, series = 'hw_beta_gamma') + 
    # autolayer(arima_model, PI = FALSE, series = 'arima')
  
  if (is_plot) {
    print(p)
  }
  
  error <- sapply(models, function(x) {
    accuracy(x, testset)[2, ]
  }) %>% 
    t() %>% 
    set_rownames(names(models)) %>% 
    extract(, c('RMSE', 'MAPE', 'MASE'))
  
  return(list(p = p, error = error))
}

# forecast one day passenger flow at a time point sequence, use n_fore for adjusting
singleDayForecastMultPlot <- function(df, n_fore, weight = 0.67, 
                                      train_len, 
                                      predict_date = NULL, h = 6, is_plot = TRUE) {
  require(gridExtra)
  p_list <- lapply(seq(0.1, 0.9, 0.05), function(x) {
    singleDayForecastPlot(df, x, n_fore, weight, 
                          train_len, 
                          predict_date, h, FALSE)
  })
  
  hw_gamma_error <- rowMeans(sapply(p_list, extract2, 'error'))
  # hw_beta_gamma_error <- rowMeans(sapply(p_list, function(p)p$error['hw_beta_gamma', ]))
  # arima_model_error <- rowMeans(sapply(p_list, function(p)p$error['arima_model', ]))
  
  if (is_plot) {
    do.call(grid.arrange, lapply(p_list, extract2, 'p'))
  }
  return(hw_gamma_error)
}

df <- pfToDf(pf_thu, min_date, max_date) %>% 
  filter(!is.na(n))

validation_date <- sort(unique(df$date))[length(unique(df$date)) - 1]
# validation_date <- max(df$date)

n_fore <- hwForecast(df, validation_date)
argGrid <- expand.grid(weight = seq(0.01, 0.99, 0.03), len = seq(10, 200, 10))
argGrid$mape <- pbapply::pbapply(argGrid, 1, function(x) {
  singleDayForecastMultPlot(df, n_fore, weight = x['weight'], train_len = x['len'], 
                            predict_date = validation_date, h = 6, is_plot = FALSE) %>% 
    extract('MAPE') %>% 
    round(3)
})
ggplot(argGrid, aes(weight, len)) + 
  geom_tile(aes(fill = mape)) + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  ggtitle(label = sprintf('weight: %s, len: %s, MAPE: %s', 
                          argGrid$weight[which.min(argGrid$mape)], 
                          argGrid$len[which.min(argGrid$mape)], 
                          min(argGrid$mape)))

singleDayForecastMultPlot(df, hwForecast(df, max(df$date)), 
                          weight = argGrid$weight[which.min(argGrid$mape)], 
                          # weight = seq_test[which.min(temp)], 
                          train_len = argGrid$len[which.min(argGrid$mape)], 
                          predict_date = max(df$date), is_plot = TRUE)
