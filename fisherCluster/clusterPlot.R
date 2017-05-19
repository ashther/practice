# get cluster solution on specific time series
timeSeriSplitPlot <- function(n, lossMtx, k, start_time) {
  require(magrittr)
  split_point <- fisherClust(lossMtx, k = k)$cluster %>% 
    extract(-length(.))
  start_time <- as.POSIXct(start_time, format = '%H:%M:%S')
  
  split_point_time <- sapply(split_point, function(x) {
    return(format(as.difftime(as.integer(x[1]) * 5, units = 'mins') + start_time, 
                  format = '%H:%M'))
  }) %>%  
    as.character()
  
  plot(n, type = 'l', main = sprintf('k = %s', k))
  split_point %>% 
    sapply(function(x) {
      x[1]
    }) %>% {
      abline(v = ., col = 'red', lwd = 2)
      invisible(lapply(., function(x) {
        text(x, 2, split_point_time[which(. == x)], col = 'blue')
      }))
    }
}

# get loss-k plot
lossSeqPlot <- function(lossMtx, k_seq) {
  require(magrittr)
  sapply(k_seq, function(k) {
    fisherClust(lossMtx, k = k)$loss
  }) %>% 
    plot(type = 'l', lwd = 1.5, ylab = 'loss', main = 'loss at different k')
}

# get mutiple plot of different time series which are same weekdays
multiWeekDayPlot <- function(pf, min_date = '2017-02-01', 
                             max_date = '2017-04-30', save = FALSE) {
  require(ggplot2)
  require(magrittr)
  df <- pfToDf(pf, min_date, max_date)
  
  label <- weekdays(df$date[1], TRUE)
  pf_without_date <- df[, -3]
  p <- df %>% 
    ggplot(aes(x = time, y = n)) + 
    geom_line(aes(group = date), col = 'red') + 
    geom_line(data = pf_without_date, col = 'grey', alpha = 0.5) + 
    facet_wrap(~ date) + 
    guides(col = FALSE) + 
    theme_bw() + 
    ggtitle(label = label)
  print(p)
  if (save) {
    ggsave(paste0('Rplot/', label, '.png'), plot = p)
  }
}

