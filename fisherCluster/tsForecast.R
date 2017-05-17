# rolling average version
# try original version
hwForecast <- function(df, predict_date) {
  require(forecast)
  sapply(unique(df$time), function(x) {
    filter(df, date != predict_date & time == x) %>% 
      select(n) %>% 
      extract2(1) %>% 
      ts() %>% 
      HoltWinters(beta = FALSE, gamma = FALSE) %>% 
      forecast.HoltWinters(h = 1) %>% 
      extract2('mean')
  }, USE.NAMES = FALSE) %>% 
    unname()
}
