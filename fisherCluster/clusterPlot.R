
timeSeriSplitPlot <- function(pf, lossMtx, k) {
  require(magrittr)
  split_point <- fisherClust(lossMtx, k = k)$cluster
  
  split_point_time <- sapply(split_point, function(x) {
    return(format(as.difftime(as.integer(x[1]) * 5, units = 'mins') + start_time, 
                  format = '%H:%M'))
  }) %>%  
    as.character()
  
  plot(pf$time, pf$n, type = 'l', main = sprintf('k = %s', k))
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

lossSeqPlot <- function(lossMtx, k_seq) {
  require(magrittr)
  sapply(k_seq, function(k) {
    fisherClust(lossMtx, k = k)$loss
  }) %>% 
    plot(type = 'l', lwd = 1.5, ylab = 'loss', main = 'loss at different k')
}
