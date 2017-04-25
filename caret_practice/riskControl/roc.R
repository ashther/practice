
library(pROC)

data("aSAH")
roc_1 <- plot.roc(aSAH$outcome, aSAH$s100b, col = '1')
roc_2 <- lines.roc(aSAH$outcome, aSAH$ndka, col = '2')
roc_compare <- roc.test(roc_1, roc_2)

text(0.5, 0.5, 
     labels = paste0('p-value :', format.pval(roc_compare$p.value)), 
     adj = c(0, 0.25))
legend('bottomright', 
       legend = c('s100b', 'ndka'), 
       col = c('1', '2'), 
       lwd = 2)
