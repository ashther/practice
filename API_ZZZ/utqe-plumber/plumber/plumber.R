
library(plumber)

pr <- plumb('/home/rstudio/api.R')
pr$run(host = '0.0.0.0', port = 8002, swagger = FALSE, debug = FALSE)
