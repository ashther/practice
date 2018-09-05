
library(plumber)

pr <- plumb('/home/ashther/utqe-plumber/api.R')
pr$run(host = '0.0.0.0', port = 8000, swagger = FALSE, debug = FALSE)