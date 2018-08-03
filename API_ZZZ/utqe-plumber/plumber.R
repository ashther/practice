
library(plumber)

pr <- plumb('/home/ashther/udas/api.R')
pr$run(host = '0.0.0.0', port = 8000)
