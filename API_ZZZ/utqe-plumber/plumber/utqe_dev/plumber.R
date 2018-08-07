
library(plumber)

pr <- plumb('/home/ashther/udas/api.R')
suppressMessages(
  pr$run(host = '0.0.0.0', port = 8000, swagger = FALSE, debug = FALSE)
)
