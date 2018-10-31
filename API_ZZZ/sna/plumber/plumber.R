
library(plumber)

isInDocker <- function() {
  group_info <- system('cat /proc/1/cgroup', intern = TRUE)
  any(grepl('docker', group_info)) | file.exists('/.dockerenv')
}

if (isTRUE(isInDocker())) {
  HOME_PATH <- '/home/rstudio'
  config <- jsonlite::fromJSON(file.path(HOME_PATH, 'config.json'))
  port <- config$port
} else {
  HOME_PATH <- '/home/ashther/user_profile_sna'
  port <- 8000
}

pr <- plumb(file.path(HOME_PATH, 'api.R'))
pr$run(host = '0.0.0.0', port = port, swagger = TRUE, debug = FALSE)
