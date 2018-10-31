
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
  HOME_PATH <- '/home/ashther/udas'
  port <- 8000
}

root <- plumber$new()

api_selection     <- plumber$new(file.path(HOME_PATH, 'api-selection.R'))
api_summary       <- plumber$new(file.path(HOME_PATH, 'api-summary.R'))
api_matriculation <- plumber$new(file.path(HOME_PATH, 'api-matriculation.R'))
api_enroll        <- plumber$new(file.path(HOME_PATH, 'api-enroll.R'))
api_employment    <- plumber$new(file.path(HOME_PATH, 'api-employment.R'))

root$mount('/selection', api_selection)
root$mount('/summary', api_summary)
root$mount('/matriculation', api_matriculation)
root$mount('/enroll', api_enroll)
root$mount('/employment', api_employment)
root$run(host = '0.0.0.0', port = port, swagger = FALSE, debug = FALSE)
