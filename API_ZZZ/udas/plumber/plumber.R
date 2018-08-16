
library(plumber)

HOME_PATH <- '/home/rstudio'
root <- plumber$new()

api_selection     <- plumber$new(file.path(HOME_PATH, 'api-selection.R'))
api_summary       <- plumber$new(file.path(HOME_PATH, 'api-summary.R'))
api_matriculation <- plumber$new(file.path(HOME_PATH, 'api-matriculation.R'))
api_enroll        <- plumber$new(file.path(HOME_PATH, 'api-enroll.R'))
api_teach         <- plumber$new(file.path(HOME_PATH, 'api-teach.R'))

root$mount('/selection', api_selection)
root$mount('/summary', api_summary)
root$mount('/matriculation', api_matriculation)
root$mount('/enroll', api_enroll)
root$mount('/teach', api_teach)
root$run(host = '0.0.0.0', port = 8001, swagger = TRUE, debug = FALSE)
