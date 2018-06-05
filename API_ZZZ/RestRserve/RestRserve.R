

# preparation ------------------------------------------------------------

HOME_PATH <- '/home/rstudio'
source(file.path(HOME_PATH, 'api_sna.R'))

# create application and register endpoints ------------------------------

RestRserveApp <- RestRserve::RestRserveApplication$new()
RestRserveApp$add_get(path = "/graph", FUN = graphGetFilter)
RestRserveApp$add_get(path = '/graph/avg_centr', FUN = graphAvgCentrReadFilter)
RestRserveApp$add_openapi(path = '/openapi.yaml', file_path = 'openapi.yaml')
RestRserveApp$add_swagger_ui(path = '/swagger', 
                             path_openapi = '/openapi.yaml', 
                             path_swagger_assets = '/__swagger__')

RestRserveApp$run(http_port = "8000", 
                  encoding = 'utf8', 
                  port = '6311', 
                  remote = 'enable')


