
library(RestRserve)

RestRserve::restrserve_deploy('97-RestRserve.R', 'RestRserve/', 
                  configuration = c('http.port' = '8000', 
                                    'encoding' = 'utf8', 
                                    'port' = '6311', 
                                    'remote' = 'enable'))
RestRserve::restrserve_start('RestRserve/')

test <- httr::GET('http://127.0.0.1:8000/graph?id=386&is_dep=TRUE')
httr::status_code(test)
temp <- httr::content(test)
jsonlite::toJSON(temp, auto_unbox = TRUE, pretty = TRUE, na = 'null', null = 'null')

RestRserve::restrserve_stop('RestRserve/')
