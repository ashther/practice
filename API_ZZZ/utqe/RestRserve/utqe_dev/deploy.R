
RestRserve::restrserve_deploy(
  file = 'deploy_local.R', 
  dir = 'deploy/', 
  configuration = c("http.port" = "8001",
                    "encoding" = "utf8",
                    "port" = "6311", 
                    'remote' = 'enable')
)

RestRserve::restrserve_start('deploy/')

RestRserve::restrserve_stop('deploy/')
