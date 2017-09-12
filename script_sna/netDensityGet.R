# replace the caculation process of meilin for character relationship complexity
netDensityGet <- function(episode_root, host_path = HOST_PATH,
                          episode_from = 0, episode_to = 999,
                          thr = THR_netGet, log_path = LOG_PATH) {
  
  tryCatch({
    suppressPackageStartupMessages({
      require(igraph)
      require(tibble)
      require(dplyr)
      require(magrittr)
      require(luzlogr)
    })
    
    if (!dir.exists(log_path)) {
      dir.create(log_path)
    }
    openlog(file.path(log_path, 'netDensityGet.log'), append = TRUE)
    on.exit(closelog(sessionInfo = FALSE))
    
    sql <- sprintf(
      paste0('select episode, scene, actor, frequency from episode_actor_play ', 
             'where episode between %s and %s;'), 
      episode_from, episode_to
    )
    
    # tranform db path on host to path in docker container
    db_path <- file.path(sub(host_path, '', episode_root), 'meta.db')
    
    # creat cooc net structure
    net_cooc <- netCoocCreate(db_path, sql, thr)
    printlog('net_cooc created')
    
    net_density <- round(edge_density(net_cooc, loops = TRUE), 4)
    net_edge_n <- length(E(as.undirected(net_cooc)))
    
    jsonlite::toJSON(list(code = 0,
                          msg = '',
                          data = list(complexity = net_density, 
                                      relation_n = net_edge_n)), 
                     auto_unbox = TRUE)
  }, 
  error = function(e) {
    printlog(sprintf('error: %s', e$message))
    jsonlite::toJSON(list(code = -1,
                          msg = e$message), 
                     auto_unbox = TRUE)
  })
}