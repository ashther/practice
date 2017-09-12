# due to th performance, we need the character importance at once, 
# as a result, we seperate the importance caculation of the main process,
# and add the location argument for the users.
netEvGet <- function(episode_root, host_path = HOST_PATH, 
                     episode_from = 0, 
                     episode_to = 999, 
                     location = NULL, 
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
    openlog(file.path(log_path, 'netEvGet.log'), append = TRUE)
    on.exit(closelog(sessionInfo = FALSE))
    
    if (!is.null(location)) {
      sql <- sprintf(
        paste0(
          'select distinct ep_ap.episode episode, ep_ap.scene scene, ',
          'ep_ap.actor actor, ep_ap.frequency frequency, ep_in.location loc ',
          'from episode_actor_play as ep_ap left join episode_index as ep_in ',
	  'on ep_ap.episode = ep_in.episode and ep_ap.scene = ep_in.scene ',
          'where ep_ap.episode between %s and %s ',
          'and ep_in.episode between %s and %s ', 
          'and ep_in.location = \'%s\';'
        ), episode_from, episode_to, episode_from, episode_to, location
      )
    } else {
      sql <- sprintf(
        paste0('select episode, scene, actor, frequency from episode_actor_play ', 
               'where episode between %s and %s;'), episode_from, episode_to
      )
    }
    
    # tranform db path on host to path in docker container
    db_path <- file.path(sub(host_path, '', episode_root), 'meta.db')
    
    # creat cooc net structure
    net_cooc <- netCoocCreate(db_path, sql, thr)
    printlog('net_cooc created')
    
    # directed = true cause r session crash
    eg <- evcent(net_cooc, directed = FALSE, weights = E(net_cooc)$n)$vector 
    
    importance <- data_frame(
      name = V(net_cooc)$name,
      eigenvector = round(eg, 3)
    ) %>% 
      arrange(desc(eigenvector))
    printlog('eigen vector caculated')
    
    jsonlite::toJSON(list(code = 0,
                          msg = '',
                          data = importance), 
                     auto_unbox = TRUE)
  }, 
  error = function(e) {
    printlog(sprintf('error: %s', e$message))
    jsonlite::toJSON(list(code = -1,
                          msg = e$message), 
                     auto_unbox = TRUE)
  })
}
