dataPrepare <- function(people) {
  suppressPackageStartupMessages(require(dplyr))
  people <- jsonlite::fromJSON(people) %>% 
    magrittr::set_colnames(c('scene', 'episode', 'people'))
  people %>% 
    group_by(people, episode, scene) %>% 
    tally() %>% 
    left_join(people, ., by = c('people' = 'people', 
                                'scene' = 'scene', 
                                'episode' = 'episode'))
}

netCreate <- function(people, thr = 0.85) {
  suppressPackageStartupMessages(require(dplyr))
  
  net <- people %>% 
    dataPrepare() %>% 
    widyr::pairwise_count(people, scene, wt = n, sort = TRUE) %>% 
    filter(n > quantile(pull(., n), thr)) %>% 
    igraph::graph_from_data_frame()
  E(net)$weight <- 1/E(net)$n
  return(net)
}

netCentrGet <- function(people, thr = 0.90) {
  tryCatch({
    suppressPackageStartupMessages({
      require(tibble)
      require(dplyr)
      require(igraph)
      require(luzlogr)
    })
    openlog('~/log/netCentrGet.log', append = TRUE, sink = TRUE)
    on.exit(closelog(sessionInfo = FALSE))
    
    graph_centr <- c()
    node_centr <- data.frame()
    
    net <- netCreate(people, thr)
    printlog('net created')
    
    deg <- centr_degree(net, mode = 'out')
    clo <- centr_clo(net, mode = 'out')
    btw <- centr_betw(net)
    eg <- evcent(net, directed = FALSE, weights = E(net)$n)$vector # directed = true cause r session crash
    graph_centr <- list(
      'edge_density' = round(edge_density(net, loops = TRUE), 3), 
      'diameter' = diameter(net, directed = FALSE), 
      'centr_degree' = round(deg$centralization, 3), 
      'centr_clo' = round(clo$centralization, 3), 
      'centr_btw' = round(btw$centralization, 3)
    )
    printlog('graph centr caculated')
    
    node_centr <- data_frame(name = V(net)$name, 
                             degree = deg$res, 
                             closeness = round(closeness(net, mode = 'out', normalized = TRUE), 3), 
                             betweenness = round(betweenness(net, normalized = TRUE), 3), 
                             eigenvector = round(eg, 3)) %>% 
      arrange(desc(eigenvector))
    printlog('node centr caculated')
    
    jsonlite::toJSON(list(state = 0, 
                          message = 'completed', 
                          graph_centr = graph_centr, 
                          node_centr = node_centr), 
                     auto_unbox = TRUE)
  }, 
  error = function(e) {
    printlog(sprintf('error: %s', e$message))
    jsonlite::toJSON(list(state = 1, 
                          message = e$message, 
                          graph_centr = graph_centr, 
                          node_centr = node_centr), 
                     auto_unbox = TRUE)
  })
}

netAttrGet <- function(people, thr = 0.90) {
  tryCatch({
    suppressPackageStartupMessages({
      require(igraph)
      require(tibble)
      require(dplyr)
      require(magrittr)
      require(luzlogr)
    })
    openlog('~/log/netAttrGet.log', append = TRUE, sink = TRUE)
    on.exit(closelog(sessionInfo = FALSE))
    
    net <- netCreate(people, thr)
    printlog('net created')
    
    mbs <- cluster_fast_greedy(as.undirected(net))$membership %>% 
      add(-1) %>% 
      set_names(V(net)$name) %>% 
      enframe()
    # mbs_color <- RColorBrewer::brewer.pal(length(unique(mbs$value)), 'Set1')
    # mbs_color <- rainbow(length(unique(mbs$value)))
    categories <- lapply(sort(unique(mbs$value)), function(x) {
      list(
        name = paste0(mbs$name[mbs$value == x][1], '等人')
        # itemStyle = list(normal = list(
        #   color = mbs_color[which(unique(mbs$value) == x)]
        # ))
      )
    })
    printlog(sprintf('categories created: %s', length(categories)))
    
    eg <- evcent(net, directed = FALSE, weights = E(net)$n)$vector %>% # directed = true cause r session crash
      multiply_by(50) %>% 
      enframe()
    nodes <- left_join(mbs, eg, by = 'name') %>% 
      mutate(draggable = TRUE) %>% 
      set_colnames(c('name', 'category', 'symbolSize', 'draggable')) %>% 
      mutate(value = name)
    printlog(sprintf('nodes created: %s', nrow(nodes)))
    
    links <- get.edgelist(net) %>% 
      cbind(E(net)$n / max(E(net)$n) * 10) %>% # E(net)$n / max(E(net)$n) * 30 change after 2017.8.2
      as.data.frame(stringsAsFactors = FALSE) %>% 
      set_colnames(c('source', 'target', 'n'))
    links <- lapply(seq_len(nrow(links)), function(x) {
      list(
        source = links$source[x], 
        target = links$target[x], 
        lineStyle = list(normal = list(
          width = links$n[x]
        ))
      )
    })
    printlog(sprintf('links created: %s', length(links)))
    
    jsonlite::toJSON(list(state = 0, 
                          message = 'completed', 
                          categories = categories, 
                          nodes = nodes, 
                          links = links), 
                     auto_unbox = TRUE)
  }, 
  error = function(e) {
    printlog(sprintf('error: %s', e$message))
    jsonlite::toJSON(list(state = 1, message = e$message), 
                     auto_unbox = TRUE)
  })
}
