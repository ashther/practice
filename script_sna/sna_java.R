
# net data prepare and creat ---------------------------------------------
# db_path <- 'e:/work/meta.db'
# sql_actor_play <- 'select * from episode_actor_play'
netCoocCreate <- function(db_path, sql_actor_play, thr) {
  require(dplyr)
  require(widyr)
  require(igraph)
  # require(magrittr)
  require(tidyr)
  # require(jsonlite)

  # net <- fromJSON(people) %>%
  #   set_colnames(c('scene', 'episode', 'people')) %>% 
  #   group_by(people, episode, scene) %>%
  #   tally() %>%  
  #   ungroup() %>% 
  #   pairwise_count(people, scene, wt = n, sort = TRUE) %>% 
  #   filter(n > quantile(pull(., n), thr)) %>% 
  #   graph_from_data_frame()
  
  net <- dataGetFromSQLite(db_path, sql_actor_play) %>% 
    unite(new_scene, episode, scene) %>% 
    pairwise_count(actor, new_scene, wt = frequency) %>% 
    filter(n > quantile(.$n, thr)) %>% 
    graph_from_data_frame()
  
  E(net)$weight <- 1/E(net)$n
  return(net)
}

# sql_talk <- 'select * from episode_talk'
# sql_seg_word <- 'select episode, scene, section, word from episode_word where stopWord != 1 and word != ""'
netSentCreate <- function(db_path, sql_talk, sql_seg_word, 
                          nodes_name, termVec, 
                          termVecSentMtx, termVecSentDT) {
  require(igraph)
  require(data.table)
  
  talk <- dataGetFromSQLite(db_path, sql_talk)
  
  talk <- talk %>% 
    filter(from != '') %>%
    mutate(prv = lag(from), nxt = lead(from), 
         prv_ep = lag(episode), prv_sc = lag(scene), prv_se = lag(section), 
         nxt_ep = lead(episode), nxt_sc = lead(scene), nxt_se = lead(section), 
         prv_to = episode == prv_ep & scene == prv_sc & (section == prv_se + 1) & 
           !is.na(prv) & from != prv, 
         nxt_to = !(episode == prv_ep & scene == prv_sc & section == prv_se + 1) &
           episode == nxt_ep & scene == nxt_sc & (section + 1 == nxt_se) & 
           !is.na(nxt) & from != nxt) %>% 
    filter(prv_to | nxt_to) %>% 
    mutate(to = if_else(prv_to, prv, nxt)) %>% 
    select(episode, scene, section, from, to)
  
  seg_word <- dataGetFromSQLite(db_path, sql_seg_word)
  script_keywords <- left_join(talk, seg_word, by = c("episode", "scene", "section")) %>% 
    filter(from != word)
  
  script_keywords <- as.data.table(script_keywords)
  script_keywords <- tfIdfCaculate(script_keywords)
  
  # temporary use sentCaculateTest, for test
  net <- sentCaculateTest(script_keywords, nodes_name,
                          termVec, termVecSentMtx, termVecSentDT) %>% 
    graph_from_data_frame()
  E(net)$weight <- 1/E(net)$n
  return(net)
}

# net attribute and centr get --------------------------------------------

netCoocAttrGet <- function(net) {
  require(igraph)
  require(tibble)
  require(dplyr)
  require(magrittr)
  
  # net <- as.undirected(net) need to be done 2017.8.26
  # for we need to show graph with undiredted form on web page
  # don't confuse echarts with nossy json data
  net <- as.undirected(net)
  
  mbs <- cluster_fast_greedy(net)$membership %>% 
    add(-1) %>% 
    set_names(V(net)$name) %>% 
    enframe()
  categories <- lapply(sort(unique(mbs$value)), function(x) {
    list(
      name = paste0(mbs$name[mbs$value == x][1], '等人')
    )
  })
  
  eg <- evcent(net, directed = FALSE, weights = 1/E(net)$weight)$vector %>% # directed = true cause r session crash
    multiply_by(50) %>% 
    enframe()
  nodes <- left_join(mbs, eg, by = 'name') %>% 
    mutate(draggable = TRUE) %>% 
    set_colnames(c('name', 'category', 'symbolSize', 'draggable')) %>% 
    mutate(value = name)
  
  links <- get.edgelist(net) %>% 
    cbind(min(E(net)$weight) / E(net)$weight * 10) %>% # E(net)$n / max(E(net)$n) * 30 change after 2017.8.2
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
  
  return(list(categories = categories, nodes = nodes, links = links))
}

netCoocCentrGet <- function(net) {
  require(tibble)
  require(dplyr)
  require(igraph)
  
  graph_centr <- c()
  node_centr <- data.frame()
  
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
  
  node_centr <- data_frame(
    name = V(net)$name, 
    degree = round(deg$res / max(deg$res), 3), # normalize degree manually 
    closeness = round(closeness(net, mode = 'out', normalized = TRUE), 3), 
    betweenness = round(betweenness(net, normalized = TRUE), 3), 
    eigenvector = round(eg, 3)
  ) %>% 
    arrange(desc(eigenvector))
  
  return(list(graph_centr = graph_centr, node_centr = node_centr))
}

netSentAttrGet <- function(net) {
  require(igraph)
  require(dplyr)
  
  net <- as.undirected(net)
  
  mbs <- cluster_fast_greedy(net)$membership %>%
    add(-1) %>% 
    set_names(V(net)$name) %>% 
    enframe()
  categories <- lapply(sort(unique(mbs$value)), function(x) {
    list(
      name = paste0(mbs$name[mbs$value == x][1], '等人')
    )
  })
  
  deg <- enframe(degree(net, normalized = TRUE) * 50) # 50 for visualization
  nodes <- left_join(mbs, deg, by = 'name') %>% 
    mutate(draggable = TRUE) %>% 
    set_colnames(c('name', 'category', 'symbolSize', 'draggable')) %>% 
    mutate(value = name)
  
  links <- cbind(get.edgelist(net), 'rgb(176, 23, 31)') # red
  links[apply(links, 1, function(x) {
    mbs$value[mbs$name == x[1]] == mbs$value[mbs$name == x[2]]
  }), 3] <- 'rgb(0, 201, 87)' # green
  
  # color <- rep('rgba(176, 23, 31, %s)', nrow(links)) # red
  # color[E(net)$n >= 1] <- 'rgba(0, 201, 87, %s)' # green
  # color <- sprintf(color, abs(E(net)$n - 1)/max(abs(E(net)$n - 1)))
  
  links <- lapply(seq_len(nrow(links)), function(x) {
    list(
      source = links[x, 1], 
      target = links[x, 2], 
      lineStyle = list(normal = list(
        color = links[x, 3]
      ))
    )
  })
  
  return(list(categories = categories, nodes = nodes, links = links))
}

egoNetAttrGet <- function(net) {
  require(igraph)
  require(tibble)
  require(dplyr)
  require(magrittr)
  
  net <- as.undirected(net)
  ego_net <- make_ego_graph(net, 1)
  ego_net <- lapply(ego_net, function(x) {
    nodes <- mutate(enframe(evcent(x, weights = 1/E(x)$weight)$vector * 50, 
                            'name', 'symbolSize'), 
                    draggable = TRUE, value = name)
    links <- cbind(get.edgelist(x), min(E(x)$weight) / E(x)$weight * 10)  
    links <- lapply(seq_len(nrow(links)), function(x) {
      list(
        source = links[x, 1], 
        target = links[x, 2], 
        lineStyle = list(normal = list(
          width = links[x, 3]
        ))
      )
    })
    return(list(nodes = nodes, links = links))
  })
  return(setNames(ego_net, V(net)$name))
}


# sentiment relation between nodes and tfidf caculate --------------------

tfIdfCaculate <- function(dt) {
  require(data.table)
  require(jiebaR)
  require(tidyr)
  require(magrittr)
  
  idf_temp <- dt %>% 
    unite(new_id, episode, scene, section) %>% 
    {split(.$word, .$new_id)} %>% 
    get_idf() %>% 
    set_colnames(c('word', 'idf')) %>% 
    as.data.table()
  
  result <- idf_temp[dt, on = 'word']
  result[, .(tfidf = sum(idf, na.rm = TRUE)), .(from, to, word)]
}

sentCaculate <- function(script_keywords, nodes_name, termVec, 
                         termVecSentMtx, termVecSentDT) {
  require(data.table)
  # script_keywords <- as.data.table(script_keywords)
  script_keywords[from %in% nodes_name & to %in% nodes_name, .(
    n = {
      words_with_tfidf <- unlist(.SD$text)
      if (is.null(words_with_tfidf)) {
        1
      } else {
        temp <- words_with_tfidf[words_with_tfidf %in% rownames(termVec)]
        temp <- data.table(word = unname(temp), value = as.numeric(names(temp)))
        if (nrow(temp) == 0) {
          1
        } else {
          temp <- temp[, .(value = sum(value)), word]
          if (nrow(temp) > 1) {
            temp_vec <- colMeans(termVec[temp$word, ] * temp$value)
          } else {
            temp_vec <- termVec[temp$word, ] * temp$value
          }
          sim <- cosSimCaculateCpp(temp_vec, termVecSentMtx)
          
          termVecSentDTCopy <- copy(termVecSentDT)
          termVecSentDTCopy[, sim := sim]
          termVecSentDTSim <- termVecSentDTCopy[sim > 0]
          termVecSentDTSim <- termVecSentDTSim[, .(prop = sum(sim * strength)), relation]
          termVecSentDTSim[, prop := prop.table(prop)]
          
          # a few improvement in future:
          # custom tf-idf in keywords 
          # n-gram in glove training
          # sum(sim*strength), better fomula?
          
          tryCatch(
            termVecSentDTSim$prop[termVecSentDTSim$relation == 1] / 
              termVecSentDTSim$prop[termVecSentDTSim$relation == -1], 
            error = function(e) {
              termVecSentDTSim$relation[which.max(termVecSentDTSim$prop)] + 1
            }
          )
        }
      }
    }
  ), .(from, to)]
}

sentCaculateTest <- function(script_keywords, nodes_name, termVec, 
                             termVecSentMtx, termVecSentDT) {
  require(data.table)
  # script_keywords <- as.data.table(script_keywords)
  script_keywords[from %in% nodes_name & to %in% nodes_name, .(
    n = {
      if (nrow(.SD) == 0) {
        1
      } else {
        temp <- .SD[word %in% rownames(termVec), .(word, tfidf)]
        if (nrow(temp) == 0) {
          1
        } else {
          if (nrow(temp) > 1) {
            temp_vec <- colMeans(termVec[temp$word, ] * temp$tfidf)
          } else {
            temp_vec <- termVec[temp$word, ] * temp$tfidf
          }
          sim <- cosSimCaculateCpp(temp_vec, termVecSentMtx)
          
          termVecSentDTCopy <- copy(termVecSentDT)
          termVecSentDTCopy[, sim := sim]
          termVecSentDTSim <- termVecSentDTCopy[sim > 0]
          termVecSentDTSim <- termVecSentDTSim[, .(prop = sum(sim * strength)), relation]
          termVecSentDTSim[, prop := prop.table(prop)]
          
          # a few improvement in future:
          # custom tf-idf in keywords 
          # n-gram in glove training
          # sum(sim*strength), better fomula?
          
          tryCatch(
            termVecSentDTSim$prop[termVecSentDTSim$relation == 1] / 
              termVecSentDTSim$prop[termVecSentDTSim$relation == -1], 
            error = function(e) {
              termVecSentDTSim$relation[which.max(termVecSentDTSim$prop)] + 1
            }
          )
        }
      }
    }
  ), .(from, to)]
}

# the only entry function interface --------------------------------------
# THR_netGet = 0.9
# LOG_PATH <- '~/log'
netGet <- function(episode_root, host_path = HOST_PATH, 
                   episode_from = 0, 
                   episode_to = 999, 
                   .termVec = termVec, .termVecSentMtx = termVecSentMtx, 
                   .termVecSentDT = termVecSentDT, 
                   thr = THR_netGet, log_path = LOG_PATH) {
  tryCatch({
    suppressPackageStartupMessages({
      require(igraph)
      require(tibble)
      require(dplyr)
      require(magrittr)
      require(data.table)
      require(luzlogr)
    })
    if (!dir.exists(log_path)) {
      dir.create(log_path)
    }
    openlog(file.path(log_path, 'netGet.log'), append = TRUE)
    on.exit(closelog(sessionInfo = FALSE))
    
    # sqlInterpolate is safe, but lots of code need to be modified in that way
    sql_actor_play <- sprintf(
      paste0('select episode, scene, actor, frequency from episode_actor_play ', 
             'where episode between %s and %s'), 
      episode_from, episode_to
    )
    sql_talk <- sprintf(
      paste0('select episode, scene, section, "from" from episode_talk ', 
             'where episode between %s and %s'), 
      episode_from, episode_to
    )
    sql_seg_word <- sprintf(
      paste0('select episode, scene, section, word from episode_word ', 
             'where episode between %s and %s ', 
             'and stopWord != 1 and word != ""'), 
      episode_from, episode_to
    )
    
    # tranform db path on host to path in docker container
    db_path <- file.path(sub(host_path, '', episode_root), 'meta.db')
    
    # creat cooc net structure
    net_cooc <- netCoocCreate(db_path, sql_actor_play, thr)
    printlog('net_cooc created')
    
    # get cooc net attribute of cooc net
    attr_cooc <- netCoocAttrGet(net_cooc)
    printlog(
      sprintf(
        'net_cooc attr get, categories: %s, nodes: %s, links: %s',
        length(attr_cooc$categories), 
        nrow(attr_cooc$nodes), 
        length(attr_cooc$links)
      )
    )
    
    # get cooc net centr
    net_centr <- netCoocCentrGet(net_cooc)
    printlog(
      sprintf(
        'net centr get, graph level: %s, nodes level: %s', 
        length(net_centr$graph_centr), nrow(net_centr$node_centr)
      )
    )
    
    # get ego net attribute
    ego_net <- egoNetAttrGet(net_cooc)
    printlog(sprintf('ego net get: %s', length(ego_net)))
    
    # create sent net structure
    nodes_name <- V(net_cooc)$name
    net_sent <- netSentCreate(db_path, sql_talk, sql_seg_word, 
                              nodes_name, .termVec, 
                              .termVecSentMtx, .termVecSentDT)
    printlog('net_sent created')
    
    # get sent net attribute of sent net
    attr_sent <- netSentAttrGet(net_sent)
    printlog(
      sprintf(
        'net_sent attr get, categories: %s, nodes: %s, links: %s',
        length(attr_sent$categories), 
        nrow(attr_sent$nodes), 
        length(attr_sent$links)
      )
    )
    
    jsonlite::toJSON(list(code = 0, 
                          msg = '', 
                          categories_cooc = attr_cooc$categories, 
                          nodes_cooc = attr_cooc$nodes, 
                          links_cooc = attr_cooc$links, 
                          
                          categories_sent = attr_sent$categories, 
                          nodes_sent = attr_sent$nodes, 
                          links_sent = attr_sent$links, 
                          
                          ego_net = ego_net, 
                          
                          graph_centr = net_centr$graph_centr, 
                          node_centr = net_centr$node_centr), 
                     auto_unbox = TRUE)
  }, 
  error = function(e) {
    printlog(sprintf('error: %s', e$message))
    jsonlite::toJSON(list(code = -1, msg = e$message), 
                     auto_unbox = TRUE)
  })
}
