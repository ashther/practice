
logTailGet <- function(log_path, tail_length = 1000) {
  # if (!R.utils::isAbsolutePath(log_path)) {
  #   log_path <- here::here(log_path)
  # }
  
  content <- readLines(log_path)
  content <- tail(content, tail_length)
  writeLines(log_path)
}


# offline build graph ----------------------------------------------------


# get data from oracle database
dataGetFromOracle <- function(sql, ...) {
  library(ROracle)
  
  con <- dbConnect(dbDriver("Oracle"), ...)
  on.exit(dbDisconnect(con))
  
  res <- dbSendQuery(con, sql)
  result <- dbFetch(res)
  dbClearResult(res)
  
  result
}

# get data from am_account, sc_accdep, sc_dict, and join into nodes table
nodesGet <- function(db_config) {
  sql_nodes <- paste0(
    'SELECT a.ACCNUM,
    a.ACCNAME,
    a.ACCDEPID,
    b.ACCDEPNAME,
    b.SHORTNAME,
    b.PARENTID,
    a.PERCODE,
    c.DICTNAME SEX,
    a.BIRTHDAY,
    a.IDTYPE,
    a.IDNO,
    a.JOINDATE
    FROM AM_Account a
    LEFT JOIN SC_AccDep b ON a.ACCDEPID = b.ACCDEPID
    LEFT JOIN SC_Dict c ON a.SEX = c.DICTNUM
    AND c.TYPENUM = 137'
  )
  
  nodes <- dataGetFromOracle(sql_nodes, 
                             username = db_config$username, 
                             password = db_config$password, 
                             dbname = db_config$dbname)
  printlog(sprintf('get nodes: %s', nrow(nodes)))
  
  mutate(
    nodes, 
    BIRTHDAY = as.Date(BIRTHDAY), 
    age = case_when(
      !is.na(BIRTHDAY) ~ as.numeric(difftime(Sys.time(), BIRTHDAY, units = 'days')/365), 
      is.na(BIRTHDAY) & (IDTYPE == 1) ~ as.numeric(difftime(
        Sys.time(), as.Date(str_sub(IDNO, 7, 14), format = '%Y%m%d'), units = 'days'
      )/365), 
      is.na(BIRTHDAY) & (IDTYPE != 1) ~ NA_real_, # NA_real_
      TRUE ~ NA_real_
    )
  )
}

# get data from mc_transaction
transactionGet <- function(db_config, dateRange) {
  sql_transaction <- sprintf(
    paste0(
      'SELECT accnum,
        devicenum,
        dealtime
        FROM MC_Transaction        
        WHERE recflag = 1
        AND devicenum <> 0
        AND isred = 0
        AND businessnum <> 100058
        AND dealtime >= sysdate - %s
        ORDER BY devicenum,
        dealtime'
    ), 
    dateRange
  )
  
  transaction <- dataGetFromOracle(sql_transaction, 
                                   username = db_config$username, 
                                   password = db_config$password, 
                                   dbname = db_config$dbname)
  printlog(sprintf('get transaction: %s', nrow(transaction)))
  
  transaction
}

# multipulate transaction, get edges
edgesGet <- function(transaction, dealtime_thr) {
  result <- transaction %>% 
    mutate(ACCNUM_LAG = lag(ACCNUM), 
           DEVICENUM_LAG = lag(DEVICENUM), 
           DEALTIME_LAG = lag(DEALTIME), 
           is_one = (ACCNUM != ACCNUM_LAG) & 
             (DEVICENUM_LAG == DEVICENUM) & 
             difftime(DEALTIME, DEALTIME_LAG, units = 'secs') <= dealtime_thr) %>% 
    filter(is_one) %>% 
    select(from = ACCNUM, to = ACCNUM_LAG) %>% 
    count(from, to) %>% 
    rename(weight = n)
  
  printlog(sprintf('edges get: %s', nrow(result)))
  result
}

graphAvgCentrGet <- function(.graphCampus = graphCampus, .nodes = nodes) {
  result <- map(unique(.nodes$ACCDEPID), ~ {
    vids <- as.character(.nodes$ACCNUM[.nodes$ACCDEPID == .x])
    g <- induced_subgraph(.graphCampus, vids = vids, impl = 'auto')
    unlist(graphCentrGet(g))
  })
  result <- reduce(result, bind_rows)
  result <- colMeans(result, na.rm = TRUE)
  saveRDS(result, 'rds/graphAvgCentr.rds')
}

# build the graph, save graph and nodes
graphBuild <- function(db_config_path, home_path, dateRange = 365, dealtime_thr = 60) {
  tryCatch({
    suppressPackageStartupMessages({
      library(igraph)
      library(dplyr)
      library(magrittr)
      library(luzlogr)
      # library(here)
    })
    
    openlog(file.path(home_path, 'graphBuild.log'), append = TRUE)
    on.exit(closelog(sessionInfo = FALSE), add = TRUE)
    # if (!R.utils::isAbsolutePath(db_config_path)) {
    #   db_config_path <- here(db_config_path)
    # }
    rds_path <- file.path(home_path, 'rds')
    if (!dir.exists(rds_path)) {
      dir.create(rds_path)
    }
    
    db_config <- jsonlite::fromJSON(db_config_path)
    
    nodes <- nodesGet(db_config)
    transaction <- transactionGet(db_config, dateRange)
    edgeList <- edgesGet(transaction, dealtime_thr)
    
    graphCampus <- graph_from_data_frame(edgeList, directed = FALSE, vertices = nodes)
    graphCampus <- igraph::simplify(graphCampus, edge.attr.comb = list(weight = 'sum'))
    printlog(sprintf('graph get: %s -- %s', gorder(graphCampus), gsize(graphCampus)))
    
    saveRDS(ndoes, file.path(rds_path, 'nodes.rds'))
    printlog(sprintf('nodes saved'))
    
    saveRDS(graphCampus, file.path(rds_path, 'graphCampus.rds'))
    printlog(sprintf('graph saved'))
    
  }, error = function(e) {
    printlog(sprintf('error: %s', e$message))
  })
}


# api --------------------------------------------------------------------

#* @apiTitle SNA
#* @apiDescription This API is for social network analysis in campus, based on the transaction data.

# calculate kinds of the graph level centralization
graphCentrGet <- function(g) {
  
  list(
    g_deg = tryCatch(
      centr_degree(g, mode = 'all', normalized = TRUE)$centralization, 
      error = function(e)NA
    ), 
    g_clo = tryCatch(
      suppressWarnings(
        centr_clo(g, mode = 'all', normalized = TRUE)$centralization
      ), error = function(e)NA
    ), 
    g_btw = tryCatch(
      centr_betw(g, directed = FALSE, normalized = TRUE)$centralization, 
      error = function(e)NA
    ), 
    g_eg = tryCatch(
      centr_eigen(g, normalized = TRUE)$centralization, 
      error = function(e)NA
    ), 
    g_density = tryCatch(
      edge_density(g, loops = TRUE), 
      error = function(e)NA
    )
  )
  
}

# calculate kinds of individual level attributes 
nodeCentrAdd <- function(g) {
  
  n_deg <- tryCatch(
    degree(g, mode = 'all', normalized = FALSE), 
    error = function(e)NA
  )
  n_clo <- tryCatch(
    suppressWarnings(
      closeness(g, mode = 'all', weights = 1/E(g)$weight, normalized = FALSE)
    ), error = function(e)NA
  )
  n_btw <- tryCatch(
    betweenness(g, directed = FALSE, weights = 1/E(g)$weight, normalized = FALSE), 
    error = function(e)NA
  )
  eg <- tryCatch(
    evcent(g, directed = FALSE, weights = NA)$vector, 
    error = function(e)NA
  )
  
  # cluster <- communities(cluster_fast_greedy(g, weights = 1/E(g)$weight))
  # cluster <- tibble(name = rep(names(cluster), sapply(cluster, length)), 
  #                   value = unname(unlist(cluster)))
  
  V(g)$degree <- n_deg
  V(g)$closeness <- n_clo / max(n_clo)
  V(g)$betweenness <- n_btw / max(n_btw)
  V(g)$eigenvector <- eg / max(eg)
  # V(g)$cluster <- left_join(as_tibble(V(g)$name), cluster, by = 'value')$name
  
  g
}


#* @get /graph
#* get a induced graph from the big graphCampus, subgraph or egograph. Besides id, home_path and is_dep, there are two arguments named .graphCampus and .nodes, which had default value: graphCampus and nodes in memory, don't change the arguments value, it's just for R function.
#* @param id:int The id of ACCDEPID in MC_Transaction and SC_AccDep tables
#* @param is_dep:bool Is the induced graph subgraph? it is default TRUE
#* @response 200 Return graph level centralization, nodes and edges attributes
graphGet <- function(id, is_dep = TRUE, home_path = '.',  
                     .graphCampus = graphCampus, .nodes = nodes) {
  tryCatch({
    suppressPackageStartupMessages({
      library(igraph)
      library(luzlogr)
      # library(here)
    })
    
    openlog(file.path(home_path, 'graphGet.log'), append = TRUE)
    on.exit(closelog(sessionInfo = FALSE), add = TRUE)
    # if (!R.utils::isAbsolutePath(.graphCampus)) {
    #   .graphCampus <- here(.graphCampus)
    # }
    # if (!R.utils::isAbsolutePath(.nodes)) {
    #   .nodes <- here(.nodes)
    # }
    
    is_dep <- as.logical(is_dep)
    id <- as.integer(id)
    if (is.na(is_dep)) {
      res$status <- 400
      res$body <- 'Parameter is_dep must be bool'
    }
    if (is.na(id)) {
      res$status <- 400
      res$body <- 'Paramter id must be integer'
    }
    if ((is_dep & !id %in% .nodes$ACCDEPID) | 
        (!is_dep & !id %in% .nodes$ACCNUM)) {
      res$status <- 400
      res$body <- 'Parameter id must be one of ACCDEPID if is_dep is TRUE, or ACCNUM if is_dep is FALSE'
    }
    
    # make the induced subgraph ----------------------------------------------
    if (is_dep) {
      vids <- as.character(.nodes$ACCNUM[.nodes$ACCDEPID == id])
    } else {
      vids <- c(neighbors(.graphCampus, as.character(id), 'all')$name, as.character(id))
    }
    if (length(vids) == 0) {
      stop('no vids for induced subgraph!')
    }
    printlog(sprintf('vids used for induced subgraph: %s', length(vids)))
    
    g <- induced_subgraph(.graphCampus, vids = vids, impl = 'auto')
    printlog(sprintf('graph get: %s -- %s', gorder(g), gsize(g)))
    
    graph_centr <- graphCentrGet(g)
    printlog(sprintf('graph level centralization caculated'))
    
    g <- nodeCentrAdd(g)
    printlog(sprintf('node level attributes caculated'))
    
    list(# g = g, 
         graph_centr = graph_centr, 
         node_attr = dplyr::as_tibble(vertex_attr(g)), 
         edge_attr = cbind(setNames(dplyr::as_tibble(get.edgelist(g)), 
                                    c('source', 'target')), 
                           dplyr::as_tibble(edge_attr(g))))
    
  }, error = function(e) {
    printlog(sprintf('error: %s', e$message))
  })
}
