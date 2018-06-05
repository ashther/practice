

# preparation ------------------------------------------------------------

HOME_PATH <- '/home/ashther/sna'
if (!exists('graphCampus')) {
  graphCampus <- readRDS(file.path(HOME_PATH, 'rds/graphCampus.rds'))
}
if (!exists('nodes')) {
  nodes <- readRDS(file.path(HOME_PATH, 'rds/nodes.rds'))
}


# function ---------------------------------------------------------------

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
    error = function(e)rep(NA, vcount(g))
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
  V(g)$closeness <- n_clo / max(n_clo) # 1/0 == Inf !!
  V(g)$betweenness <- n_btw / max(n_btw)
  V(g)$eigenvector <- eg / max(eg)
  # V(g)$cluster <- left_join(as_tibble(V(g)$name), cluster, by = 'value')$name
  
  g
}

graphGet <- function(id, is_dep = TRUE, loglevel = 0, home_path = HOME_PATH,  
                     .graphCampus = graphCampus, .nodes = nodes) {
  tryCatch({
    suppressPackageStartupMessages({
      library(igraph)
      library(luzlogr)
    })
    
    if (loglevel > 0) {
      openlog(file.path(home_path, 'graphGet.log'), append = TRUE)
      on.exit(closelog(sessionInfo = FALSE), add = TRUE)
    }
    
    if ((is_dep & !id %in% .nodes$ACCDEPID) | 
        (!is_dep & !id %in% .nodes$ACCNUM)) {
      return(list(
        error_msg = 'Parameter id must be one of ACCDEPID if is_dep is TRUE, or ACCNUM if is_dep is FALSE', 
        error_code = 40003
      ))
    }
    
    # make the induced subgraph ----------------------------------------------
    if (is_dep) {
      vids <- as.character(.nodes$ACCNUM[.nodes$ACCDEPID == id])
    } else {
      vids <- c(neighbors(.graphCampus, as.character(id), 'all')$name, as.character(id))
    }
    if (length(vids) <= 1) {
      return(list(
        error_msg = 'Induced subgraph is empty, it only has 0 or 1 node', 
        error_code = 40004
      ))
    }
    suppressWarnings(printlog(sprintf('vids used for induced subgraph: %s', length(vids))))
    
    g <- induced_subgraph(.graphCampus, vids = vids, impl = 'auto')
    suppressWarnings(printlog(sprintf('graph get: %s -- %s', gorder(g), gsize(g))))
    
    graph_centr <- graphCentrGet(g)
    suppressWarnings(printlog(sprintf('graph level centralization caculated')))
    
    g <- nodeCentrAdd(g)
    suppressWarnings(printlog(sprintf('node level attributes caculated')))
    
    list(graph_centr = graph_centr, 
         node_attr = dplyr::as_tibble(vertex_attr(g)), 
         edge_attr = cbind(setNames(dplyr::as_tibble(get.edgelist(g)), 
                                    c('source', 'target')), 
                           dplyr::as_tibble(edge_attr(g))))
    
  }, error = function(e) {
    suppressWarnings(printlog(sprintf('error: %s', e$message)))
    list(error_msg = e$message, error_code = 40099)
  })
}

graphAvgCentrRead <- function(home_path = HOME_PATH) {
  
  tryCatch({
    
    # suppressPackageStartupMessages({
    #   library(luzlogr)
    # })
    # openlog(file.path(home_path, 'graphAvgCentrRead.log'), append = TRUE)
    # on.exit(closelog(sessionInfo = FALSE), add = TRUE)
    
    graph_avg_centr <- readRDS(file.path(home_path, 'rds/graphAvgCentr.rds'))
    as.list(graph_avg_centr)
    
  }, error = function(e) {
    # printlog(sprintf('error: %s', e$message))
    list(error_msg = e$message, error_code = 40099)
  })
}

# api filter -------------------------------------------------------------

graphAvgCentrReadFilter <- function(request, response) {
  
  #' ---
  #' summary: 提取所有子图平均指标API
  #' description: 该API用于提取graphCampus的所有子图的图级别指标平均值，该平均值通过离线计算完成
  #' 
  #' responses:
  #'   200:
  #'     description: 正常返回
  #'     content:
  #'       application/json:
  #'   400:
  #'     description: 入参错误
  #'     content:
  #'       application/json:
  #'         schema:
  #'           properties:
  #'             error_msg:
  #'               type: string
  #'             error_code:
  #'               type: string
  #'   500:
  #'     description: 内部计算错误
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           properties:
  #'             error_msg:
  #'               type: string
  #'             error_code:
  #'               type: string
  #' ---
  
  library(RestRserve)
  library(jsonlite)
  
  result <- graphAvgCentrRead()
  if ('error_code' %in% names(result)) {
    return(RestRserveResponse$new(
      body = toJSON(result, auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L
    ))
  } else {
    return(RestRserveResponse$new(
      body = toJSON(result, auto_unbox = TRUE, na = 'null', null = 'null'), 
      content_type = 'application/json', 
      status_code = 200L
    ))
  }
}

graphGetFilter <- function(request, response) {
  
  #' ---
  #' summary: 社交网络分析API
  #' description: 该API用于基于一卡通系统数据的校园社交网络分析(social network analysis)，利用了一卡通交易流水数据构建了社交网络
  #' parameters:
  #'   - name: "id"
  #'     description: 账户部门序号（MC_Transaction中的ACCDEPID）或者个人账号（SC_AccDep中的ACCNUM）
  #'     in: query
  #'     schema:
  #'       type: integer
  #'     example: 386
  #'     required: true
  #'   - name: "is_dep"
  #'     description: 入参id是否为账户部门序号
  #'     in: query
  #'     schema:
  #'       type: string
  #'       enum: 
  #'         - TRUE
  #'         - FALSE
  #'     example: "TRUE"
  #'     required: true
  #'   - name: "loglevel"
  #'     description: 是否打印日志，供调试使用
  #'     in: query
  #'     schema:
  #'       type: integer
  #'       enum: 
  #'         - 0
  #'         - 1
  #'     example: 0
  #'     required: false
  #' responses:
  #'   200:
  #'     description: 正常返回
  #'     content:
  #'       application/json:
  #'   400:
  #'     description: 入参错误
  #'     content:
  #'       application/json:
  #'         schema:
  #'           properties:
  #'             error_msg:
  #'               type: string
  #'             error_code:
  #'               type: string
  #'   500:
  #'     description: 内部计算错误
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           properties:
  #'             error_msg:
  #'               type: string
  #'             error_code:
  #'               type: string
  #' ---
  
  library(RestRserve)
  library(jsonlite)
  
  # check is_dep parameter if be provided or valid
  if (!'is_dep' %in% names(request$query)) {
    return(RestRserveResponse$new(
      body = toJSON(list(error_msg = 'Parameter is_dep is required.', 
                         error_code = 40001), 
                    auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L
    ))
  } else {
    is_dep <- as.logical(request$query[['is_dep']])
    if (is.na(is_dep) | is.null(is_dep)) {
      return(RestRserveResponse$new(
        body = toJSON(list(error_msg = 'Parameter is_dep is invalid.', 
                           error_code = 40002), 
                      auto_unbox = TRUE), 
        content_type = 'application/json', 
        status_code = 400L
      ))
    }
  }
  
  # check id parameter if be provided or valid
  if (!'id' %in% names(request$query)) {
    return(RestRserveResponse$new(
      body = toJSON(list(error_msg = 'Parameter id is required.', 
                         error_code = 40001), 
                    auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L
    ))
  } else {
    id <- as.integer(request$query[['id']])
    if (is.na(id) | is.null(id)) {
      return(RestRserveResponse$new(
        body = toJSON(list(error_msg = 'Parameter id is invalid.', 
                           error_code = 40002), 
                      auto_unbox = TRUE), 
        content_type = 'application/json', 
        status_code = 400L
      ))
    }
  }
  
  # check loglevel parameter if be provided or valid
  if (!'loglevel' %in% names(request$query)) {
    loglevel <- 0
  } else {
    loglevel <- as.integer(request$query[['loglevel']])
    if (is.na(loglevel) | is.null(loglevel)) {
      return(RestRserveResponse$new(
        body = toJSON(list(error_msg = 'Parameter loglevel is invalid.', 
                           error_code = 40002), 
                      auto_unbox = TRUE), 
        content_type = 'application/json', 
        status_code = 400L
      ))
    }
  }
  
  result <- graphGet(id, is_dep, loglevel)
  if ('error_code' %in% names(result)) {
    return(RestRserveResponse$new(
      body = toJSON(result, auto_unbox = TRUE), 
      content_type = 'application/json', 
      status_code = 400L
    ))
  } else {
    return(RestRserveResponse$new(
      body = toJSON(result, auto_unbox = TRUE, na = 'null', null = 'null'), 
      content_type = 'application/json', 
      status_code = 200L
    ))
  }
  
}


# create application and register endpoints ------------------------------

RestRserveApp <- RestRserve::RestRserveApplication$new()
RestRserveApp$add_get(path = "/graph", FUN = graphGetFilter)
RestRserveApp$add_get(path = '/graph/avg_centr', FUN = graphAvgCentrReadFilter)
RestRserveApp$add_openapi(path = '/openapi.yaml', file_path = 'openapi.yaml')
RestRserveApp$add_swagger_ui(path = '/swagger', 
                             path_openapi = '/openapi.yaml', 
                             path_swagger_assets = '/__swagger__')

# RestRserveApp$run(http_port = "8000")
