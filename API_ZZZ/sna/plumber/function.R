
suppressPackageStartupMessages({
  library(igraph)
  library(dplyr)
  library(tibble)
  library(purrr)
})

# get specific level grpah
graphSelect <- function(level, id, thr = 0.9) {

  if (level == 'all') {
    g <- graphCollege
    g <- delete_edges(g, which(E(g)$weight < quantile(E(g)$weight, thr)))
    # g <- subgraph.edges(g, E(g)[E(g)$weight >= quantile(E(g)$weight, thr)])
    return(list(
      g = g,
      node_label = college[college$id %in% V(g)$name, ]
    ))

  } else if (level == 'college') {
    # id is college id
    if (!id %in% major$college_id) {
      stop(sprintf('the college %s hasn\'t a social network!', id))
    }
    vids <- as.character(major$id[major$college_id == id])
    vids <- intersect(vids, V(graphMajor)$name)
    g <- induced_subgraph(graphMajor, vids)
    g <- delete_edges(g, which(E(g)$weight < quantile(E(g)$weight, thr)))
    # g <- subgraph.edges(g, E(g)[E(g)$weight >= quantile(E(g)$weight, thr)])
    return(list(
      g = g,
      node_label = major[major$id %in% V(g)$name, c('id', 'name')]
    ))

  } else if (level == 'major') {
    # id is major id
    if (!id %in% user_major$major_id) {
      stop(sprintf('the major %s hasn\'t a social network!', id))
    }
    vids <- as.character(user_major$id[user_major$major_id == id])
    vids <- intersect(vids, V(graphUser)$name)
    g <- induced_subgraph(graphUser, vids)
    g <- delete_edges(g, which(E(g)$weight < quantile(E(g)$weight, thr)))
    # g <- subgraph.edges(g, E(g)[E(g)$weight >= quantile(E(g)$weight, thr)])
    return(list(
      g = g,
      node_label = user_major[user_major$id %in% V(g)$name, c('id', 'name')]
    ))

  } else {
    # id is user id aka accnum
    if (!id %in% V(graphUser)$name) {
      stop(sprintf('the user %s hasn\'t a ego social network!', id))
    }
    vids <- c(id, neighbors(graphUser, id, 'all')$name)
    g <- induced_subgraph(graphUser, vids)
    deg_ego <- degree(g)
    g <- induced_subgraph(g, names(deg_ego[deg_ego >= quantile(deg_ego, thr)]))
    return(list(
      g = g,
      node_label = user_major[user_major$id %in% V(g)$name, c('id', 'name')]
    ))
  }
}

# get attributes of graph level
graphAttrGet <- possibly(function(g) {

  safeJSON(list(
    g_deg = tryCatch(
      centr_degree(g, mode = 'all', normalized = TRUE)$centralization,
      error = function(e)NA
    ),
    g_clo = tryCatch(
      suppressWarnings(
        abs(centr_clo(g, mode = 'all', normalized = TRUE)$centralization)
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
  ))
}, otherwise = list())

graphCollegeAvgCentr <- function() {
  as.list(colMeans(as.matrix(
    allGraphCollegeCentr[, -6]
  )))
}

graphMajorAvgCentr <- function() {
  as.list(colMeans(as.matrix(
    allGraphMajorCentr[, -6]
  )))
}

# util function for nodeAttrGet
topSlice <- function(df, n) {
  slice(arrange(enframe(df, 'id'), desc(value)), seq_len(n))
}

# util function for nodeAttrGet
nodeId2Label <- function(node_attr, name_df) {
  lapply(node_attr, purrr::possibly(function(x) {
    name_df$id <- as.character(name_df$id)
    dplyr::left_join(x, name_df, by = 'id')
  }, otherwise = tibble()))
}

# get attributes of nodes level
nodeAttrGet <- possibly(function(g, name_df, top = 10) {

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

  temp <- list(
    degree = topSlice(n_deg, top),
    closeness = topSlice(n_clo / safeMax(n_clo, na.rm = TRUE), top),
    betweenness = topSlice(n_btw / safeMax(n_btw, na.rm = TRUE), top),
    eigenvector = topSlice(eg / safeMax(eg, na.rm = TRUE), top)
  )
  
  nodeId2Label(temp, name_df)
  
}, otherwise = list())

edgeListGet <- possibly(function(g) {

  edgeList <- get.edgelist(g)

  dplyr::tibble(
    source = edgeList[, 1],
    target = edgeList[, 2],
    width = E(g)$weight / safeMax(E(g)$weight, na.rm = TRUE) * 10
  )

}, otherwise = tibble())

# a safer json transform function, it will convert NA, NULL, NaN, Inf to NA
# this is for API response
safeJSON <- function(lst) {
  purrr::modify_if(lst, ~ !is.finite(.) || is.null(.), ~ NA)
}

safeMax <- function(vec, na.rm = FALSE) {
  if (length(vec) > 0 && all(is.finite(vec))) {
    max(vec, na.rm = na.rm)
  } else {
    switch(typeof(vec),
           'double' = numeric(0),
           'integer' = integer(0),
           'character' = character(0),
           'logical' = logical(0),
           'NULL' = numeric(0))
  }
}
