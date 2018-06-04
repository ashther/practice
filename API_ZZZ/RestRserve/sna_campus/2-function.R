
# get property of node, edge and graph -----------------------------------

graphCentrGet <- function(g) {
  
  list(
    g_deg = centr_degree(g, mode = 'all', normalized = TRUE)$centralization, 
    g_clo = suppressWarnings(
      centr_clo(g, mode = 'all', normalized = TRUE)$centralization
    ), 
    g_btw = centr_betw(g, directed = FALSE, normalized = TRUE)$centralization, 
    g_eg = centr_eigen(g, normalized = TRUE)$centralization, 
    g_density = edge_density(g, loops = TRUE)
  )
  
}

nodeCentrAdd <- function(g) {
  
  n_deg <- degree(g, mode = 'all', normalized = FALSE)
  n_clo <- suppressWarnings(
    closeness(g, mode = 'all', weights = 1/E(g)$weight, normalized = FALSE)
  )
  n_btw <- betweenness(g, directed = FALSE, weights = 1/E(g)$weight, normalized = FALSE) 
  eg <- evcent(g, directed = FALSE, weights = NA)$vector
  
  cluster <- communities(cluster_fast_greedy(g, weights = 1/E(g)$weight))
  cluster <- tibble(name = rep(names(cluster), sapply(cluster, length)), 
                    value = unname(unlist(cluster)))
  
  V(g)$degree <- n_deg
  V(g)$closeness <- n_clo / max(n_clo)
  V(g)$betweenness <- n_btw / max(n_btw)
  V(g)$eigenvector <- eg / max(eg)
  V(g)$cluster <- left_join(as_tibble(V(g)$name), cluster, by = 'value')$name
  
  g
}

# 386 42683
graphGet <- function(id, is_dep = TRUE, .graphCampus = graphCampus, .nodes = nodes) {
  
  library(igraph)
  library(dplyr)

# make the induced subgraph ----------------------------------------------

  if (is_dep) {
    vids <- as.character(.nodes$ACCNUM[.nodes$ACCDEPID == id])
  } else {
    vids <- c(neighbors(.graphCampus, as.character(id), 'all')$name, as.character(id))
  }
  
  g <- induced_subgraph(.graphCampus, vids = vids, impl = 'auto')
  
  graph_centr <- graphCentrGet(g)
  # print(dplyr::as_tibble(graph_centr))
  
  g <- nodeCentrAdd(g)
  # node_attr <- vertex_attr(g)
  list(g = g, graph_centr = graph_centr)
}


graphPlot <- function(g, node_color_var, node_size_var, filename, thr = 0) {
  
  library(ggraph)
  
  g <- induced_subgraph(g, V(g)[degree > thr])
  
  {
    ggraph(g, layout = 'fr') + 
      geom_edge_link(aes(edge_width = weight),
                     alpha = 0.2, 
                     show.legend = FALSE) +
      geom_node_point(aes(color = eval(parse(text = node_color_var)), 
                          size = eval(parse(text = node_size_var))),
                      show.legend = TRUE) + 
      geom_node_text(aes(label = sprintf('%s\n%s', name, ACCDEPNAME)), 
                     size = 2, 
                     repel = TRUE) + 
      guides(color = guide_legend(title = node_color_var), 
             size = guide_legend(title = node_size_var)) + 
      theme_void()
  } %>% 
    ggsave(filename = paste0('./plot/', filename, '.png'), plot = ., 
           width = 7.35, height = 7.35)
  
}

attrToJson <- function(g) {
  
  nodes <- as_tibble(vertex_attr(g)) %>% 
    select(ACCNUM, ACCNAME, ACCDEPNAME, sex, JOINDATE)
  
  links <- as_tibble(edge_attr(g))
  links <- cbind(target = get.edgelist(g)[, 2], links)
  links <- cbind(source = get.edgelist(g)[, 1], links)
  links$source <- as.character(links$source)
  links$target <- as.character(links$target)
  
  links <- lapply(seq_len(nrow(links)), function(x) {
    list(
      source = links$source[x], 
      target = links$target[x], 
      lineStyle = list(normal = list(
        width = links$weight[x] / max(links$weight)
      ))
    )
  })
  
  return(list(nodes = nodes, links = links))
}