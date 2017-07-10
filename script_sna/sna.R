
# library(janeaustenr)
# library(tidytext)
source('dataSource.R')

netCreate <- function(script, item = 'people', feature = 'scene',  
                      thr = 0.75, plot = TRUE, plot_name = 'count_') {
  require(widyr)
  require(dplyr)
  require(ggraph)
  require(igraph)
  
  people_count_net <- script %>% 
    pairwise_count_(item, feature, sort = TRUE) %>% 
    filter(n > quantile(pull(., n), thr)) %>% 
    graph_from_data_frame()
  
  if (plot) {
    mbs <- cluster_fast_greedy(as.undirected(people_count_net))$membership
    deg <- degree(people_count_net, mode = 'all')
    {
      ggraph(graph = people_count_net, layout = 'fr') + 
        geom_edge_link(aes(edge_alpha = n, 
                           edge_width = n), 
                       edge_colour = 'darkred', 
                       show.legend = FALSE) + 
        geom_node_point(aes(size = deg, 
                            color = as.factor(mbs)), 
                        show.legend = FALSE) + 
        geom_node_text(aes(label = name), repel = TRUE, 
                       point.padding = unit(0.2, 'lines')) + 
        theme_void()
    } %>% 
      ggsave(filename = paste0('./plot/',
                               plot_name, 
                               strftime(Sys.time(), format = '%Y%m%d-%H%M%S'), 
                               '.png'), 
             plot = .)
  }
  return(people_count_net)
}

netAttrGet <- function(net) {
  require(tibble)
  require(igraph)
  
  deg <- centr_degree(net)
  clo <- centr_clo(net, mode = 'all')
  btw <- centr_betw(net)
  pg <- page.rank(net)$vector
  cat(
    '\n', 
    sprintf(
      'edge density: %s', round(edge_density(net, loops = TRUE), 3)
    ), '\n', 
    sprintf(
      'diameter: %s', diameter(net, directed = FALSE)
    ), '\n', 
    sprintf(
      'degree centralization: %s', round(deg$centralization, 3)
    ), '\n', 
    sprintf(
      'closeness centralization: %s', round(clo$centralization, 3)
    ), '\n', 
    sprintf(
      'betweenness centralization: %s', round(btw$centralization, 3)
    ), '\n\n'
  )
  
  data_frame(name = V(net)$name, 
             degree = deg$res, 
             closeness = round(clo$res, 3), 
             betweenness = round(btw$res, 3), 
             page.rank = round(pg, 3)) %>% 
    arrange(desc(page.rank)) %>% 
    print()
}

netClusterPlot <- function(net) {
  require(igraph)
  
  ceb <- cluster_edge_betweenness(net)
  clp <- cluster_label_prop(net)
  cfg <- cluster_fast_greedy(as.undirected(net))
  wt <- cluster_walktrap(net)
  
  old.par.mfrow <- par()$mfrow
  on.exit(par(mfrow = old.par.mfrow))
  
  png(filename = paste0('./plot/clutser_', 
                        strftime(Sys.time(), format = '%Y%m%d-%H%M%S'), 
                        '.png'), 
      width = 1500, height = 1500)
  par(mfrow = c(2, 2))
  plot(ceb, net, 
       edge.arrow.mode = 0, 
       vertex.label.dist = 1, 
       #main = paste0('ceb: ', round(modularity(ceb), 3)), 
       layout = layout_with_fr)
  title(paste0('ceb: ', round(modularity(ceb), 3)), cex.main = 3)
  plot(clp, net, 
       edge.arrow.mode = 0, 
       vertex.label.dist = 1, 
       # main = paste0('clp: ', round(modularity(clp), 3)), 
       layout = layout_with_fr)
  title(paste0('clp: ', round(modularity(clp), 3)), cex.main = 3)
  plot(cfg, net, 
       edge.arrow.mode = 0, 
       vertex.label.dist = 1, 
       # main = paste0('cfg: ', round(modularity(cfg), 3)), 
       layout = layout_with_fr)
  title(paste0('cfg: ', round(modularity(cfg), 3)), cex.main = 3)
  plot(wt, net, 
       edge.arrow.mode = 0, 
       vertex.label.dist = 1, 
       # main = paste0('wt: ', round(modularity(wt), 3)), 
       layout = layout_with_fr)
  title(paste0('wt: ', round(modularity(wt), 3)), cex.main = 3)
  on.exit(dev.off())
}

# austen_section_words <- austen_books() %>% 
#   filter(book == 'Sense & Sensibility') %>%
#   mutate(section = row_number() %/% 20) %>% 
#   filter(section > 0) %>% 
#   unnest_tokens(word, text) %>%
#   filter(!word %in% stop_words$word) %>%
#   select(-book) %>% 
#   filter(word %in% tolower(c('Henry', 'Elinor', 'Marianne', 'Margaret', 
#                              'John', 'Fanny', 'Middleton', 'Edward', 
#                              'Colonel', 'Willoughby', 'Lucy')))
# people_cor <- austen_section_words %>% 
#   pairwise_cor(word, section, sort = TRUE)
# people_count <- austen_section_words %>% 
#   pairwise_count(word, section, sort = TRUE)

# people_cor <- script %>% 
#   pairwise_cor(people, scene, sort = TRUE)
# people_count <- script %>% 
#   pairwise_count(people, scene, sort = TRUE)

# thr_cor <- quantile(people_cor$correlation, 0.8)
# thr_count <- quantile(people_count$n, 0.75)

# people_cor %>% 
#   filter(correlation > thr_cor & 
#            item1 %in% V(people_count_net)$name & 
#            item2 %in% V(people_count_net)$name) %>% 
#   graph_from_data_frame() %>% {
#     people_cor_net <<- .
#     # E(people_cor_net)$weight <<- E(people_cor_net)$correlation
#     mbs <- cluster_edge_betweenness(people_cor_net)$membership
#     deg <- degree(people_cor_net, mode = 'all')
#     ggraph(graph = people_cor_net, layout = 'auto') + 
#       geom_edge_link(aes(edge_alpha = correlation, 
#                          edge_width = correlation), 
#                      edge_colour = 'cyan4', 
#                      show.legend = FALSE) + 
#       geom_node_point(aes(color = as.factor(mbs)), 
#                       size = 5, 
#                       show.legend = FALSE) + 
#       geom_node_text(aes(label = name), repel = TRUE, 
#                      point.padding = unit(0.2, 'lines')) + 
#       theme_void()
#   } %>% 
#   ggsave(filename = paste0('./plot/cor_', 
#                            strftime(Sys.time(), format = '%Y%m%d-%H%M%S'), 
#                            '.png'), 
#          plot = .)

# people_count %>% 
#   filter(n > thr_count) %>% 
#   graph_from_data_frame() %>% {
#     people_count_net <<- .
#     # E(people_count_net)$weight <<- E(people_count_net)$n
#     mbs <- cluster_fast_greedy(as.undirected(people_count_net))$membership
#     deg <- degree(people_count_net, mode = 'all')
#     ggraph(graph = people_count_net, layout = 'fr') + 
#       geom_edge_link(aes(edge_alpha = n, 
#                          edge_width = n), 
#                      edge_colour = 'darkred', 
#                      show.legend = FALSE) + 
#       geom_node_point(aes(size = deg, 
#                           color = as.factor(mbs)), 
#                       show.legend = FALSE) + 
#       geom_node_text(aes(label = name), repel = TRUE, 
#                      point.padding = unit(0.2, 'lines')) + 
#       theme_void()
#   } %>% 
#   ggsave(filename = paste0('./plot/count_', 
#                            strftime(Sys.time(), format = '%Y%m%d-%H%M%S'), 
#                            '.png'), 
#          plot = .)

