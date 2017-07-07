
library(janeaustenr)
library(tidytext)
library(ggraph)
library(igraph)
library(widyr)
library(dplyr)

austen_section_words <- austen_books() %>% 
  filter(book == 'Sense & Sensibility') %>%
  mutate(section = row_number() %/% 20) %>% 
  filter(section > 0) %>% 
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  select(-book) %>% 
  filter(word %in% tolower(c('Henry', 'Elinor', 'Marianne', 'Margaret', 
                             'John', 'Fanny', 'Middleton', 'Edward', 
                             'Colonel', 'Willoughby', 'Lucy')))
people_cor <- austen_section_words %>% 
  pairwise_cor(word, section, sort = TRUE)
people_count <- austen_section_words %>% 
  pairwise_count(word, section, sort = TRUE)

thr_cor <- quantile(people_cor$correlation, 0.7)
thr_count <- quantile(people_count$n, 0.7)

people_cor %>% 
  filter(correlation > thr_cor) %>% 
  graph_from_data_frame() %>% {
    people_cor_net <<- .
    mbs <- cluster_edge_betweenness(people_cor_net)$membership
    deg <- degree(people_cor_net, mode = 'all')
    ggraph(graph = people_cor_net, layout = 'auto') + 
      geom_edge_link(aes(edge_alpha = correlation, 
                         edge_width = correlation), 
                     edge_colour = 'cyan4', 
                     show.legend = FALSE) + 
      geom_node_point(aes(color = as.factor(mbs)), 
                      size = 5, 
                      show.legend = FALSE) + 
      geom_node_text(aes(label = name), repel = TRUE, 
                     point.padding = unit(0.2, 'lines')) + 
      theme_void()
  } %>% 
  ggsave(filename = paste0('./plot/cor_', 
                           strftime(Sys.time(), format = '%Y%m%d-%H%M%S'), 
                           '.png'), 
         plot = .)

people_count %>% 
  filter(n > thr_count) %>% 
  graph_from_data_frame() %>% {
    people_count_net <<- .
    mbs <- cluster_edge_betweenness(people_count_net)$membership
    deg <- degree(people_count_net, mode = 'all')
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
  ggsave(filename = paste0('./plot/count_', 
                           strftime(Sys.time(), format = '%Y%m%d-%H%M%S'), 
                           '.png'), 
         plot = .)

netAttrGet <- function(net) {
  deg <- sort(degree(net, loops = FALSE), decreasing = TRUE)
  clo <- sort(round(closeness(net, mode = 'all'), 2), decreasing = TRUE)
  btw <- sort(round(betweenness(net, directed = FALSE), 2), decreasing = TRUE)
  cat(
    '\n', 
    sprintf(
      'edge density: %s', edge_density(net, loops = FALSE)
    ), '\n', 
    sprintf(
      'diameter: %s', diameter(net, directed = FALSE)
    ), '\n', 
    sprintf(
      'degree: %s', paste(names(deg), deg, sep = ':', collapse = ' ')
    ), '\n', 
    sprintf(
      'closeness: %s', paste(names(clo), clo, sep = ':', collapse = ' ')
    ), '\n', 
    sprintf(
      'betweenness: %s', paste(names(btw), btw, sep = ':', collapse = ' ')
    ), '\n'
  )
}

netClusterPlot <- function(net) {
  ceb <- cluster_edge_betweenness(net)
  clp <- cluster_label_prop(net)
  cfg <- cluster_fast_greedy(as.undirected(net))
  
  par(mfrow = c(1, 3))
  plot(ceb, net, edge.arrow.mode = 0, vertex.label.dist = 1, main = 'ceb')
  plot(clp, net, edge.arrow.mode = 0, vertex.label.dist = 1, main = 'clp')
  plot(cfg, net, edge.arrow.mode = 0, vertex.label.dist = 1, main = 'cfg')
}
