
iter <- unique(nodes$ACCDEPID)


# get graph without 100058 businessnum --------------------------------------

edgeList <- read_rds('rds/edgeList.rds') %>% 
  filter(weight >= 2)
graphCampus <- graph_from_data_frame(edgeList, directed = FALSE, vertices = nodes)
graphCampus <- igraph::simplify(graphCampus, edge.attr.comb = list(weight = 'sum'))
pb <- progress_estimated(length(iter))
graph_centr_device <- map_dfr(iter, ~ {
  res <- graphGet(.x, .graphCampus = graphCampus) %>% 
    .$graph_centr %>% 
    as_tibble() %>% 
    mutate(dep_id = .x)
  pb$tick()$print()
  res
}) %>% 
  gather(key = 'centr', value = 'value', -dep_id) %>% 
  mutate(option = 'device')


# get graph with 100058 businessnum --------------------------------------

edgeList_test <- read_rds('rds/edgeList_test.rds') %>% 
  filter(weight >= 2)
graphCampus_all <- graph_from_data_frame(edgeList_test, directed = FALSE, vertices = nodes)
graphCampus_all <- igraph::simplify(graphCampus_all, edge.attr.comb = list(weight = 'sum'))
pb <- progress_estimated(length(iter))
graph_centr_business <- map_dfr(iter, ~ {
  res <- graphGet(.x, .graphCampus = graphCampus_all) %>% 
    .$graph_centr %>% 
    as_tibble() %>% 
    mutate(dep_id = .x)
  pb$tick()$print()
  res
}) %>% 
  gather(key = 'centr', value = 'value', -dep_id) %>% 
  mutate(option = 'business')


# compare centralization between two graphs ------------------------------

ggthemr::ggthemr('flat')

bind_rows(graph_centr_device, graph_centr_business) %>% 
  ggplot(aes(x = value, group = option)) + 
  geom_histogram(aes(y = ..density.., color = option), alpha = 0.3) +
  geom_density(aes(color = option), alpha = 0.7, size = 1) +
  facet_wrap(~ centr, scales = 'free')
  
bind_rows(graph_centr_device, graph_centr_business) %>% 
  group_by(option, centr) %>% 
  summarise(temp = mean(value, na.rm = TRUE)) %>% 
  spread(centr, temp)

#   option       g_btw  g_clo  g_deg g_density  g_eg
#   <chr>        <dbl>  <dbl>  <dbl>     <dbl> <dbl>
# 1 no_100058   0.0417 0.0158 0.0665    0.0251 0.740
# 2 with_100058 0.0560 0.0215 0.0950    0.0402 0.721