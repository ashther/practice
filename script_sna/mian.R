
source('dataSource.R', encoding = 'utf-8')
source('sna.R')

net <- netCreate(people)
netAttrGet(net)
netClusterPlot(net)

script %>% 
  filter(nchar(people) > 0 & 
           people %in% names(head(sort(table(script$people), TRUE), 20)) & 
           loc %in% names(head(sort(table(script$loc), TRUE), 20))) %>% 
  tidyr::unite(pl, people, loc) %>%
  # filter(episode >= 13 & episode <= 16) %>% 
  netCreate('pl', 'episode', thr = 0.95, plot_name = 'pl/pl_')
  # group_by(people, loc) %>% 
  # tally() %>% 
  # tidyr::spread(loc, n, fill = 0)

script %>% 
  filter(people == 'Peter') %>% 
  netCreate('loc', 'episode', plot_name = 'Peter_')