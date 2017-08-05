
source('dataSource.R', encoding = 'utf-8')
source('sna.R', encoding = 'utf-8')

# SNA in all episode and scene
people %>% 
  # filter(scene >= 1 & scene <= 100) %>% # SNA in specific episode or scene
  netCreate(plot_name = 'people/final_') %>% 
  netAttrGet()
# netClusterPlot(net)

# people-scene SNA in all episode and scene
script %>% 
  filter(nchar(people) > 0 & 
           people %in% names(head(sort(table(script$people), TRUE), 20)) & 
           loc %in% names(head(sort(table(script$loc), TRUE), 20))) %>% 
  tidyr::unite(pl, people, loc) %>%
  # filter(episode >= 13 & episode <= 16) %>% # SNA in specific episode or scene
  # group_by(people, loc) %>% 
  # tally() %>% 
  # tidyr::spread(loc, n, fill = 0) # for cross-table, nice but useless
  netCreate('pl', 'episode', thr = 0.95, plot_name = 'pl/pl_')

# all scene which specific people show, SNA
script %>% 
  filter(people == 'Peter') %>% 
  netCreate('loc', 'episode', plot_name = 'Peter_')

# sentiment analysis

{
  script_term <- left_join(script_term, sentiment) %>% 
    mutate(new_id = id %/% 100 + 1)
  script_term
  } %>% {
    script_sen <- group_by(., new_id, sentiment0) %>% 
      tally()
      # summarise(sen = sum(polarity, na.rm = TRUE))
    script_sen
  } %>% 
  filter(!is.na(sentiment0)) %>% 
  ggplot(aes(new_id, n)) + 
  geom_line(aes(group = sentiment0, 
                col = sentiment0), 
            size = 1)

idf_path <- 'dict/test_idf.utf8'
segment(script$text, seg) %>% 
  get_idf(stop_word = 'dict/stop_words.utf8', 
          path = idf_path)
key_user <- worker('keywords', 
                   stop_word = 'dict/stop_words.utf8', 
                   idf = idf_path, 
                   topn = 40)
which.min(script_sen$sen) %>% 
  {filter(script_term, new_id == .)} %>% 
  pull(id) %>% 
  unique() %>% 
  {filter(script, id %in% . &
            (is_dialogue | is_scene))} %>% 
  pull(text) %>% 
  paste0(collapse = '') %>% 
  keywords(key_user) %>% 
  enframe() %>% 
  filter(!value %in% script$people) %>% 
  top_n(20, name)
