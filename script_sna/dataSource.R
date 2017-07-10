
library(dplyr)
library(stringr)
library(tibble)
library(magrittr)

script <- readLines('data/data.txt') %>% 
  str_subset('[^^$]')
script <- data_frame(id = seq_along(script), text = script) %>% 
  mutate(scene = cumsum(str_detect(text, '^\\d+-\\d+\\t*\\s*\\t*景')), 
         is_scene = str_detect(text, '△'), 
         is_dialogue = str_detect(text, '：') & 
           !str_detect(text, '景：|时：|人：'), 
         is_intro = str_detect(text, '景：|时：|人：'), 
         is_title = str_detect(text, '^\\s*第\\s*\\w+\\s*集\\s*') & 
                                 !str_detect(text, '完'), 
         episode = cumsum(is_title)) %>% 
  mutate(people = if_else(is_dialogue, 
                          str_split(text, '[^[:alnum:]]', n = 2, simplify = TRUE)[, 1], 
                          ''))

idx <- which(with(script, is_scene + is_dialogue + is_intro + is_title) == 0)
while (length(idx) > 0) {
  script[idx, c('is_scene', 'is_dialogue', 'people', 'is_intro', 'is_title')] <- 
    script[idx - 1, c('is_scene', 'is_dialogue', 'people', 'is_intro', 'is_title')]
  idx <- which(with(script, is_scene + is_dialogue + is_intro + is_title) == 0)
}

script <- script %>% 
  filter(nchar(people) > 0) %>% 
  select(id, scene, episode, people)