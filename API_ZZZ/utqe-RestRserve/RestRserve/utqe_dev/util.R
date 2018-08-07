

library(stringr)
library(dplyr)
library(RMySQL)

# create endpoint_table_params.R -----------------------------------------

purrr::map_chr(dbListTables(pool), function(x) {
  temp <- dbGetQuery(pool, sprintf('show fields from %s', x)) %>% 
    filter(!str_detect(Field, '[\u4e00-\u9fa5]')) %>% 
    mutate(typeof = case_when(
      str_detect(Type, 'varchar|text') ~ 'as.character', 
      str_detect(Type, 'int|tinyint|decimal') ~ 'as.integer'
    ))
  sprintf('%s = \'%s\'', temp$Field, temp$typeof) %>% 
    paste0(collapse = ',\n\t\t') %>% 
    sprintf(glue::glue(
      'list(
         req_params = c(pageIndex = \'as.integer\', 
                           pageSize = \'as.integer\'), 
         opt_params = c(%s), 
         table_name = \'%s\'
      )'
    ), ., x) %>% 
    sprintf('\'/%s\' = %s', gsub('_', '/', x), .)
}) %>% 
  paste0(collapse = ',\n') %>% 
  sprintf('endpoint_table_params <- list(%s)', .) %>% 
  cat(file = 'endpoint_table_params.R')

