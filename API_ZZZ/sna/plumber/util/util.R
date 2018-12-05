
library(RSQLite)
library(tidyverse)

con <- dbConnect(SQLite(), 'db.sqlite')

dict <- dbReadTable(con, 'dict') %>%
  filter(type_code == 'major')
major_list <- split(dict$item_name, dict$remark)

account <- dbReadTable(con, 'account')
user_list <- split(account$accnum, account$major)
dbDisconnect(con)

allGraphCollegeCentr <- map_dfr(
  names(major_list),

  possibly(
    function(x) {
      vids <- intersect(major_list[[x]], V(graphMajor)$name)
      g <- induced_subgraph(graphMajor, vids)
      as_tibble(graphAttrGet(g)) %>%
        mutate(colloge = x) %>%
        mutate_if(is.numeric, funs(replace_na(., 0)))
    }, otherwise = tibble(), quiet = FALSE
  )
)
saveRDS(allGraphCollegeCentr, 'graph/allGraphCollegeCentr.rds')

pb <- progress_estimated(length(user_list))
allGraphMajorCentr <- map_dfr(
  names(user_list),

  possibly(
    function(x) {
      vids <- intersect(user_list[[x]], V(graphUser)$name)
      g <- induced_subgraph(graphUser, vids)
      pb$tick()$print()

      as_tibble(graphAttrGet(g)) %>%
        mutate(major = x) %>%
        mutate_if(is.numeric, funs(replace_na(., 0)))
    }, otherwise = tibble(), quiet = FALSE
  )
)
saveRDS(allGraphMajorCentr, 'graph/allGraphMajorCentr.rds')
