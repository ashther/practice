
library(tidyverse)
library(RSQLite)
library(igraph)

con <- dbConnect(RSQLite::SQLite(), 'db/card.sqlite')
transaction <- tbl(con, 'transaction')

# dict and account -------------------------------------------------------

dict <- dbReadTable(con, 'dict')

dict_sex <- filter(dict, TYPENUM == 137) %>% 
  select(DICTNUM, DICTNAME) %>% 
  rename(sex = DICTNAME)

dict_country <- filter(dict, TYPENUM == 142) %>% 
  select(DICTNUM, DICTNAME) %>% 
  rename(country = DICTNAME)

dict_nation <- filter(dict, TYPENUM == 143) %>% 
  select(DICTNUM, DICTNAME) %>% 
  rename(nation = DICTNAME)

dict_politics <- filter(dict, TYPENUM == 141) %>% 
  select(DICTNUM, DICTNAME) %>% 
  rename(politics = DICTNAME)

acc_dep <- dbReadTable(con, 'acc_dep') %>% 
  select(ACCDEPID, ACCDEPNAME, SHORTNAME, PARENTID)

nodes <- dbReadTable(con, 'account') %>% 
  mutate(BIRTHDAY = as.Date(BIRTHDAY), 
         age = case_when(
           !is.na(BIRTHDAY) ~ as.numeric(difftime(Sys.time(), BIRTHDAY, units = 'days')/365), 
           is.na(BIRTHDAY) & (IDTYPE == 1) ~ as.numeric(difftime(
             Sys.time(), as.Date(str_sub(IDNO, 7, 14), format = '%Y%m%d'), units = 'days'
           )/365), 
           is.na(BIRTHDAY) & (IDTYPE != 1) ~ 0, # NA_real_
           TRUE ~ 0
         )) %>% 
  left_join(dict_sex, by = c('SEX' = 'DICTNUM')) %>% 
  left_join(dict_country, by = c('COUNTRY' = 'DICTNUM')) %>% 
  left_join(dict_nation, by = c('NATION' = 'DICTNUM')) %>% 
  left_join(dict_politics, by = c('POLITICS' = 'DICTNUM')) %>% 
  left_join(acc_dep, by = 'ACCDEPID') %>% 
  mutate(sex = case_when(is.na(sex) ~ '未注明', TRUE ~ sex), 
         country = case_when(is.na(country) ~ '未注明', TRUE ~ country), 
         nation = case_when(is.na(nation) ~ '未注明', TRUE ~ nation), 
         politics = case_when(is.na(politics) ~ '未注明', TRUE ~ politics), 
         JOINDATE = case_when(is.na(JOINDATE) ~ '未注明', TRUE ~ JOINDATE)) %>% 
  select(ACCNUM, ACCNAME, ACCDEPID, ACCDEPNAME, SHORTNAME, PARENTID, PERCODE, 
         sex, age, country, nation, politics, JOINDATE)

# get edge list from transaction table -----------------------------------

df <- transaction %>% 
  filter(DEVICENUM != 0 & 
           ISRED == 0 & 
           BUSINESSNUM != 100058) %>% 
  select(ACCNUM, BUSINESSNUM, DEVICENUM, DEALTIME) %>% 
  collect() %>% 
  mutate(
    DEALTIME = case_when(
      nchar(DEALTIME) <= 10 ~ paste(DEALTIME, '0:00:00', sep = ' '), 
      TRUE ~ DEALTIME
    ), 
    DEALTIME = as.POSIXct(DEALTIME)
  ) %>% 
  filter(DEALTIME >= '2017-09-01' & DEALTIME < '2018-02-01') %>% 
  arrange(DEVICENUM, DEALTIME)

dbDisconnect(con)

edgeList <- df %>% 
  mutate(ACCNUM_LAG = lag(ACCNUM), 
         DEVICENUM_LAG = lag(DEVICENUM), 
         DEALTIME_LAG = lag(DEALTIME), 
         is_one = (ACCNUM != ACCNUM_LAG) & 
           (DEVICENUM_LAG == DEVICENUM) & 
           difftime(DEALTIME, DEALTIME_LAG, units = 'secs') <= 60) %>% 
  filter(is_one) %>% 
  select(from = ACCNUM, to = ACCNUM_LAG) %>% 
  count(from, to) %>% 
  # filter(n >= 3) %>% 
  rename(weight = n)

graphCampus <- graph_from_data_frame(edgeList, directed = FALSE, vertices = nodes)
graphCampus <- igraph::simplify(graphCampus, edge.attr.comb = list(weight = 'sum'))

rm(list = c('acc_dep', 'con', 'dict', 'dict_country', 'dict_nation', 
            'dict_politics', 'dict_sex', 'transaction'))
saveRDS(edgeList, 'rds/edgeList.rds')
saveRDS(nodes, 'rds/nodes.rds')
saveRDS(graphCampus, 'rds/graphCampus.rds')


# # test: use business_num instead of device_num ---------------------------
# 
# # 不使用business_num，因为降低边创建的门槛最终导致图反映的是学生的作息和课程
# # 设置情况，去除水控刷卡数据是同样的道理
# 
# library(data.table)
# # test: use businessnum instead of devicenum
#  
# dt <- read_rds('rds/df_no_100058.rds') %>% 
#   select(-DEVICENUM) %>% 
#   filter(BUSINESSNUM != 0 & BUSINESSNUM != 100023) %>% 
#   mutate(DEALTIME = unclass(DEALTIME)) %>% 
#   arrange(BUSINESSNUM, DEALTIME) %>% 
#   as.data.table()
# 
# setkey(dt, BUSINESSNUM, DEALTIME)
# 
# # pb <- progress_estimated(nrow(dt))
# # edgeList_test <- map_dfr(seq_len(nrow(dt)), ~ {
# #   test <- dt[.x]
# #   pb$tick()$print()
# #   temp <- dt[
# #     .(test$BUSINESSNUM)
# #   ][
# #      DEALTIME >= test$DEALTIME &
# #        DEALTIME <= test$DEALTIME + 30
# #   ]
# #   
# #   temp[, from := test$ACCNUM]
# # 
# # })
# 
# 
# # pb <- progress_estimated(length(unique(dt$BUSINESSNUM)))
# edgeList_test <- map_dfr(unique(dt$BUSINESSNUM), function(x) {
#   test <- dt[.(x)]
#   print(sprintf('[%s]: %s(%s)', 
#                 Sys.time(), 
#                 which(unique(dt$BUSINESSNUM) == x), 
#                 nrow(test)))
#   pb <- progress_estimated(nrow(test))
#   
#   map_dfr(seq_len(nrow(test)), function(y) {
#     temp <- test[y]
#     res <- test[DEALTIME >= temp$DEALTIME & 
#                   DEALTIME <= temp$DEALTIME + 30]
#     pb$tick()$print()
#     res[, from := temp$ACCNUM]
#   })
#   
# })
# 
# 
# edgeList_test <- select(edgeList_test, from, ACCNUM, DEALTIME) %>% 
#   filter(from != ACCNUM) %>% 
#   count(from, ACCNUM) %>% 
#   rename(to = ACCNUM, weight = n)