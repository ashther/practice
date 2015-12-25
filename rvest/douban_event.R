library(RCurl)
library(rjson)
library(magrittr)

city_list_url <- 'https://api.douban.com/v2/loc/list'
# {
# "count": 3,
# "start": 0,
# "total": 882,
# "locs": [
#     {
#         "parent": "china",
#         "habitable": "yes",
#         "id": "108288",
#         "name": "北京",
#         "uid": "beijing"
#     }

event_list_url <- 'https://api.douban.com/v2/event/list?'
# loc	id	
# day_type	future, week, weekend, today, tomorrow
# type	all,music, film, drama, commonweal, salon, 
# exhibition, party, sports, travel, others

event_url <- 'https://api.douban.com/v2/event/'
# https://api.douban.com/v2/event/10069638/wishers
# https://api.douban.com/v2/event/10069638/participants
# {
# "count": 20,
# "start": 0,
# "total": 2,
# "users": [
#     {
#         "avatar": "http://img3.douban.com/icon/u54770363-2.jpg",
#         "alt": "http://www.douban.com/people/54770363/",
#         "id": "54770363",
#         "name": "小子",
#         "uid": "54770363"
#     },

# https://api.douban.com/v2/event/16878708
# {
# "participant_count": 27,
# "id": "16878708",
# "title": "美国乡村音乐：RandyAbelStable",
# "wisher_count": 114,
# "end_time": "2012-08-1723: 30: 00",
# "loc_name": "北京"
# }

city_list <- getURL(city_list_url) %>% 
    fromJSON() %>% 
    `[[`('locs') %>% 
    unlist() %>% 
    matrix(ncol = 5, byrow = TRUE, 
           dimnames = list(NULL, c('parent', 'habitable', 'id', 'name', 'uid'))) %>% 
    as.data.frame(stringsAsFactors = FALSE)

event_list_temp <- paste0(event_list_url, 'loc=', city_list[city_list$uid == 'xian', 'id'], 
                     '&day_type=week&type=film') %>% getURL() %>% fromJSON
event_list <- data.frame(replicate(event_list_temp$total, 
                                   c('id' = NA, 'title' = NA, 'loc_name' = NA, 
                                     'category' = NA, 'wisher_count' = NA, 'participant_count' = NA, 
                                     'end_time' = NA)) %>% t)
for (i in seq_along(event_list_temp$events)) {
    event_list[i, ] <- with(event_list_temp$events[[i]], 
                            c(id, title, loc_name, category,
                              wisher_count, participant_count, end_time))
}



















