
library(RSQLite)
library(dplyr)

con <- dbConnect(RSQLite::SQLite(), 'data/weibo.sqlite')
# dbSendQuery(
#   con,
#   paste0('CREATE TABLE drama_26584874 ', # add url_id column
#          '(id INTEGER PRIMARY KEY AUTOINCREMENT, `page` INT, ',
#          'id_weibo TEXT, `text` VARCHAR(255), source VARCHAR(255), reposts_count INT, ',
#          'comments_count INT, attitudes_count INT, created_at TEXT, ',
#          'user_id TEXT, screen_name TEXT, followers_count INT, ',
#          'follow_count INT, gender TEXT, time_stamp TEXT);')
# )

test <- dbReadTable(con, 'drama_26584874')

dbDisconnect(con)
