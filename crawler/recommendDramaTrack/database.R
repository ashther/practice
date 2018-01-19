
library(RSQLite)
library(dplyr)

con <- dbConnect(RSQLite::SQLite(), 'data/drama.sqlite')
# dbSendQuery(
#   con,
#   paste0('CREATE TABLE recommend ',
#          # '(id INTEGER PRIMARY KEY AUTOINCREMENT, ',
#          '(rate NUMERIC, title VARCHAR(255), id LONG, rank INT, date TEXT, ',
#          'time_stamp TEXT)')
# )
# 
# dbSendQuery(
#   con,
#   paste0('CREATE TABLE rating ',
#          # '(id INTEGER PRIMARY KEY AUTOINCREMENT, ',
#          '(id LONG, rating NUMERIC, votes INT, rating_per TEXT, tags TEXT, ',
#          'doing TEXT, collection TEXT, wish TEXT, comment TEXT, review TEXT, time_stamp TEXT)')
# )
# 
# test_recommend_df <- data_frame(rate = 1.0, title = 'test title', id = 26322999, rank = 1,
#                       date = as.character(Sys.Date()), time_stamp = as.character(Sys.time()))
# 
# test_rating_df <- data_frame(id = 26322999, rating = 1.0, votes = 1, rating_per = '1 2 3 4 5',
#                              tags = 'a b c', doing = '1', collection = '1', wish = '1',
#                              comment = '1', review = '1',
#                              time_stamp = as.character(Sys.time()))
# 
# dbWriteTable(con, 'recommend', test_recommend_df, append = TRUE)
# dbWriteTable(con, 'rating', test_rating_df, append = TRUE)

test <- dbReadTable(con, 'rating')

dbDisconnect(con)
