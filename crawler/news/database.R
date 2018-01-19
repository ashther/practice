
library(RSQLite)
library(dplyr)

con <- dbConnect(RSQLite::SQLite(), 'data/news.sqlite')
# dbSendQuery(
#   con,
#   paste0('CREATE TABLE news ', # add url_id column
#          '(id INTEGER PRIMARY KEY AUTOINCREMENT, ',
#          'title VARCHAR(255), ts VARCHAR(255), amount VARCHAR(255), source VARCHAR(255), ', 
#          'zan VARCHAR(255), content TEXT, url_id INT, ', 
#          'timestamp TIMESTAMP not null default CURRENT_TIMESTAMP)')
# )
# 
# test_df <- data_frame(title = 'test_title', ts = 'test_ts', amount = 'test_amount', 
#                       source = 'test_source', zan = 'test_zan', content = 'test_content', 
#                       url_id = 'test_url_id')
# 
# dbWriteTable(con, 'news', test_df, append = TRUE)

test <- dbReadTable(con, 'news')

dbDisconnect(con)


