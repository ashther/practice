library(tidyverse)
library(RMySQL)
library(RbaiduLBS)

con <- dbConnect(MySQL(), host = '10.21.3.101', port = 3306, username = 'r', 
                 password = '123456', dbname = 'shiny_data')
dbSendQuery(con, 'set names gbk')
geo_df <- dbGetQuery(con, 
                   "SELECT DISTINCT ROUND(lat, 1) AS lat, ROUND(lon, 1) AS lon
                   FROM yz_app_track_db.app_start
                   WHERE del_status = 0
                   AND CONCAT(ROUND(lat, 1), '-', ROUND(lon, 1)) NOT IN (
                   SELECT CONCAT(ROUND(lat, 1), '-', ROUND(lon, 1)) 
                   FROM shiny_data.lat_lon_city);")

city <- pbapply::pbapply(geo_df, 1, function(x) {
  tryCatch({
    result <- revGeocoding(c(x[1], x[2]))
    return(
      c(
        result$result$addressComponent$province, 
        result$result$addressComponent$city
      )
    )
  }, error = function(e)return(c(e$message, ''))
  )
}) %>% 
  t()

lat_lon_city <- cbind(geo_df[, 1:2], city)
colnames(lat_lon_city) <- c('lat', 'lon', 'province', 'city')

temp <- lat_lon_city
temp$province <- as.character(temp$province)
temp$city <- as.character(temp$city)

Encoding(temp$city) <- 'utf-8'
Encoding(temp$province) <- 'utf-8'

# for empty lat_lon_city
# dbWriteTable(con, 'lat_lon_city', temp, row.names = FALSE, overwrite = TRUE)
dbWriteTable(con, 'lat_lon_city', temp, row.names = FALSE, append = TRUE)

dbDisconnect(con)
