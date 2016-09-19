library(tidyverse)
library(RMySQL)

con <- dbConnect(MySQL(), host = '10.21.3.101', port = 3306, username = 'r', 
                 password = '123456', dbname = 'shiny_data')

geo_df <- dbGetQuery(con, 
                   "SELECT Round(lat, 1) AS lat, 
                   Round(lon, 1) AS lon, 
                   Count(*)      AS n 
                   FROM   yz_app_track_db.app_start 
                   WHERE  del_status = 0 
                   GROUP  BY 1, 
                   2; ")

city <- pbapply::pbapply(geo_df, 1, function(x) {
  tryCatch(
    RbaiduLBS::revGeocoding(c(x[1], x[2]))$result$addressComponent$city, 
    error = function(e)return('error')
  )
})

lat_lon_city <- cbind(geo_df[, 1:2], city)
dbWriteTable(con, 'lat_lon_city', lat_lon_city, row.names = FALSE, overwirte = TRUE)

dbDisconnect(con)
