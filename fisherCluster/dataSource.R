
library(magrittr)
library(dplyr)
library(Rcpp)
source('fisherCluster.R')
sourceCpp('minLossSplitCpp.cpp')

timeToSeq <- function(time_vec, start_time, by) {
  # time_seq <- vector('integer', diff(time_range))
  # names(time_seq) <- seq_len(diff(time_range))
  # start_time <- as.POSIXct(format(time_vec[1], format = '%Y-%m-%d'))
  # value <- table(floor(difftime(time_vec, start_time, units = 'mins')) - time_range[1])
  # time_seq[names(value)] <- value
  # return(time_seq)
  start_time <- as.POSIXct(paste(format(time_vec[1], format = '%Y-%m-%d'), start_time))
  value <- floor(as.numeric(difftime(time_vec, start_time, units = 'mins'))/by)
  return(value)
}

test <- readLines('d:/t_passenger_flow_result_log.sql')
header <- test[23:47] %>% 
  strsplit('\\"') %>% 
  sapply(function(x)x[2])
# [1] "pfr_uuid"              "pfr_line_name"         "pfr_line_uuid"        
# [4] "pfr_line_type"         "pfr_station_seq"       "pfr_line_station_uuid"
# [7] "pfr_lng_lat"           "pfr_open_door_time"    "pfr_close_door_time"  
# [10] "pfr_get_on_number"     "pfr_get_off_number"    "pfr_upload_time"      
# [13] "pfr_pfod_uuid"         "prf_enter_out_status"  "prf_dev_uuid"         
# [16] "prf_dev_code"          "prf_get_f_on_number"   "prf_get_f_off_number" 
# [19] "prf_get_c_on_number"   "prf_get_c_off_number"  "prf_get_e_on_number"  
# [22] "prf_get_e_off_number"  "prf_dev_datastring"    "prf_get_person_count" 
# [25] "pfr_station_uuid" 
df <- test[59:(length(test) - 9)] %>% 
  pbapply::pblapply(function(x) {
    temp <- substr(x, 60, nchar(x) - 2) %>% 
      strsplit(',') %>% 
      unlist() %>% 
      stringi::stri_trim()
    idx <- grepl('\'', temp)
    temp[idx] <- substr(temp[idx], 2, nchar(temp[idx]) - 1)
    temp
  }) %>% 
  do.call('rbind', .) %>% 
  data.frame(stringsAsFactors = FALSE)
rm(test)

colnames(df) <- header
df <- df %>% 
  select(pfr_line_uuid, pfr_line_type, 
         pfr_station_seq, pfr_line_station_uuid, pfr_station_uuid, 
         pfr_get_on_number, pfr_get_off_number, prf_get_person_count,  
         pfr_upload_time)
df$pfr_line_type <- as.integer(df$pfr_line_type)
df$pfr_station_seq <- as.integer(df$pfr_station_seq)
df$pfr_get_off_number <- as.integer(df$pfr_get_off_number)
df$pfr_get_on_number <- as.integer(df$pfr_get_on_number)
df$prf_get_person_count <- as.integer(df$prf_get_person_count)
df$pfr_upload_time <- as.POSIXct(df$pfr_upload_time)
#Sys.setlocale("LC_TIME", "C")
df$weekdays <- weekdays(df$pfr_upload_time, abbreviate = TRUE)

df_0427 <- filter(df, lubridate::date(pfr_upload_time) == '2017-04-27') %>% 
  select(-pfr_line_uuid, -pfr_line_station_uuid, -pfr_station_uuid,  
         pfr_line_type, seq = pfr_station_seq, on = pfr_get_on_number, 
         off = pfr_get_off_number, count = prf_get_person_count, 
         time = pfr_upload_time, weekdays) %>% 
  arrange(time)

df_0427$time = timeToSeq(df_0427$time, '04:00:00', by = 5)

pf <- df_0427 %>% 
  group_by(time) %>% 
  summarise(n = sum(on)) %>% 
  left_join(data.frame(time = seq_len((24 - 4) * (60 / 5))), .)
pf$n[is.na(pf$n)] <- 0
