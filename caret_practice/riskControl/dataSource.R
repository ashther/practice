
library(dplyr)
library(magrittr)

driver <- read.csv('data/driver.csv', stringsAsFactors = FALSE, na.strings = '', 
                   colClasses = c('character', 'integer', 'Date', 'integer', 'integer', 
                                  'character', 'character', 'character', 'integer', 
                                  'character', 'Date')) %>% 
  select(-score) %>% 
  filter((nchar(gsub('0', '', phone)) != 0 | is.na(phone)) & 
           (nchar(gsub('0', '', mobile)) != 0 | is.na(mobile)) & 
           !is.na(dob))

driver <- driver %>% 
  mutate(dob = as.numeric(difftime(Sys.Date(), dob, units = 'days') / 365), 
         license_date = as.numeric(difftime(Sys.Date(), license_date, units = 'days') / 365), 
         reg_add = substr(reg_add, 1, 4), 
         mail_add = substr(mail_add, 1, 4),
         phone = if_else(is.na(phone), 0, 1), 
         mobile = if_else(is.na(mobile), 0, 1))
driver$diff_add <- if_else(driver$reg_add == driver$mail_add, 0, 1)
license_type <- sapply(driver$license, function(x) {
  temp <- c('c' = 0, 'other' = 0)
  tryCatch({
    # temp['c'] <- any(gregexpr('A1|A2|A3', x)[[1]] != -1)
    # temp['b'] <- any(gregexpr('B1|B2', x)[[1]] != -1)
    temp['c'] <- any(gregexpr('C1|C2', x)[[1]] != -1)
    # temp['d'] <- any(gregexpr('C3|C4|D|E|F|M|N|P', x)[[1]] != -1)
    temp['other'] <- any(gregexpr('A1|A2|A3|B1|B2|C3|C4|D|E|F|M|N|P', x)[[1]] != -1)
  })
  return(temp)
}) %>% 
  t()

# driver$license_type_a <- license_type[, 'a']
# driver$license_type_b <- license_type[, 'b']
driver$license_type_c <- license_type[, 'c']
# driver$license_type_d <- license_type[, 'd']
driver$license_type_other <- license_type[, 'other']
driver <- select(driver, -license)

driver$reg_add <- if_else(driver$reg_add %in% 
                            c('6101'), 
                        # c('6101', '6102', '6103', '6104', '6105', '6106',
                        #   '6107', '6108', '6109', '6110'),
                      driver$reg_add, 'other')
driver$mail_add <- if_else(driver$mail_add %in% 
                             c('6101'), 
                         # c('6101', '6102', '6103', '6104', '6105', '6106',
                         #   '6107', '6108', '6109', '6110'),
                       driver$mail_add, 'other')
driver$status <- if_else(driver$status %in% 
                           c('A'), 
                       # c('A', 'G', 'H', 'R'), 
                     driver$status, 'other')
# status_type <- sapply(df$status, function(x) {
#   temp <- c('a' = 0, 'g' = 0, 'h' = 0, 'r' = 0, 'other' = 0)
#   tryCatch({
#     temp['a'] <- any(grepl('A', x))
#     temp['g'] <- any(grepl('G', x))
#     temp['h'] <- any(grepl('H', x))
#     temp['r'] <- any(grepl('R', x))
#     temp['other'] <- !all(grepl('A|G|H|R', x))
#   })
#   return(temp)
# }) %>% 
#   t()
# df$status_a <- status_type[, 'a']
# df$status_g <- status_type[, 'g']
# df$status_h <- status_type[, 'h']
# df$status_r <- status_type[, 'r']
# df$status_other <- status_type[, 'other']
# df <- select(df, -status)

social <- read.csv('data/id_social_2Year.csv', stringsAsFactors = FALSE, na.strings = '')
car <- read.csv('data/id_car.csv', stringsAsFactors = FALSE, na.strings = '')
crime <- read.csv('data/id_crime.csv', stringsAsFactors = FALSE, na.strings = '')
kCar <- read.csv('data/id_kCar.csv', stringsAsFactors = FALSE, na.strings = '')
kCarDisp <- read.csv('data/id_kCarDisp.csv', stringsAsFactors = FALSE, na.strings = '')
kCarPow <- read.csv('data/id_kCarPow.csv', stringsAsFactors = FALSE, na.strings = '')
kCarWb <- read.csv('data/id_kCarWb.csv', stringsAsFactors = FALSE, na.strings = '')
mortCar <- read.csv('data/id_mortCar.csv', stringsAsFactors = FALSE, na.strings = '')
noBusiCar <- read.csv('data/id_noBusiCar.csv', stringsAsFactors = FALSE, na.strings = '')

df <- driver %>% 
  left_join(social, by = 'id') %>% 
  left_join(car, by = 'id') %>% 
  left_join(kCar, by = 'id') %>% 
  left_join(kCarDisp, by = 'id') %>%
  left_join(kCarPow, by = 'id') %>%
  left_join(kCarWb, by = 'id') %>%
  left_join(mortCar, by = 'id') %>%
  left_join(noBusiCar, by = 'id') %>%
  left_join(crime, by = 'id') %>% 
  select(-id) %>% 
  filter(!is.na(car))
df[] <- lapply(df, function(x) {
  x[is.na(x)] <- 0
  x
})

df$score <- if_else(df$score >= 4, 0, 1)
# 'data.frame':	4999 obs. of  24 variables:
# $ gender        : num  1 2 1 2 1 1 1 2 1 1 ...
# $ dob           : num  55.3 54.2 50.6 48 46.1 ...
# $ reg_add       : chr  "6101" "6101" "6101" "6101" ...
# $ mail_add      : chr  "6101" "6101" "6101" "6101" ...
# $ phone         : num  0 1 1 1 1 1 0 0 0 1 ...
# $ mobile        : num  0 0 1 1 1 1 1 1 1 1 ...
# $ status        : chr  "G" "A" "H" "A" ...
# $ license_date  : num  14.02 14.07 3.15 9.17 6.37 ...
# $ diff_add      : num  0 0 0 0 0 0 0 0 0 0 ...
# $ license_type_a: num  0 0 0 0 0 0 0 0 0 0 ...
# $ license_type_b: num  0 0 0 0 1 1 0 0 0 0 ...
# $ license_type_c: num  0 1 1 1 0 0 1 1 1 1 ...
# $ license_type_d: num  1 0 0 0 0 0 1 0 0 0 ...
# $ social        : num  1 8 0 4 0 0 0 3 0 1 ...
# $ car           : num  2 2 2 1 1 1 1 1 1 1 ...
# $ k_car         : num  1 2 1 1 0 1 1 1 1 1 ...
# $ kCarDisp      : num  1591 1798 1206 1781 0 ...
# $ kCarPow       : num  90.4 93 63 74 0 88 93 123 104 77 ...
# $ kCarWb        : num  2650 2638 2720 2656 0 ...
# $ mortCar       : num  0 0 0 0 0 0 0 0 0 0 ...
# $ noBusiCar     : num  1 2 1 1 0 1 1 1 1 1 ...
# $ info          : num  0 13 4 2 0 0 0 0 0 3 ...
# $ money         : num  0 1300 500 200 0 0 0 0 0 300 ...
# $ score         : num  1 0 0 1 1 1 1 1 1 0 ...
# 
# ggplot(df, aes(x = noBusiCar, y = ..count..)) + geom_bar()
# ggplot(df, aes(x = money, y = ..count..)) + geom_histogram()

df <- df %>% 
  filter(social <= 15 & 
           car <= 5 & 
           k_car <= 5 & 
           kCarDisp <= 4000 & 
           kCarPow <= 300 & 
           kCarWb <= 4000 & 
           mortCar <= 1 & 
           noBusiCar <= 5 &
           info <= 15 & 
           money <= 1000) %>% 
  select(-dob) # problem database select!!!
