
library(RSQLite)
library(readr)
library(crayon)

con <- dbConnect(RSQLite::SQLite(), 'db/card.sqlite')

account <- read_csv('data/AM_Account.csv', locale = locale(encoding = 'gbk'))
dbWriteTable(con, 'account', account, append = FALSE)
rm(account)

acc_class <- read_csv('data/SC_AccClass.csv', locale = locale(encoding = 'gbk'))
dbWriteTable(con, 'acc_class', acc_class, append = FALSE)
rm(acc_class)

acc_dep <- read_csv('data/SC_AccDep.csv', locale = locale(encoding = 'gbk'))
dbWriteTable(con, 'acc_dep', acc_dep, append = FALSE)
rm(acc_dep)

business <- read_csv('data/SC_Business.csv', locale = locale(encoding = 'gbk'))
dbWriteTable(con, 'business', business, append = FALSE)
rm(business)

device <- read_csv('data/SC_Device.csv', locale = locale(encoding = 'gbk'))
dbWriteTable(con, 'device', device, append = FALSE)
rm(device)

dict <- read_csv('data/SC_Dict.csv', locale = locale(encoding = 'gbk'))
dbWriteTable(con, 'dict', dict, append = FALSE)
rm(dict)

dict_ep <- read_csv('data/SC_Dict_EP.csv', locale = locale(encoding = 'gbk'))
dbWriteTable(con, 'dict_ep', dict_ep, append = FALSE)
rm(dict_ep)

transaction_type <- read_csv('data/SC_TransactionType.csv', locale = locale(encoding = 'gbk'))
dbWriteTable(con, 'transaction_type', transaction_type, append = FALSE)
rm(transaction_type)

device_type <- read_csv('data/SC_DeviceType.csv', locale = locale(encoding = 'gbk'))
dbWriteTable(con, 'device_type', device_type, append = FALSE)
rm(device_type)

if (dbExistsTable(con, 'transaction')) {
  dbRemoveTable(con, 'transaction')
}
transaction <- list.files('data/', full.names = TRUE, pattern = '^MC_Transaction')
purrr::walk(transaction, ~ {
  cat(sprintf('%s: reading %s\n', Sys.time(), .x))
  temp <- read_csv(.x, locale = locale(encoding = 'gbk'))
  cat(sprintf('%s: finish reading\n', Sys.time()))
  dbWriteTable(con, 'transaction', temp, append = TRUE)
  cat(sprintf('%s: finish inserting\n', Sys.time()))
  rm(temp)
})

dbDisconnect(con)
