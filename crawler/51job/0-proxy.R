
proxyRefresh <- function(page_id = 1) {
  proxy_url <- glue::glue('http://www.xicidaili.com/nt/{page_id}')
  read_html(proxy_url) %>% 
    html_table() %>% 
    .[[1]] %>% 
    rename(ip = `IP地址`, port = `端口`) %>% 
    select(ip, port)
}

proxyFilter <- function(proxy_list, target = 'www.baidu.com') {
  # con <- dbConnect(RSQLite::SQLite(), db_path)
  # on.exit(dbDisconnect(con), add = TRUE)
  
  proxy_db <- dbReadTable(con, 'proxy')
  proxy_list <- anti_join(proxy_list, proxy_db, by = c('ip', 'port'))
  
  pb <- progress_estimated(nrow(proxy_list))
  proxy_list$status <- pmap_dbl(
    proxy_list, possibly(function(ip, port) {
      pb$tick()$print()
      GET(target, use_proxy(ip, port), timeout(3))
      Sys.sleep(2)
      1
    }, otherwise = 0)
  )
  proxy_list
}

proxyBatchGet <- function(iter_max = NULL, result_max = NULL, time_out = 300) {
  stopifnot(!is.null(iter_max) | !is.null(result_max)) 
  
  if (!is.null(iter_max)) {
    iwalk(seq_len(iter_max), ~ {
      temp <- proxyFilter(proxyRefresh(.y))
      printlog(glue::glue(
        'iter-{.y}, {nrow(temp[temp$status == 1, ])} new get'
      ))
      proxyInsert2DB(temp)
    })
    
  } else {
    page_id <- 1
    start_time <- Sys.time()
    while (TRUE) {
      past_time <- as.numeric(Sys.time() - start_time)
      printlog(sprintf('%s seconds to go', time_out - past_time))
      temp <- proxyFilter(proxyRefresh(page_id))
      page_id <- page_id + 1
      proxyInsert2DB(temp)
      if (nrow(res) >= result_max | 
          past_time > time_out) {
        break()
      }
    }
  }
}

proxyInsert2DB <- function(proxy_list) {
  # con <- dbConnect(RSQLite::SQLite(), db_path)
  # on.exit(dbDisconnect(con), add = TRUE)
  
  proxy_list <- as_tibble(proxy_list)
  
  dbWriteTable(
    con, 'proxy', proxy_list, append = TRUE, row.names = FALSE
  )
  printlog(sprintf('insert into database: %s', nrow(proxy_list)))
  
  invisible(NULL)
}

# crawl proxy and save ---------------------------------------------------

library(luzlogr)

openlog('log/proxy.log', append = TRUE)

source('utils.R', local = TRUE)
proxyBatchGet(iter_max = 3)

dbDisconnect(con)
closelog(FALSE)