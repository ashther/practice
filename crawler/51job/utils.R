
suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(dplyr)
  library(purrr)
  library(RSQLite)
})

db_path <- '/home/ashther/employment/data/proxy.sqlite'
url_51 <- "https://search.51job.com/list/200200,000000,0000,00,9,07,%2B,2,{page_id}.html?ord_field=1"


# make database copy if copy doesn't exist or copy is older than 1 day --------

file_name <- basename(db_path)
file_extension <- paste0('.', tools::file_ext(file_name))
file_copy <- paste0(gsub(file_extension, '', db_path), '-copy', file_extension)

if (!file.exists(file_copy) | as.numeric(
  difftime(file.mtime(db_path), file.mtime(file_copy), units = 'days')
) > 1) {
  file.copy(db_path, paste0(gsub(file_name, '', db_path), file_copy))
}

con <- dbConnect(RSQLite::SQLite(), db_path)

# create proxy table if not exist ----------------------------------------

if (!dbExistsTable(con, 'proxy')) {
  dbSendQuery(
    con,
    glue::glue(
      "create table proxy 
      (id integer not null primary key autoincrement, 
      ip text not null, 
      port int not null, 
      status int default 1, 
      time_stamp timestamp not null default current_timestamp);"
    )
  )
}
# dbWriteTable(con, 'proxy', proxy, append = TRUE, row.names = FALSE)


# create job_list table if not exist -------------------------------------

if (!dbExistsTable(con, 'job_list')) {
  dbSendQuery(
    con,
    glue::glue(
      "create table job_list 
      (id integer not null primary key autoincrement, 
      job text, 
      job_url text, 
      company text, 
      company_url text, 
      location text, 
      salary text, 
      publish_date text, 
      time_stamp timestamp not null default current_timestamp);"
    )
  )
}
# dbWriteTable(con, 'job_list', job_list, append = TRUE, row.names = FALSE)


# create job_info table if not exist -------------------------------------

if (!dbExistsTable(con, 'job_info')) {
  dbSendQuery(
    con,
    glue::glue(
      "create table job_info 
      (id integer not null primary key autoincrement,  
      job_url text, 
      title text, 
      msg text, 
      tag text, 
      job_msg text, 
      address text, 
      time_stamp timestamp not null default current_timestamp);"
    )
  )
}


# create company_info table if not exist ---------------------------------

if (!dbExistsTable(con, 'company_info')) {
  dbSendQuery(
    con,
    glue::glue(
      "create table company_info 
      (id integer not null primary key autoincrement,  
      company_url text, 
      title text, 
      flag text, 
      people text, 
      trade text, 
      msg text, 
      address text, 
      time_stamp timestamp not null default current_timestamp);"
    )
  )
}
# dbDisconnect(con)

# utils ------------------------------------------------------------------

proxy <- dbReadTable(con, 'proxy') %>% 
  filter(status == 1)
proxyGoodSelect <- function() {
  sample_n(proxy, 1)
}

proxyBadDelete <- function(proxy_list) {
  # con <- dbConnect(RSQLite::SQLite(), db_path)
  # on.exit(dbDisconnect(con), add = TRUE)
  
  proxy_list <- as_tibble(proxy_list)
  sql <- glue::glue_data(
    proxy_list, 
    'update proxy set status = 0 where ip = "{ip}" and port = {port}'
  )
  
  walk(sql, safely(function(s) {
    dbSendQuery(con, s)
  }))
  
  invisible(NULL)
}

pageGet <- function(url, iter_max = 10) {
  
  page_got <- 0
  iter <- 1
  while (iter <= iter_max) {
    proxy <- proxyGoodSelect()
    if (nrow(proxy) == 0) {
      stop('no more good proxy.')
    }
    
    tryCatch({
      temp <- GET(
        url, 
        use_proxy(proxy$ip, proxy$port), 
        timeout(5)
      )
      page_got <- 1
    }, error = function(e) {
      print(sprintf('bad proxy: %s-%s', proxy$ip, proxy$port))
      # proxyBadDelete(proxy) # may be proxy just un-connect for a while
    })
    
    if (page_got == 1) break()
    iter <- iter + 1
  }
  
  if (page_got != 1 && iter > iter_max) {
    stop('try too many times.')
  }
  tryCatch(
    content(temp, encoding = 'utf-8'), 
    error = function(e)content(temp, encoding = 'gbk')
  )
}

pageGetParseSafe <- purrr::possibly(
  .f = function(url) {
    pageParse(pageGet(url))
  }, 
  otherwise = dplyr::tibble(), 
  quiet = FALSE
)

# main process -----------------------------------------------------------

# env_proxy <- new.env()
# sys.source('0-proxy.R', env_proxy)
# 
# env_job_list <- new.env()
# sys.source('1-job_list-51job.R', env_job_list)
# 
# env_job_info <- new.env()
# sys.source('1-job_info-51job.R', env_job_info)
