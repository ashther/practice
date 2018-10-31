
library(parallel)
library(stringr)
library(glue)
library(luzlogr)

# get job_url diff between job_list and job_info ----------------------

jobNewDiff <- function(start = Sys.Date()) {
  # con <- dbConnect(RSQLite::SQLite(), db_path)
  # on.exit(dbDisconnect(con), add = TRUE)
  
  sql <- "
  SELECT DISTINCT job_url,
                  company_url
  FROM job_list
  LEFT JOIN job_info using(job_url)
  WHERE job_info.job_url IS NULL"
  sql_start <- " and job_list.time_stamp >= ?start "
  
  if (!is.null(start)) {
    sql <- paste0(sql, sql_start)
    sql <- DBI::sqlInterpolate(DBI::ANSI(), sql, start = as.character(start))
  }
  
  dbGetQuery(con, sql)
}

# define page parse function ---------------------------------------------

pageParse <- function(page) {
  company_tag <- map_dfr(
    html_nodes(page, '.com_tag p.at'), possibly(function(x) {
      tibble(label = html_attr(html_node(x, 'span'), 'class'), 
             text = html_text(x))
    }, otherwise = tibble())
  )
  
  tibble(
    job_title = html_attr(html_node(page, '.cn h1'), name = 'title'), 
    job_msg = html_attr(html_node(page, '.msg.ltype'), name = 'title'), 
    job_tag = paste0(html_text(html_nodes(page, '.jtag span.sp4')), collapse = '|'), 
    job_msg_inbox = str_squish(html_text(html_nodes(page, '.bmsg.job_msg.inbox'))), 
    address = tryCatch(str_squish(html_text(html_nodes(page, '.bmsg.inbox')[2])), 
                       error = function(e)character(0)), 
    
    company_title = html_attr(html_node(page, '.com_name p'), name = 'title'), 
    company_flag = ifelse('i_flag' %in% company_tag$label, 
                          company_tag$text[company_tag$label == 'i_flag'], 
                          character(0)), 
    company_people = ifelse('i_people' %in% company_tag$label, 
                            company_tag$text[company_tag$label == 'i_people'], 
                            character(0)), 
    company_trade = ifelse('i_trade' %in% company_tag$label, 
                           company_tag$text[company_tag$label == 'i_trade'], 
                           character(0)),
    company_msg = str_squish(html_text(html_nodes(page, '.tmsg.inbox')))
  )
}

# crawl ------------------------------------------------------------------

main <- function() {
  openlog('log/job_info-51job.log', append = TRUE, sink = TRUE)
  on.exit(closelog(FALSE))
  
  source('utils.R', local = FALSE)
  on.exit(dbDisconnect(con), add = TRUE)
  
  job_new <- jobNewDiff()
  
  cl <- makeCluster(2, outfile = '')
  on.exit(stopCluster(cl), add = TRUE)
  clusterEvalQ(cl, source('utils.R', local = TRUE))
  clusterExport(cl, 'pageParse', environment())
  clusterEvalQ(cl, library(glue))
  clusterEvalQ(cl, library(stringr))
  printlog(sprintf('socket cluster with %s nodes is ready', length(cl)))
  
  job_new$info <- pbapply::pblapply(job_new$job_url, function(x) {
    Sys.sleep(2)
    # print(sprintf('crawling page: %s', x))
    pageGetParseSafe(x)
  }, cl = cl) 
  job_new <- tidyr::unnest(job_new)
  
  if (nrow(job_new) == 0) {
    stop('no more job or company info to be insert into database.')
  }
  
  job_new_only <- select(
    job_new, job_url, job_title, job_msg, job_tag, job_msg_inbox, address
  ) %>% 
    rename(title = job_title, 
           msg = job_msg, 
           tag = job_tag, 
           job_msg = job_msg_inbox)
  
  dbWriteTable(
    con, 'job_info', job_new_only, append = TRUE, row.names = FALSE
  )
  printlog(sprintf('insert into job info database: %s', nrow(job_new_only)))
  
  company_db <- dbGetQuery(con, 'select distinct company_url from company_info;')
  job_new_company <- select(
    job_new, company_url, company_title, company_flag, 
    company_people, company_trade, company_msg, address
  ) %>% 
    rename(title = company_title, 
           flag = company_flag, 
           people = company_people, 
           trade = company_trade, 
           msg = company_msg) %>% 
    distinct() %>% 
    anti_join(company_db, by = 'company_url')
  if (nrow(job_new_company) == 0) {
    stop('no more new company info to be inserted.')
  }
  
  dbWriteTable(
    con, 'company_info', job_new_company, append = TRUE, row.names = FALSE
  )
  printlog(sprintf('insert into company info database: %s', nrow(job_new_company)))
  
  invisible(NULL)
}

main()

