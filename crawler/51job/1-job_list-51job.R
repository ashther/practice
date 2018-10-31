
library(parallel)
library(glue)
library(luzlogr)

# define page parse functioni --------------------------------------------

jobUrlGet <- function(page) {
  map_chr(html_nodes(page, '.dw_table div.el .t1 a'), html_attr, name = 'href')
}
companyUrlGet <- function(page) {
  map_chr(html_nodes(page, '.dw_table div.el .t2 a'), html_attr, name = 'href')
}

pageParse <- function(page) {
  map(paste0('t', 1:5), ~ {
    html_nodes(page, glue('.dw_table div.el .{.x}')) %>% 
      html_text(TRUE)
  }) %>% 
    bind_cols() %>% 
    setNames(.[1, ]) %>% 
    slice(2:nrow(.)) %>% 
    mutate(job_url = jobUrlGet(page),
           company_url = companyUrlGet(page))
}

# crawl ------------------------------------------------------------------

main <- function() {
  openlog('log/job_list_51job.log', append = TRUE, sink = TRUE)
  on.exit(closelog(FALSE))
  
  source('utils.R', local = FALSE)
  on.exit(dbDisconnect(con), add = TRUE)
  
  page_id <- 1
  page_max <- glue(url_51) %>% 
    pageGet() %>% 
    html_node('.p_in span.td') %>% 
    html_text(TRUE) %>% 
    stringr::str_extract('[0-9]+') %>% 
    as.integer()
  printlog(sprintf('max page is: %s', page_max))
  
  cl <- makeCluster(2, outfile = '')
  on.exit(stopCluster(cl), add = TRUE)
  clusterEvalQ(cl, source('utils.R', local = TRUE))
  clusterExport(cl, 'pageParse', environment())
  clusterExport(cl, 'jobUrlGet', environment())
  clusterExport(cl, 'companyUrlGet', environment())
  clusterExport(cl, 'page_max', environment())
  clusterEvalQ(cl, library(glue))
  printlog(sprintf('socket cluster with %s nodes is ready', length(cl)))
  
  result <- bind_rows(pbapply::pblapply(1:page_max, function(x) {
    Sys.sleep(2)
    page_id <- x
    # print(sprintf('crawling page: %s - %s', x, page_max))
    pageGetParseSafe(glue(url_51))
  }, cl = cl))
  printlog(sprintf('crawling finished'))
  
  result <- result %>% 
    setNames(c('job', 'company', 'location', 'salary', 'publish_date', 
               'job_url', 'company_url')) %>% 
    select(job, job_url, company, company_url, location, salary, publish_date)
  
  dbWriteTable(con, 'job_list', result, append = TRUE, row.names = FALSE)
  printlog(sprintf('insert into database: %s', nrow(result)))
  
  invisible(NULL)
}

main()
