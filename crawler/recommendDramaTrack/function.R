
simpleCss <- function(css, .html_content = html_content) {
  html_text(html_nodes(.html_content, css))
}

simpleFindReplace <- function(string, patr, repl) {
  str_trim(str_replace(str_subset(string, patr), patr, repl))
}

dramaApiGet <- function(id, drama_info_path) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(httr)
    library(luzlogr)
  })
  
  api_url <- 'http://api.douban.com/v2/movie/subject/'
  
  tryCatch({
    
    drama <- content(GET(paste0(api_url, id)))
    printlog(sprintf('drama info got, url_id: %s', id))
    Sys.sleep(1.5)
    
    if (file.exists(drama_info_path)) {
      drama_all <- readRDS(drama_info_path)
    } else {
      drama_all <- list()
    }
    drama_all[[as.character(id)]] <- drama
    saveRDS(drama_all, drama_info_path)
    
  }, error = function(e) {
    printlog(sprintf('url_id: %s, api error: %s', id, e$message))
  })

}

dramaRatingGet <- function(id, drama_rating_path, timeout) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(rvest)
    library(stringr)
    library(methods)
    library(R.utils)
    library(luzlogr)
    library(RSQLite)
  })
  
  base_url <- 'https://movie.douban.com/subject/'
  
  tryCatch({
    
    con <- dbConnect(RSQLite::SQLite(), drama_rating_path)
    on.exit(dbDisconnect(con))
    
    withTimeout(
      html_content <- content(GET(
        paste0(base_url, id), 
        config = config(cookie = readLines('/home/slj_temp/recommendDramaTrack/cookie'))
      )), 
      timeout = timeout
    )
    printlog(sprintf('drama rating got, url_id: %s', id))
    Sys.sleep(5)
    
    rating <- simpleCss('strong[property="v:average"]', html_content)
    votes <- simpleCss('span[property="v:votes"]', html_content)
    rating_per <- paste0(simpleCss('.rating_per', html_content), collapse = ' ')
    tags <- paste0(simpleCss('a[href^="/tag"]', html_content), collapse = ' ')
    doing <- simpleCss('.subject-others-interests-ft a[href$="doings"]', html_content)
    collection <- simpleCss('.subject-others-interests-ft a[href$="collections"]', html_content)
    wish <- simpleCss('.subject-others-interests-ft a[href$="wishes"]', html_content)
    comment <- simpleCss('a[href$="comments?status=P"]', html_content)
    review <- simpleCss('header a[href="reviews"]', html_content)
    time_stamp <- as.character(Sys.time())
    
    drama_rating <- data_frame(
      id = id, 
      rating = rating, 
      votes = votes, 
      rating_per = rating_per, 
      tags = tags, 
      doing = doing, 
      collection = collection, 
      wish = wish, 
      comment = comment, 
      review = review, 
      time_stamp = time_stamp
    )
    
    dbWriteTable(con, 'rating', drama_rating, append = TRUE)
    
  }, error = function(e) {
    printlog(sprintf('url_id: %s, rating error: %s', id, e$message))
  })
}

recommendDramaCron <- function(drama_rating_path, drama_info_path,  
                               timeout = 30, 
                               log_path = '/home/slj_temp/log') {
  suppressPackageStartupMessages({
    library(luzlogr)
    library(jsonlite)
    library(dplyr)
    library(httr)
    library(RSQLite)
  })
  
  if (!dir.exists(log_path)) {
    dir.create(log_path)
  }
  openlog(file.path(log_path, 'recommendDrama.log'), append = TRUE)
  on.exit(closelog(sessionInfo = FALSE))
  
  recommend_url <- 'https://movie.douban.com/j/search_subjects?type=tv&tag=%E5%9B%BD%E4%BA%A7%E5%89%A7&sort=recommend&page_limit=20&page_start=0'
  
  tryCatch({
    
    con <- dbConnect(RSQLite::SQLite(), drama_rating_path)
    on.exit(dbDisconnect(con))
    
    recommend_drama <- GET(
      recommend_url, 
      config = config(cookie = readLines('/home/slj_temp/recommendDramaTrack/cookie'))
    ) %>% 
      content() %>%
      toJSON() %>% 
      fromJSON() %>% 
      .[['subjects']] %>% 
      select(rate, title, id) %>% 
      mutate(rank = row_number(), 
             date = as.character(Sys.Date()), 
             time_stamp = as.character(Sys.time())) %>% 
      mutate_if(is.list, unlist)
    printlog(sprintf('got new recommend drama: %s', nrow(recommend_drama)))

    dbWriteTable(con, 'recommend', recommend_drama, append = TRUE)
    printlog('new recommend drama inserted')
    
    invisible(lapply(recommend_drama$id, function(x) {
      tryCatch({
        printlog(sprintf('info and rating reading... %s/%s', 
                         which(recommend_drama$id == x), 
                         nrow(recommend_drama)))
        dramaApiGet(x, drama_info_path)
        dramaRatingGet(x, drama_rating_path, timeout)
        
      }, error = function(e) {
        printlog(sprintf('url_id: %s, error: %s', x, e$message))
      })
    }))
    
  }, error = function(e) {
    printlog(e$message)
  })
}