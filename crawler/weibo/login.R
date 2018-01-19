
cookieGet <- function(cookie = '/home/slj_temp/cookie', 
                      log_path = '/home/slj_temp/log/cookieGet.log') {
  library(RSelenium)
  library(purrr)
  library(stringr)
  library(luzlogr)
  library(methods)
  
  log_path_dir <- str_replace(log_path, paste0('/', basename(log_path)), '')
  if (!dir.exists(log_path_dir)) {
    dir.create(log_path_dir)
  }
  openlog(log_path, append = TRUE, sink = FALSE)
  on.exit(closelog(sessionInfo = FALSE), add = TRUE)
  
  tryCatch({
    pjs_pid <- system('pgrep phantomjs', intern = TRUE)
    if (length(pjs_pid) == 0) {
      pjs <- phantom('/home/slj_temp/weibo/phantomjs-2.1.1-linux-x86_64/bin/phantomjs',
                     port = 5555)
    } else {
      system(paste0('sudo -kS kill ', pjs_pid), input = '')
      pjs <- phantom('/home/slj_temp/weibo/phantomjs-2.1.1-linux-x86_64/bin/phantomjs',
                     port = 5555)
    }
    on.exit(pjs$stop(), add = TRUE)
    pjs_pid <- system('pgrep phantomjs', intern = TRUE)
    printlog(sprintf('pjs pid is: %s', pjs_pid))
    
    ecap <- list(
      phantomjs.page.settings.userAgent = 
        'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36', 
      phantomjs.binary.path = '/home/slj_temp/weibo/phantomjs-2.1.1-linux-x86_64/bin/phantomjs'
    )
    remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = ecap, port = 5555)
    Sys.sleep(10)
    remDr$open()
    on.exit(remDr$close())
    printlog('remDr is opened.')
    
    # navigate and login -----------------------------------------------------
    
    # remDr$navigate('https://weibo.com') # don't know why
    remDr$navigate('https://passport.weibo.cn/signin/login')
    login_webelem <- remDr$findElement('css selector', 'input[type="password"]')
    printlog(sprintf('login_webelem: %s', length(login_webelem)))
    if (unlist(login_webelem$getElementText()) == '') {
      
      remDr$findElement('css selector', 'input#loginName')$sendKeysToElement(list(''))
      remDr$findElement('css selector', 'input#loginPassword')$sendKeysToElement(list(''))
      remDr$findElement('css selector', '#loginAction')$clickElement()
    }
    printlog(sprintf('log in: %s', remDr$getCurrentUrl()[[1]]))
    
    remDr$getAllCookies() %>% {
      name <- unlist(map(., 'name'))
      value <- unlist(map(., 'value'))
      paste(name, value, sep = "=", collapse = ";")
    } %>% 
      writeLines(cookie)
    
  }, error = function(e) {
    printlog(sprintf('error: %s', e$message))
  })
  
}
