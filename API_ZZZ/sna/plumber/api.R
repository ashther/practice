
# find where script is and get config ------------------------------------

isInDocker <- function() {
  group_info <- system('cat /proc/1/cgroup', intern = TRUE)
  any(grepl('docker', group_info)) | file.exists('/.dockerenv')
}

if (isTRUE(isInDocker())) {
  HOME_PATH <- '/home/rstudio'
} else {
  HOME_PATH <- '/home/ashther/user_profile_sna'
}
config <- jsonlite::fromJSON(file.path(HOME_PATH, 'config.json'))

# log config and log function --------------------------------------------

LOG_PATH <- file.path(HOME_PATH, 'logger')
LOG_LEVEL <- eval(parse(text = paste0('futile.logger::', config$log_level)))
futile.logger::flog.threshold(LOG_LEVEL)
futile.logger::flog.appender(futile.logger::appender.file(LOG_PATH),
                             name = 'api_upsna')
layout_custom <- futile.logger::layout.format(
  '~l [~t] ~m', datetime.fmt = '%Y-%m-%d %H:%M:%OS3'
)
futile.logger::flog.layout(layout_custom, name = 'api_upsna')

res_logger <- function(req, res, msg = NULL) {
  if (as.integer(res$status) == 200) {
    futile.logger::flog.info(
      '{"uuid":"%s","status":"%s","msg":"response successfully"}',
      req$cookies$uuid,
      res$status,
      name = 'api_upsna'
    )
    # plumber::forward()
  } else {
    futile.logger::flog.error(
      '{"uuid":"%s","status":"%s","msg":"%s"}',
      req$cookies$uuid,
      res$status,
      msg,
      name = 'api_upsna'
    )
    # plumber::forward()
  }
}


# get constant and object ------------------------------------------------


graphCollege <- readRDS(file.path(HOME_PATH, config$graphCollege_path))
graphMajor <- readRDS(file.path(HOME_PATH, config$graphMajor_path))
graphUser <- readRDS(file.path(HOME_PATH, config$graphUser_path))
allGraphCollegeCentr <- readRDS(file.path(HOME_PATH, config$allGraphCollegeCentr_path))
allGraphMajorCentr <- readRDS(file.path(HOME_PATH, config$allGraphMajorCentr_path))

college <- readRDS(file.path(HOME_PATH, config$college))
major <- readRDS(file.path(HOME_PATH, config$major))
user_major <- readRDS(file.path(HOME_PATH, config$user_major))

source(file.path(HOME_PATH, 'function.R'), local = TRUE)

# endpoint and filter ----------------------------------------------------

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "PUT, GET, POST, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Origin, Authorization, Accept, Content-Type")
  res$setHeader("Access-Control-Allow-Credentials", "true")
  plumber::forward()
}

#* @filter req logger
function(req) {

  if (req$REQUEST_METHOD %in% c('GET', 'DELETE', 'OPTIONS')) {
    params <- req$QUERY_STRING
  } else if (req$REQUEST_METHOD == c('POST', 'PUT')) {
    params <- req$postBody
  } else {
    params <- NULL
  }

  req$cookies$uuid <- uuid::UUIDgenerate(TRUE)

  futile.logger::flog.info(
    '{"uuid":"%s","addr":"%s","server":"%s","port":"%s","path":"%s","method":"%s","params":"%s","headers":{%s}}',
    req$cookies$uuid,
    req$REMOTE_ADDR,
    req$SERVER_NAME,
    req$SERVER_PORT,
    req$PATH_INFO,
    req$REQUEST_METHOD,
    params,
    paste0(sprintf('"%s":"%s"', names(req$HEADERS), req$HEADERS),
           collapse = ','),
    name = 'api_upsna'
  )
  plumber::forward()
}

#* @filter auth
function(req, res) {
  if (req$REQUEST_METHOD == 'OPTIONS' | req$PATH_INFO == '/__swagger__/' | 
      req$PATH_INFO == '/swagger.json') {
    plumber::forward()
  } else {
    tryCatch({
      
      auth <- req$HTTP_AUTHORIZATION
      if (length(auth) == 0) stop('no token')
      auth <- gsub('^Basic ', '', auth)
      token <- unlist(strsplit(
        rawToChar(jose::base64url_decode(auth)), ':', fixed = TRUE
      ))[1]
      jose::jwt_decode_hmac(token, config$secret_key)
      
      jwt_header <- unlist(strsplit(token, '.', fixed = TRUE))[1]
      jwt_header <- rawToChar(jose::base64url_decode(jwt_header))
      
      exp_time <- jsonlite::fromJSON(jwt_header)$exp
      exp_time <- as.POSIXct(exp_time, origin = '1970-01-01')
      
      if (exp_time >= Sys.time()) {
        res$status <- 200L
        plumber::forward()
      } else {
        stop('expired token')
      }
      
    }, error = function(e) {
      res$status <- 403L
      res_logger(req, res, e$message)
      'Unauthorized Access'
    })
  }
}

get_group <- function(req) {
  auth <- gsub('^Basic ', '', req$HTTP_AUTHORIZATION)
  token <- unlist(strsplit(
    rawToChar(jose::base64url_decode(auth)), ':', fixed = TRUE
  ))[1]
  payload <- jose::jwt_decode_hmac(token, config$secret_key)
  
  payload[c('group', 'group_id')]
}

group_auth_verify <- function(payload, level, id) {
  group <- payload$group
  group_id <- payload$group_id
  
  if (group == 'all') {
    invisible()
  } else if (group == 'college') { # college user query
    major_id <- major$id[major$college_id == group_id]
    user_major_id <- user_major$major_id[user_major$id == id]
    
    if (level == 'college' & group_id == id) {
      invisible()
    } else if (level == 'major' & 
               ifelse(length(major_id) == 0, FALSE, id %in% major_id)) {
      invisible()
    } else if (level == 'ego' & 
               ifelse(length(major_id) == 0 | length(user_major_id) == 0, 
                      FALSE, user_major_id %in% major_id)) {
      invisible()
    } else {
      stop('Unauthorized Data Query')
    }
  } else if (group == 'major') { # major user query
    user_major_id <- user_major$major_id[user_major$id == id]
    
    if (level == 'major' & group_id == id) {
      invisible()
    } else if (level == 'ego' & 
               ifelse(length(user_major_id) == 0, FALSE, user_major_id == group_id)) {
      invisible()
    } else {
      stop('Unauthorized Data Query')
    }
  } else if (group == 'ego') {
    if (level == 'ego' & group_id == id) {
      invisible()
    } else {
      stop('Unauthorized Data Query')
    }
  } else {
    stop('Unauthorized Data Query')
  }
}

#* @apiTitle 
#* @apiDescription 


#* @get /sna/allGraphAttr/<level>
#* @options /sna/allGraphAttr/<level>
#* @serializer unboxedJSON
function(level, req, res) {
  tryCatch({

    stopifnot(level %in% c('college', 'major'))

    if (level == 'college') {
      result <- graphCollegeAvgCentr()
    } else {
      result <- graphMajorAvgCentr()
    }
    res_logger(req, res)

    return(result)

  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    e$message
  })
}


#* @param level:character 
#* @param id:int 
#* @param thr:numeric 
#* @param top:int 
#* @get /sna/graph
#* @options /sna/graph
#* @serializer unboxedJSON
function(level, id, thr, top, req, res) {
  tryCatch({
    # valid parameters
    stopifnot(level %in% c('all', 'college', 'major', 'ego'))

    if (level != 'all') {
      id <- as.character(id)
      stopifnot(!is.na(id))
    }
    # group auth verify
    if (req$REQUEST_METHOD != 'OPTIONS') {
      payload <- get_group(req)
      id <- ifelse(missing(id), '0', id)
      group_auth_verify(payload, level, id)
    }

    if (!missing(thr)) {
      thr <- as.numeric(thr)
      stopifnot(!is.na(thr) & thr > 0 & thr < 1)
    } else {
      thr <- 0.9
    }

    if (!missing(top)) {
      top <- as.integer(top)
      stopifnot(!is.na(top) & top > 0)
    } else {
      top <- 10
    }

    # get the graph and attributes
    temp <- graphSelect(level, id, thr)
    g <- temp$g

    result <- list(
      node_label = temp$node_label,
      edge_list = edgeListGet(g),
      graph_attr = graphAttrGet(g),
      node_attr = nodeAttrGet(g, temp$node_label, top)
    )

    res_logger(req, res)
    return(result)

  }, error = function(e) {
    if (e$message == 'Unauthorized Data Query') {
      res$status <- 403L
    } else {
      res$status <- 400L
    }
    res_logger(req, res, e$message)
    list(message = e$message)
  })
}
