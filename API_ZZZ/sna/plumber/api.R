
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
    plumber::forward()
  } else {
    futile.logger::flog.error(
      '{"uuid":"%s","status":"%s","msg":"%s"}',
      req$cookies$uuid,
      res$status,
      msg,
      name = 'api_upsna'
    )
    plumber::forward()
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

# library(RSQLite)
# con <- dbConnect(SQLite(), file.path(HOME_PATH, config$db_path))
# college <- dbGetQuery(con, "
#                       SELECT item_code AS id,
#                              item_name AS name
#                       FROM dict
#                       WHERE type_code = 'college';")
# major <- dbGetQuery(con, "
#                       SELECT item_code AS id,
#                              item_name AS name,
#                              parent_id  AS college_id
#                       FROM dict
#                       WHERE type_code = 'major';")
# user_major <- dbGetQuery(con, "
#                          SELECT accnum AS id,
#                                 name || ' ' || percode AS name,
#                                 major_id
#                          FROM account;")
# dbDisconnect(con)

source(file.path(HOME_PATH, 'function.R'), local = TRUE)

# endpoint and filter ----------------------------------------------------

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @filter req logger
function(req) {

  if (req$REQUEST_METHOD == 'GET') {
    params <- req$QUERY_STRING
  } else if (req$REQUEST_METHOD == 'POST') {
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

#* @apiTitle 
#* @apiDescription 


#* @get /sna/allGraphAttr/<level>
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
#* @serializer unboxedJSON
function(level, id, thr, top, req, res) {
  tryCatch({
    # valid parameters
    stopifnot(level %in% c('all', 'college', 'major', 'ego'))

    if (level != 'all') {
      id <- as.character(id)
      stopifnot(!is.na(id))
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
    res$status <- 400L
    res_logger(req, res, e$message)
    list(error = e$message)
  })
}


#* @get /selection/<level>
function(level, req, res) {
  tryCatch({
    # valid parameters
    # stopifnot(level %in% c('college', 'major', 'ego'))
    
    if (level == 'college') {
      result <- college
    } else if (level == 'major') {
      result <- major[major$id %in% user_major$major_id, c('id', 'name')]
    } else if (level == 'degree') {
      result <- c('全部', unique(user_major$degree))
    } else if (level == 'sex') {
      result <- c('全部', unique(user_major$sex))
    } else if (level == 'area') {
      result <- c('全部', unique(user_major$area))
    } else if (level == 'yearIn') {
      result <- c('全部', sort(as.integer(unique(user_major$yearIn)), 
                             decreasing = TRUE))
    } else {
      stop('not correct selection type')
    }
    
    res_logger(req, res)
    return(result)
    
  }, error = function(e) {
    res$status <- 400L
    res_logger(req, res, e$message)
    list(error = e$message)
  })
}
