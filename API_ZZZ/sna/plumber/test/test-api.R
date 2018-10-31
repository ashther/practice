
library(httr)
library(tidyverse)

config <- jsonlite::fromJSON('config.json')
PORT <- 8003

graphCollege <- readRDS(config$graphCollege_path)
graphMajor <- readRDS(config$graphMajor_path)
graphUser <- readRDS(config$graphUser_path)
allGraphCollegeCentr <- readRDS(config$allGraphCollegeCentr_path)
allGraphMajorCentr <- readRDS(config$allGraphMajorCentr_path)

college <- readRDS(config$college)
major <- readRDS(config$major)
user_major <- readRDS(config$user_major)

# con <- dbConnect(SQLite(), config$db_path)
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

graphAttrTest <- function(level, id = NULL) {
  res <- GET(sprintf('http://localhost:%s/sna/graph', PORT), query = list(
    level = level, id = id
  ))
  stop_for_status(res)
  g <- jsonlite::fromJSON(content(res, type = 'text', encoding = 'utf-8'))

  stopifnot(
    length(g) == 4,
    all(c('node_label', 'edge_list', 'graph_attr', 'node_attr') %in% names(g)),
    length(g$graph_attr) == 5,
    all(c('g_deg', 'g_clo', 'g_btw', 'g_eg', 'g_density') %in% names(g$graph_attr)),
    all(map_lgl(g$graph_attr, ~ is.null(.x) || (is.numeric(.x) && between(.x, 0, 1)))),
    all(c('degree', 'closeness', 'betweenness', 'eigenvector') %in% names(g$node_attr))
  )
}

# test allGraphAttr endpoint ---------------------------------------------

temp <- GET(sprintf('localhost:%s/sna/allGraphAttr/college', PORT))
stop_for_status(temp)
temp <- content(temp)
stopifnot(
  length(temp) == 5,
  all(c('g_deg', 'g_clo', 'g_btw', 'g_eg', 'g_density') %in% names(temp)),
  all(map_lgl(temp, ~ is.numeric(.x) & between(.x, 0, 1)))
)

temp <- GET(sprintf('localhost:%s/sna/allGraphAttr/major', PORT))
stop_for_status(temp)
temp <- content(temp)
stopifnot(
  length(temp) == 5,
  all(c('g_deg', 'g_clo', 'g_btw', 'g_eg', 'g_density') %in% names(temp)),
  all(map_lgl(temp, ~ is.numeric(.x) & between(.x, 0, 1)))
)


# test graph endpoint ----------------------------------------------------

# test parameters thr
temp <- GET(sprintf('http://localhost:%s/sna/graph', PORT), query = list(
  level = 'all', thr = 'test', top = 5
))
stopifnot(identical(status_code(temp), 400L))
temp <- content(temp)
stopifnot(identical(temp$error, '!is.na(thr) & thr > 0 & thr < 1 is not TRUE'))

# test parameters top
temp <- GET(sprintf('http://localhost:%s/sna/graph', PORT), query = list(
  level = 'all', thr = 0.95, top = 'test'
))
stopifnot(identical(status_code(temp), 400L))
temp <- content(temp)
stopifnot(identical(temp$error, '!is.na(top) & top > 0 is not TRUE'))

# when level == 'all'
graphAttrTest('all')

# when level == 'college'
print('college api test')
pb <- progress_estimated(nrow(college))
walk(college$id, ~ tryCatch({
  graphAttrTest('college', .x)
  pb$tick()$print()
}, error = function(e) {
  print(sprintf(
    'college id: %s -- error: %s', .x, e$message
  ))
}))

# when level == 'major'
print('major api test')
pb <- progress_estimated(nrow(major))
walk(major$id, ~ tryCatch({
  graphAttrTest('major', .x)
  pb$tick()$print()
}, error = function(e) {
  print(sprintf(
    'major id: %s -- error: %s', .x, e$message
  ))
}))

# when level == 'ego'
print('ego api test')
pb <- progress_estimated(200)
walk(sample(user_major$id, 200), ~ tryCatch({
  temp <- GET(sprintf('http://localhost:%s/sna/graph', PORT), query = list(
    level = 'ego', id = .x
  ))
  if (status_code(temp) != 200L) {
    identical(content(temp)$error,
              sprintf("the user %s hasn't a ego social network!", .x))
  } else {
    graphAttrTest('ego', .x)
  }
  pb$tick()$print()

}, error = function(e) {
  print(sprintf(
    'ego id: %s -- error: %s', .x, e$message
  ))
}))
