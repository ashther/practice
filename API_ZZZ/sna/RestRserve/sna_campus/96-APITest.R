
library(httr)
library(jsonlite)
library(tidyverse)

nodes <- read_rds('rds/nodes.rds')

# function test ----------------------------------------------------------

get_custom <- function(id, is_dep) {
  result <- GET(sprintf('localhost:8000/graph?id=%s&is_dep=%s', id, is_dep))
  list(status_code = status_code(result), content = content(result))
}
get_custom_safely <- safely(get_custom)



# call API and get dep graph ---------------------------------------------
# get all dep graph looping all unique ACCDPEID in nodes dataset
iter <- unique(nodes$ACCDEPID)
pb <- progress_estimated(length(iter))
result <- map(iter, ~ {
  pb$tick()$print()
  get_custom_safely(.x, 'TRUE')
}) %>% 
  transpose()

errors <- unlist(result$error)
status_codes <- map_int(result$result, 'status_code')
contents <- map(result$result, 'content')

idx_not_200 <- status_codes != 200
contents_not_200 <- bind_rows(contents[idx_not_200])


# call API and get individual graph --------------------------------------
# due to too much calculation time, only test nodes with 0 degree
id_degree_0 <- map(contents[!idx_not_200], ~ {
  tryCatch({
    keep(.x$node_attr, function(x) {
      x$degree == 0
    }) %>% 
      transpose() %>% 
      .$name %>% 
      unlist()
  }, error = function(e) {
    print(.x)
    stop()
  })
}) %>% 
  unlist()

# get all ego graph looping all 0 degree nodes
iter <- id_degree_0
pb <- progress_estimated(length(iter))
result <- map(iter, ~ {
  pb$tick()$print()
  temp <- get_custom_safely(.x, 'FALSE')
  res <- list(
    error = temp$error, 
    status_code = temp$result$status_code, 
    content = ''
  )
  if (res$status_code != 200) {
    res$content <- temp$result$content
  }
  res
}) %>% 
  transpose()

errors <- unlist(result$error)
status_codes <- unlist(result$status_code)
contents <- result$content %>% 
  setNames(iter) %>% 
  keep(~ length(.x) > 1) %>% {
    nm <- names(.)
    bind_rows(.) %>% 
      add_column(id = nm)
  }


# performance test -------------------------------------------------------
# https://github.com/apigee/apib
library(glue)
concurrent_seq <- 1:25
duration <- 20


col_names <- system('apib -T', intern = TRUE) %>% 
  str_split(',') %>% 
  unlist()

# pb <- progress_estimated(length(concurrent_seq))
result <- map_dfr(concurrent_seq, ~ {
  tryCatch({
    
    RestRserve::restrserve_start('RestRserve/')
    Sys.sleep(5)
    
    warm_up <- 5 + .x * 2
    apib_command <- glue(
      "apib -w {warm_up} -c {.x} -d {duration} -S -N {.x} \\
  'http://127.0.0.1:8000/graph?id=54&is_dep=TRUE'"
    )
    cat(sprintf('[%s] %s\n', as.character(Sys.time()), apib_command))
    # pb$tick()$print()
    
    temp <- system(apib_command, intern = TRUE)
    cat(sprintf('[%s] %s\n', as.character(Sys.time()), temp))
    RestRserve::restrserve_stop('RestRserve/')
    
    temp %>% 
      str_split(',') %>% 
      unlist() %>% 
      t() %>% 
      as_tibble()
    
  }, error = function(e) {
    cat(sprintf('[%s] .x: %s, error: %s', as.character(Sys.time()), .x, e$message))
    tibble()
  })
})
result <- setNames(result, col_names)

normalize <- function(vec) {
  tryCatch({
    (vec - min(vec)) / (max(vec) - min(vec))
  }, error = function(e) {
    cat(sprintf('error in normalization: %s', e$message))
    rep(NA, length(vec))
  })
}

ggthemr::ggthemr('flat')
select(result, Connections, Throughput, `Avg. Latency`, `Client Mem Usage`) %>% 
  mutate_all(as.numeric) %>% 
  mutate(tps = Throughput / max(Throughput),
         latency = `Avg. Latency` / max(`Avg. Latency`),
         mem = `Client Mem Usage` / 100) %>% 
  # mutate_at(.vars = vars(-Connections), .funs = normalize) %>%
  # gather(item, value, -Connections) %>% 
  gather(item, value, tps, latency, mem) %>%
  ggplot(aes(Connections, value, group = item, color = item)) + 
  geom_point(alpha = 0.3) + 
  geom_line(alpha = 0.3) + 
  geom_smooth(se = FALSE, alpha = 0.1, size = 1.5)