
salary_min <- 29
salary_max <- 85
dlt <- c(1, 2, 2, 3, 5) * 3
special <- c(56, 70, 85)

set.seed(1123)
salary <- c(8500,5400,3400,3200,3200,3200,5600,5100,3300,3200,4500,3200,3600,
            2800,4500,4000,3100,3300,3700,3500,5500,4400,3200,3200,4300,5600,
            3600,3300,3933,3500,
            4500,5700,4100,3100) / 100

dlts <- expand.grid(1:5, 1:5, 1:5, 1:5, 1:5) %>% 
  set_colnames(letters[1:5]) %>% 
  arrange(a, b, c, d, e) %>% 
  mutate(idx = b >= a & c >= b & d >= c & e >= d) %>% 
  filter(idx) %>% 
  select(-idx) %>% 
  multiply_by(3)

levelFind <- function(dlt, salary_min, salary_max) {
  salary_range <- (salary_min + dlt[1] + 1):(salary_max - dlt[length(dlt)] - 1)
  temp <- t(combn(salary_range, length(dlt) - 2))
  
  idx <- apply(temp, 1, function(x) {
    if (any((x + dlt[2:(length(dlt) - 1)]) >= c(x[-1], salary_max))) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  temp <- temp[idx, ]
  return(cbind(rep(salary_min, nrow(temp)), 
               temp, 
               rep(salary_max - dlt[length(dlt)], nrow(temp))))
}

allLevelsCreate <- function(vec, dlt) {
  sort(c(
    vec, 
    vec + dlt / 3, 
    vec + dlt * 2 / 3, 
    vec + dlt
  ))
}

prettyLevel <- function(vec, n = 4) {
  matrix(vec, ncol = n, byrow = TRUE, dimnames = list(NULL, LETTERS[1:n]))
}

upNearest <- function(salary, level) {
  require(magrittr)
  stopifnot(max(level) >= max(salary))
  
  sapply(salary, function(s) {
    level[which(level >= s)[1]]
  }) %>% 
    cbind(salary, salary_adj = .)
}

lossFunc <- function(salary_change_mtx) {
  not_change_id <- (nrow(salary_change_mtx) - 3):nrow(salary_change_mtx)
  salary_change_mtx[not_change_id, 2] <- salary_change_mtx[not_change_id, 1]
  temp <- colSums(salary_change_mtx)
  change_per <- unname((temp[2] - temp[1]) / temp[1])
  change_num <- sum(mapply(`!=`, salary_change_mtx[, 1], salary_change_mtx[, 2]))
  return(c(change_per, change_num))
}

minLossFind <- function(salary, salary_min, salary_max, dlt, special) {
  dlt <- unname(dlt)
  tryCatch({
    level <- levelFind(dlt, salary_min, salary_max)
    loss <- t(
      apply(level, 1, function(l) {
        levels <- allLevelsCreate(l, dlt)
        if (any(!special %in% levels)) {
          return(c(NA, NA))
        }
        lossFunc(upNearest(salary, levels))
      })
    )
    id_minPer <- which.min(loss[, 1])
    id_minNum <- which.min(loss[, 2])
    
    if (identical(id_minPer, integer(0)) & identical(id_minNum, integer(0))) {
      return(NULL)
    }
    
    return(list(
      byPer = list(
        levels = prettyLevel(allLevelsCreate(level[id_minPer, ], dlt)),  
        loss = scales::percent(loss[id_minPer, 1]), 
        num = loss[id_minPer, 2]
      ), 
      
      byNum = list(
        levels = prettyLevel(allLevelsCreate(level[id_minNum, ], dlt)), 
        loss = scales::percent(loss[id_minNum, 1]), 
        num = loss[id_minNum, 2]
      )
    ))
  }, error = function(e)return(NULL))
}

minLossFindFinal <- function(salary, salary_min, salary_max, dlts, special) {
  result <- pbapply::pbapply(
    dlts, 1, 
    minLossFind, salary = salary, salary_min = salary_min, 
    salary_max = salary_max, special = special
  )
  
  temp <- sapply(result, function(x) {
    if (is.null(x)) {
      return(c(NA, NA, NA, NA))
    }
    return(c(
      byPer_loss = as.numeric(sub('%', '', x$byPer$loss)), 
      byPer_num = x$byPer$num, 
      byNum_loss = as.numeric(sub('%', '', x$byNum$loss)), 
      byNum_num = x$byNum$num
    ))
  }) %>% 
    t()
  
  dt <- cbind(dlts, temp)
  dt$byPer_levels <- lapply(result, function(x) {
    if (is.null(x)) {
      return(NA)
    }
    x$byPer$levels
  })
  dt$byNum_levels <- lapply(result, function(x) {
    if (is.null(x)) {
      return(NA)
    }
    x$byNum$levels
  })
  
  list(
    byPer = arrange(dt, byPer_loss, byPer_num) %>% 
      select(a, b, c, d, e, starts_with('byPer')) %>% 
      head(), 
    
    byNum = arrange(dt, byNum_num, byNum_loss) %>% 
      select(a, b, c, d, e, starts_with('byNum')) %>% 
      head()
  )
}






















