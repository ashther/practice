library(dplyr)
library(text2vec)
library(magrittr)
library(jsonlite)
library(widyr)
library(igraph)
library(tibble)
library(luzlogr)
library(stringi)
library(httr)
library(data.table)

LOG_PATH <- '/home/rstudio/log' # container path
WORK_PATH <- '/home/rstudio' # container path
HOST_PATH <- '/usr/local/xtde' # host path

THR_netGet <- 0.90
THR_genreGet <- 0.90

source(file.path(WORK_PATH, 'sna/sna.R'))
source(file.path(WORK_PATH, 'sna/netEvGet.R'))
source(file.path(WORK_PATH, 'sna/netDensityGet.R'))
source(file.path(WORK_PATH, 'topic_model/topic_model.R'))

# for sna
termVec <- readRDS(file.path(WORK_PATH, 'sna/rds/termVec.rds'))
termVecSentDT <- readRDS(file.path(WORK_PATH, 'sna/rds/termVecSentDT.rds'))
termVecSentMtx <- readRDS(file.path(WORK_PATH, 'sna/rds/termVecSentMtx.rds'))

# for topic_model
modelLdaList <- readRDS(file.path(WORK_PATH, 'topic_model/rds/modelLdaList.rds'))
vectorizer <- readRDS(file.path(WORK_PATH, 'topic_model/rds/vectorizer.rds'))

Rcpp::sourceCpp(file.path(WORK_PATH, 'sna/cosSimCaculateCpp.cpp'))

# reading data from sqlite db file for all module
dataGetFromSQLite <- function(db_path, sql) {
  require(RSQLite)
  require(DBI)
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))
  # dbListTables(con)
  # dbListFields(con, 'episode_actor')
  
  res <- dbSendQuery(con, sql)
  result <- dbFetch(res)
  dbClearResult(res)
  
  return(result)
}
