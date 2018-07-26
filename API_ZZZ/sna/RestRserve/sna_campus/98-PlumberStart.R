
library(plumber)
library(igraph)
library(dplyr)

if (!exists('graphCampus')) {
  graphCampus <- readRDS('rds/graphCampus.rds')
}
if (!exists('nodes')) {
  nodes <- readRDS('rds/nodes.rds')
}

r <- plumb('99-graph.R')
r$run(port = 8000)
