library(text2vec)
library(dplyr)

graphRead <- function(graph_path, weight = FALSE, directed = FALSE) {
  net <- read.table(graph_path, sep = ' ')
  edgelist <- net[, 1:2, drop = FALSE]
  weights <- rep(1, nrow(net))
  
  if (weight & ncol(net) > 2) {
    weights <- net[, 3]
  }
  
  if (!directed) {
    edgelist <- rbind(edgelist, cbind(edgelist[, 2], edgelist[, 1]))
    weights <- c(weights, weights)
    idx <- !duplicated(edgelist)
    edgelist <- edgelist[idx, , drop = FALSE]
    weights <- weights[idx]
  }
  
  setNames(lapply(unique(edgelist[, 1]), function(x) {
    nbrs <- edgelist[edgelist[, 1] == x, 2]
    wts <- weights[edgelist[, 1] == x & edgelist[, 2] %in% nbrs]
    setNames(wts, nbrs)
  }), unique(edgelist[, 1]))
}

walkSimulate <- function(edgelist, num_walks = 10, walk_length = 80) {
  nodes <- names(edgelist)
  do.call(`c`, pbapply::pblapply(seq_len(num_walks), function(x) {
    nodes <- sample(nodes)
    lapply(nodes, walkNode2Vec, edgelist = edgelist, alias = alias, walk_length = walk_length)
  }))
}

walkNode2Vec <- function(start_node, edgelist, alias, walk_length) {
  alias_nodes <- alias$alias_nodes
  alias_edges <- alias$alias_edges
  
  walk <- start_node
  while (length(walk) < walk_length) {
    # print(length(walk))
    cur <- walk[length(walk)]
    cur_nbrs <- names(edgelist[[cur]])
    if (length(cur_nbrs) > 0) {
      if (length(walk) == 1) {
        walk <- c(
          walk, 
          cur_nbrs[aliasDraw(alias_nodes[[cur]]$j, alias_nodes[[cur]]$q)]
        )
      } else {
        prev <- walk[length(walk) - 1]
        walk <- c(walk, cur_nbrs[aliasDraw(
          alias_edges[[prev]][[cur]]$j, 
          alias_edges[[prev]][[cur]]$q
        )])
      }
    } else {
      break
    }
  }
  
  return(walk)
}

aliasDraw <- function(j, q) {
  idx <- ceiling(runif(1) * length(j)) # different from py version
  if (runif(1) < q[idx]) idx else j[idx]
}

probsTrans <- function(edgelist, p, q) {
  alias_nodes <- setNames(lapply(names(edgelist), function(x) {
    normalized_probs <- prop.table(edgelist[[x]])
    aliasSetup(normalized_probs)
  }), names(edgelist))
  
  alias_edges <- setNames(lapply(names(edgelist), function(src) {
    setNames(lapply(names(edgelist[[src]]), function(tar) {
      tryCatch(aliasEdgeGet(edgelist, src, tar, p = p, q = q), 
               error = function(e)print(paste0(src, ', ', tar)))
    }), names(edgelist[[src]]))
  }), names(edgelist))
  
  return(list(alias_nodes = alias_nodes, 
              alias_edges = alias_edges))
}

aliasSetup <- function(normalized_probs) {
  K <- length(normalized_probs)
  q <- K * normalized_probs
  j <- rep(0, K)
  smaller <- which(q < 1.0)
  larger <- which(q >= 1.0)
  
  i <- 0
  while (length(smaller) > 0 & length(larger) > 0) {
    small <- smaller[length(smaller)]
    smaller <- smaller[-length(smaller)]
    large <- larger[length(larger)]
    larger <- larger[-length(larger)]
    
    j[small] <- large
    q[large] <- q[large] + q[small] - 1.0
    
    if (q[large] < 1.0) {
      smaller <- c(smaller, large)
    } else {
      larger <- c(larger, large)
    }
  }
  
  return(list(j = j, q = q))
}

aliasEdgeGet <- function(edgelist, src, tar, p = 1, q = 1) {
  normalized_probs <- edgelist[[tar]]
  idx <- names(normalized_probs) == src
  normalized_probs[idx] <- normalized_probs[idx] / p
  if (length(normalized_probs[!idx]) > 0) {
    normalized_probs[!idx] <- sapply(names(normalized_probs)[!idx], function(x) {
      if (src %in% names(edgelist[[x]])) {
        normalized_probs[x]
      } else {
        normalized_probs[x] / q
      }
    })
  }
  
  normalized_probs <- prop.table(unname(normalized_probs))
  return(aliasSetup(normalized_probs))
}

walks2vec <- function(walks) {
  it <- itoken(walks, progressbar = TRUE)
  v <- create_vocabulary(it) %>% 
    prune_vocabulary(term_count_min = 0L)
  tcm <- create_tcm(it, vocab_vectorizer(v), skip_grams_window = 10L)
  glove <- GlobalVectors$new(word_vectors_size = 50, 
                             vocabulary = v, 
                             x_max = 10)
  wv_main <- glove$fit_transform(tcm, n_iter = 20, convergence_tol = 0.001)
  termVec <- wv_main + t(glove$components)
  
  return(termVec)
}
