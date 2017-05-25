library(igraph)
mat <- as.matrix(read.table(text=
                              "node X1 X2 X3 X4 X5 X6
                            1  0  3  7  4 NA NA
                            2  3  0  2 NA NA  9
                            3  7  2  0  1  3  6
                            4  4 NA  1  0  3 NA
                            5 NA NA  3  3  0  3
                            6 NA  9  6 NA  3  0", header=T))
rownames(mat) <- mat[,1]
mat <- mat[, -1]
colnames(mat) <- rownames(mat)
mat[is.na(mat)] <- 0
