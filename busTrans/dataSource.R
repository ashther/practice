
library(igraph)
matTime <- as.matrix(read.table(text=
                              "node X1 X2 X3 X4 X5 X6
                            1  0  3  7  4 NA NA
                            2  3  0  2 NA NA  9
                            3  7  2  0  1  3  6
                            4  4 NA  1  0  3 NA
                            5 NA NA  3  3  0  3
                            6 NA  9  6 NA  3  0", header=T))
rownames(matTime) <- matTime[,1]
matTime <- matTime[, -1]
colnames(matTime) <- rownames(matTime)
matTime[is.na(matTime)] <- 0

mat <- matTime
mat[mat > 0] <- 1

matTrans <- matTime
matTrans <- matTrans * 0
matTrans[rbind(c('4', '3'), c('5', '6'))] <- 1
