
# library(tidyverse)
# library(magrittr)
# library(modelr)
# 
# errFun <- function(model, test) {
#   model %>% 
#     predict(test) %>% 
#     # apply(1, which.max) %>% 
#     equals(test$Species) %>% 
#     mean()
# }
# 
# iris %>% 
#   crossv_kfold(k = 10) %>% 
#   mutate(train = map(train, as_tibble)) %>% 
#   mutate(model = map(train, 
#                      ~ randomForest::randomForest(
#                        Species ~ ., data = ., method = 'class'
#                      ))) %>% 
#   mutate(err = map2_dbl(model, train, errFun)) %>% 
#   select(.id, err)

# ============================

library(caret)
library(magrittr)

data(PimaIndiansDiabetes, package = 'mlbench')
data_all <- PimaIndiansDiabetes
# pregnant glucose pressure triceps insulin mass pedigree age diabetes
# 1        6     148       72      35       0 33.6    0.627  50      pos
# 2        1      85       66      29       0 26.6    0.351  31      neg
# 3        8     183       64       0       0 23.3    0.672  32      pos
# 4        1      89       66      23      94 28.1    0.167  21      neg
# 5        0     137       40      35     168 43.1    2.288  33      pos
# 6        5     116       74       0       0 25.6    0.201  30      neg

na_col <- 2:6
data_all <- data.frame(data_all[, c(1, 7, 8)],
                       lapply(data_all[, na_col], function(x) {
                         x[x == 0] <- NA
                         x
                       }),
                       diabetes = data_all[, 9])

tryCatch({
  nzv <- nearZeroVar(data_all)
  if (!identical(nzv, integer(0))) {
    data_all <- data_all[, -nzv]
  }
  cl <- findCorrelation(cor(data_all[, -ncol(data_all)]), 0.9)
  if (!identical(cl, integer(0))) {
    data_all <- data_all[, -cl]
  }
  lc <- findLinearCombos(data_all[, -ncol(data_all)])$remove
  if (!is.null(lc)) {
    data_all <- data_all[, -lc]
  }
}, error = function(e)e)

ctg <- colnames(data_all)[ncol(data_all)]
set.seed(1)
train_idx <- createDataPartition(data_all[[ctg]], p = 0.75, list = FALSE)
data_train <- data_all[train_idx, ]
data_test <- data_all[-train_idx, ]

preProc <- preProcess(data_all, 'knnImpute')
# preProc <- preProcess(data_all, c('center', 'scale'))
data_train <- predict(preProc, data_train)
data_test <- predict(preProc, data_test)

# prc <- prcomp(data_train[, -ncol(data_train)], center = TRUE, scale. = TRUE)
# data_train <- data.frame(prc$x[, 1:8], data_train[[ctg]])
# colnames(data_train)[9] <- ctg
# 
# data_test_temp <- predict(prc, data_test[, -ncol(data_test)])
# data_test <- data.frame(data_test_temp[, 1:8], data_test[[ctg]])
# colnames(data_test)[9] <- ctg


#length is = (n_repeats*nresampling)+1
seeds <- replicate(10 * 10, sample.int(10000, 10), simplify = FALSE)
seeds[[10 * 10 + 1]] <- sample.int(10000, 1)

ctrl <- trainControl(
  method = 'repeatedcv', 
  number = 10, 
  repeats = 10, 
  seeds = seeds, 
  verboseIter = TRUE, 
  classProbs = TRUE, 
  # summaryFunction = twoClassSummary, 
  search = 'random' # search = 'grid'
)

require(doParallel)
# such outfile for tracking training process in console
cl <- makeCluster(detectCores() - 1, outfile = '') 
registerDoParallel(cl)

# glmgrid <- expand.grid(alpha = seq(0, 1, 0.1), 
#                        lambda = seq(0, 5, 0.5))
glmFit <- train(
  as.formula(paste0(ctg, '~.')),
  # diabetes ~ pregnant + glucose + pressure + insulin + mass + pedigree, 
  data = data_train, 
  method = 'glmnet', 
  family = 'binomial', 
  trControl = ctrl, 
  # preProc = 'knnImpute', 
  # preProc = c('center', 'scale'), 
  # tuneGrid = glmgrid, 
  tuneLength = 10 
  # metric = 'ROC' 
  # na.action = na.pass
  # verbose = TRUE
)

# temp <- seq(0.5, 0.9, 0.04)
# rfGrid <- data.frame(temp, 1 - temp)
rfFit <- train(
  as.formula(paste0(ctg, '~.')), 
  data = data_train, 
  method = 'rf', 
  trControl = ctrl, 
  # preProc = 'knnImpute', 
  # preProc = c('center', 'scale'), 
  # tuneGrid = cutoff
  # cutoff = c(0.83, 0.17), 
  tuneLength = 10
  # metric = 'ROC'
  # na.action = na.pass
  # verbose = TRUE
)

# svmGrid <- expand.grid(C = seq(1, 10, 2), 
#                        sigma = seq(0.2, 2, 0.5))
svmFit <- train(
  as.formula(paste0(ctg, '~.')),
  data = data_train,
  method = 'svmRadial',
  trControl = ctrl,
  # preProc = 'knnImpute', 
  # preProc = c('center', 'scale'), 
  # tuneGrid = svmGrid,
  tuneLength = 10
  # metric = 'ROC' 
  # na.action = na.pass
  # verbose = TRUE
)


nnFit <- train(
  as.formula(paste0(ctg, '~.')),
  data = data_train,
  method = 'nnet', 
  trControl = ctrl, 
  # preProc = 'knnImpute', 
  # preProc = c('center', 'scale'), 
  tuneLength = 10
  # metric = 'ROC' 
  # na.action = na.pass
  # verbose = TRUE
)

gbmFit <- train(
  as.formula(paste0(ctg, '~.')),
  data = data_train,
  method = 'gbm', 
  trControl = ctrl, 
  # preProc = 'knnImpute', 
  # preProc = c('center', 'scale'), 
  tuneLength = 10 
  # metric = 'ROC' 
  # na.action = na.pass
  # verbose = TRUE
)

xgbFit <- train(
  as.formula(paste0(ctg, '~.')),
  data = data_train,
  method = 'xgbTree', 
  trControl = ctrl, 
  # preProc = 'knnImpute', 
  # preProc = c('center', 'scale'), 
  tuneLength = 10
  # metric = 'ROC' 
  # na.action = na.pass 
  # verbose = TRUE
)

stopImplicitCluster()
registerDoSEQ()

# fit_resamples <- resamples(list(glm = glmFit, rf = rfFit, svm = svmFit))
# bwplot(fit_resamples)

# rfConMtx <- rfFit %>% 
#   predict.train(newdata = data_test) %>% 
#   confusionMatrix(data_test[[ctg]])
# svmConMtx <- svmFit %>% 
#   predict.train(newdata = data_test) %>% 
#   confusionMatrix(data_test[[ctg]])
#   
#   
#   
# ==========================================================
# Confusion Matrix and Statistics
# 
# Reference
# Prediction neg pos
#        neg 121   7
#        pos   4  60
# 
# Accuracy : 0.9427          
# 95% CI : (0.8998, 0.9711)
# No Information Rate : 0.651           
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.8726          
# Mcnemar's Test P-Value : 0.5465          
#                                           
#             Sensitivity : 0.9680          
#             Specificity : 0.8955          
#          Pos Pred Value : 0.9453          
#          Neg Pred Value : 0.9375          
#              Prevalence : 0.6510          
#          Detection Rate : 0.6302          
#    Detection Prevalence : 0.6667          
#       Balanced Accuracy : 0.9318

save(data_train, data_test, 
     glmFit, rfFit, svmFit, nnFit, gbmFit, xgbFit,  
     file = paste0('models_', format(Sys.time(), '%Y%m%d_%H%M%S'), '.rda'))

# ===========================================================================
# rfgrid <- expand.grid(ntree = seq(500, 2000, 300), 
#                       cutoff = seq(0.5, 0.9, 0.1))
# 
# models <- pbapply(rfgrid, 1, function(param) {
#   set.seed(1)
#   model <- train(
#     as.formula(paste0(ctg, '~.')), 
#     data = data_train, 
#     method = 'rf', 
#     trControl = ctrl, 
#     preProc = c('center', 'scale'), 
#     ntree = param[1], 
#     cutoff = unname(c(param[2], 1 - param[2])), 
#     tuneLength = 5, 
#     metric = 'Spec', 
#     verbose = FALSE
#   )
#   return(list(param, model$results))
# })
# 
# data.frame(ntree = rep(unlist(map(map(models, 1), 'ntree')), each = 4), 
#            cutoff = rep(unlist(map(map(models, 1), 'cutoff')), each = 4), 
#            do.call(rbind, map(models, 2)))
