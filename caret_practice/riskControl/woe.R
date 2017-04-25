
library(caret)
library(dplyr)
library(magrittr)
library(woe)

# kaggle_train_data <- read.csv('kaggle_example/cs-training.csv')
# kaggle_train_data <- select(kaggle_train_data, -1) %>% 
#   rename(y = SeriousDlqin2yrs, 
#          x1 = RevolvingUtilizationOfUnsecuredLines, 
#          x2 = age, 
#          x3 = NumberOfTime30.59DaysPastDueNotWorse, 
#          x4 = DebtRatio, 
#          x5 = MonthlyIncome, 
#          x6 = NumberOfOpenCreditLinesAndLoans, 
#          x7 = NumberOfTimes90DaysLate, 
#          x8 = NumberRealEstateLoansOrLines, 
#          x9 = NumberOfTime60.89DaysPastDueNotWorse, 
#          x10 = NumberOfDependents)
# kaggle_train_data <- knnImputation(kaggle_train_data) # too slow
# kaggle_train_data <- kaggle_train_data %>% 
#   filter(x2 > 0 & 
#            !x3 %in% c(96, 98) & 
#            x6 < 30 & 
#            !x7 %in% c(96, 98) & 
#            x8 < 4 & 
#            !x9 %in% c(96, 98) & 
#            x10 <= 5)

# kaggle_train_data$test <- 1 - kaggle_train_data$test
# colnames(kaggle_train_data)[15] <- 'test'
iv <- iv.mult(kaggle_train_data, 'score', summary = TRUE, verbose = TRUE)
# iv_variable <- iv$Variable[iv$Strength != 'Wery weak']
iv_variable <- iv$Variable[iv$InformationValue >= 0.01 & 
                             !iv$Variable %in% c('info', 'status')]
data_woe <- iv.mult(kaggle_train_data, 'score', vars = iv_variable, verbose = TRUE)
names(data_woe) <- iv_variable

# replace original data with woe value
kaggle_train_data <- iv.replace.woe(kaggle_train_data, 
                           iv.mult(kaggle_train_data, 'score', 
                                   vars = iv_variable),
                           verbose = TRUE) %>% 
  select(score, ends_with('woe'))

colnames(kaggle_train_data)[which(colnames(kaggle_train_data) == 'score')] <- 'y'
colnames(kaggle_train_data) <- gsub('_woe', '', colnames(kaggle_train_data))
# it looks like smote need factor y
kaggle_train_data$y <- factor(kaggle_train_data$y,
                              labels = c('bad', 'good'))

y_idx <- which(colnames(kaggle_train_data) == 'y')
df_colname <- colnames(kaggle_train_data[, -y_idx])
cl <- findCorrelation(cor(kaggle_train_data[, -y_idx]), 0.8)
if (!identical(cl, integer(0))) {
  kaggle_train_data <- kaggle_train_data[, -which(colnames(kaggle_train_data) %in% 
                                                    df_colname[cl])]
}
lc <- findLinearCombos(kaggle_train_data[, -y_idx])$remove
if (!is.null(lc)) {
  kaggle_train_data <- kaggle_train_data[, -lc]
}

corrplot::corrplot(cor(kaggle_train_data[, -y_idx]), 'number')

set.seed(2017)
idx <- createDataPartition(kaggle_train_data$y, p = 0.8, list = FALSE)
data_train <- kaggle_train_data[idx, ]
data_test <- kaggle_train_data[-idx, ]

data_train_smote <- DMwR::SMOTE(y ~ ., data_train)
data_train_down <- downSample(data_train[, -1], data_train$y)
data_train_up <- upSample(data_train[, -1], data_train$y)
data_train_rose <- ROSE::ROSE(y ~ ., data = data_train)$data

# 10-fold glm method ===================================================
ctrl <- trainControl(
  method = 'cv',
  number = 10,
  # repeats = 10,
  verboseIter = TRUE,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  search = 'random'
)

fitCaretSmote <- train(y ~ ., 
                       data = data_train_smote, method = 'glm', family = binomial(),
                       trControl = ctrl, metric = 'ROC')
fitCaretRose <- train(y ~ ., 
                      data = data_train_rose, method = 'glm', family = binomial(),
                      trControl = ctrl, metric = 'ROC')
fitCaretUp <- train(Class ~ ., 
                    data = data_train_up, method = 'glm', family = binomial(),
                    trControl = ctrl, metric = 'ROC')
fitCaretDown <- train(Class ~ ., 
                      data = data_train_down, method = 'glm', family = binomial(),
                      trControl = ctrl, metric = 'ROC')

models <- list(smote = fitCaretSmote, 
               rose = fitCaretRose, 
               up = fitCaretUp, 
               down = fitCaretDown)
# rm(fitCaretSmote, fitCaretRose, fitCaretUp, fitCaretDown)
resamps <- resamples(models)
# rm(models)
summary(resamps) %>% {
  cbind(
    .$statistics$ROC[, 'Mean'], 
    .$statistics$Sens[, 'Mean'], 
    .$statistics$Spec[, 'Mean']
  )
} %>% 
  set_colnames(c('roc', 'sens', 'spec'))
#         roc   sens   spec
# smote 0.8176 0.9036 0.5611
# rose  0.7737 0.8290 0.6481
# up    0.8200 0.8727 0.6254
# down  0.8190 0.8667 0.6263

pbapply::pbsapply(models, function(m) {
  temp <- confusionMatrix(predict(m, data_test), data_test$y)
  accu <- temp$overall['Accuracy']
  sens <- temp$byClass['Sensitivity']
  spec <- temp$byClass['Specificity']
  auc <- pROC::auc(data_test$y, predict(m, data_test, 'prob')[, 'bad'])
  return(c(auc = auc, sens, spec, accu))
}) %>% 
  t()
#             auc Sensitivity Specificity  Accuracy
# smote 0.8205537   0.9060043   0.5664894 0.8840654
# rose  0.8149890   0.8581980   0.6308511 0.8435073
# up    0.8205043   0.8746968   0.6250000 0.8585619
# down  0.8200060   0.8723451   0.6276596 0.8565340

# hold-out glm method ===================================================
fit_test <- glm(Class ~ ., family = binomial(), data = data_train_up)
summary(fit_test)
rm(fit_test)

fit <- glm(Class ~ x1_woe + x2_woe + x3_woe + x4_woe + x5_woe + x6_woe + x10_woe, 
           family = binomial(), 
           data = data_train_up)
summary(fit)

pre <- predict(fit, data_test, 'response')
pROC::auc(data_test$y, pre)
pre_label <- ifelse(pre > 0.4524044, 'good', 'bad')
confusionMatrix(pre_label, data_test$y, positive = 'good')

# Area under the curve: 0.8206 smote
# Call:
# glm(formula = y ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, 
#     family = binomial(), data = data_train_smote)
#     
# Area under the curve: 0.815 rose
# Call:
# glm(formula = y ~ x2 + x3 + x4 + x5 + x7 + x8 + x9 + x10, family = binomial(), 
#     data = data_train_rose)

# Area under the curve: 0.818 down
# Call:
# glm(formula = Class ~ x2 + x3 + x7 + x9 + x10, family = binomial(), 
#     data = data_train_down)
# 
# Area under the curve: 0.8205 up
# Call:
# glm(formula = Class ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + 
#       x10, family = binomial(), data = data_train_up)

# get best cutoff ====================================================
df_temp <- data.frame(actual = data_test$y,
                      predict = unname(predict(fitCaretSmote, data_test, 'prob')['good']))
source('ROCInfo.R')
roc_info <- ROCInfo(df_temp, 'predict', 'actual', cost.fp = 4, cost.fn = 1)
grid.draw(roc_info$plot)

pre <- predict(fitCaretSmote, data_test, 'prob')[, 'good']
pROC::auc(data_test$y, pre)
pre_label <- ifelse(pre > roc_info$cutoff, 'good', 'bad')
confusionMatrix(pre_label, data_test$y, positive = 'good')
# $plot
# TableGrob (2 x 2) "arrange": 3 grobs
# z     cells    name                grob
# 1 1 (2-2,1-1) arrange      gtable[layout]
# 2 2 (2-2,2-2) arrange      gtable[layout]
# 3 3 (1-1,1-2) arrange text[GRID.text.379]
# 
# $cutoff
# [1] 0.4193423
# 
# $totalcost
# [1] 42650
# 
# $auc
# [1] 0.7018072
# 
# $sensitivity
# [1] 0.6636066
# 
# $specificity
# [1] 0.6653543

# get score ===========================================================
coe <- fitCaretSmote$finalModel$coefficients

# 60 = a + b * (-log(1/0.5 - 1))
# 100 = a + b * (-log(1/0.99 - 1))
# total_score = base_score + field_score
b <- (100 - 60) / (-log(1/0.92 - 1) + log(1/roc_info$cutoff - 1))
a <- 100 - b * (-log(1/0.92 - 1))
base_score <- a + b * coe[1]
field_score <- lapply(names(data_woe), function(w) {
  data_woe[[w]] %>% 
    select(variable, class, woe) %>% 
    mutate(score = woe * coe[w] * b)
})

# the range of total_score
sapply(field_score, FUN = function(x)if(any(!is.na(x$score))){min(x$score)}else{0}) %>% 
  sum() %>% 
  add(base_score)
# score = base_score + weight
# 
# [[1]]
# variable    class         woe     score
# 1       x2 (;68.94)  0.09772944 -0.659046
# 2       x2 <68.94;) -0.43412870  2.927580
# 
# [[2]]
# variable class          woe        score
# 1        x3  6101 -0.492910613  8.129441219
# 2        x3  6102  0.242553020 -4.000361245
# 3        x3  6103  0.218449310 -3.602825298
# 4        x3  6104 -0.038133487  0.628925271
# 5        x3  6105 -0.285354141  4.706268552
# 6        x3  6106 -0.000393159  0.006484266
# 7        x3  6107  0.187348465 -3.089887485
# 8        x3  6108 -0.000393159  0.006484266
# 9        x3  6109 -0.011442995  0.188726220
# 10       x3  6110  0.073714813 -1.215758443
# 11       x3 other  0.245832200 -4.054443875
# 
# [[4]]
# variable  class         woe      score
# 1       x7 (;0.5) -0.58915401 -0.9947924
# 2       x7 <0.5;)  0.06696251  0.1130669
# 
# [[5]]
# variable class         woe      score
# 1       x8     A -0.04014633   0.501923
# 2       x8     G -1.15438759  14.432543
# 3       x8     H  1.49760519 -18.723565
# 4       x8 other  1.23479466 -15.437819
# 5       x8     R -2.24728935  28.096369
# 
# [[6]]
# variable  class        woe     score
# 1      x10 (;0.5)  0.1379245  1.799192
# 2      x10 <0.5;) -0.1638636 -2.137562
# 
# [[7]]
# variable  class        woe     score
# 1      x13 (;0.5) -0.6250575 10.519280
# 2      x13 <0.5;)  0.1491092 -2.509403