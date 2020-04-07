library(data.table)
library(stringi)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(lubridate)
library(stringr)
library(car)
library(MLmetrics)
library(gbm)
train$y <- ''
test$y <- ''
train$y <- year(train$離職日西元)
test$y <- year(test$離職日西元)
train$y <- ifelse(train$y <2004 ,'舊',ifelse(train$y <2014 ,'中','新'))
test$y <- ifelse(test$y <2004 ,'舊',ifelse(test$y <2014 ,'中','新'))


train$離職與否
train$離職與否 <- as.integer(train$離職與否)
test$離職與否 <- as.integer(test$離職與否)
gbm.model.2 <- gbm(formula = 離職與否 ~ 加班時數_3m + 案件催辦次數_3m + 事假時數_3m+
                   病假時數_3m +  曠職時數_3m +  特休時數_3m + 遲到次數_3m + 未帶卡次數_3m + 遺失卡次數_3m+ as.factor(認定學歷名稱) +
                   忘刷卡次數_3m  + 年資 + as.factor(婚姻名稱) + 撫養人數+ as.factor(姓別代號)  + as.factor(考績) +
                   `台成清交(最高)` +　`理工科系(最高)`  +　`台成清交(次高)` + `理工科系(次高)`,
    data=train,
    shrinkage = 0.1,
    #distribution = "bernoulli",
    n.trees = 20000,
    interaction.depth = 2,
    n.minobsinnode = 5,
    bag.fraction = 0.6,
    train.fraction = 1,
    n.cores = NULL,
    cv.folds = 0 
    )


pred <- predict(gbm.model.2,
               test,
               n.trees =20000,
               type = "response",
               single.tree = FALSE
               )

#gbm.perf(gbm.model.2,method="cv")
#max(pred)

new_pred <- ifelse(pred >= 0.5, 1, 0)

#new_pred

#test2018$離職與否


F1 <- F1_Score(y_pred = new_pred, y_true = test$離職與否, positive = 1)
F1


#which.min(gbm.model.2$train.error)
#lsqrt(min(gbm.model.2$train.error))

#recall <- 9434/(1353+9434)
#recall
#precision <- 9434/(557+9434)
#precision
tb =table(real=test$離職與否, predict=new_pred)
tb

accuracy <- sum(diag(tb)) / sum(tb)
accuracy

hyper_grid <- expand.grid(
  shrinkage = c(0.3, 0.1, 0.01), # 學習步伐
  interaction.depth = c(  5, 10), # 模型切割數
  n.minobsinnode = c(10, 15, 20), # 節點最小觀測值個數
  bag.fraction = c(0.5,0.6, 0.8, 1), # 使用隨機梯度下降(<1)
  optimal_trees = 0,               # 儲存最適模型樹的欄位
  min_RMSE = 0                     # 儲存最小均方差的欄位
)

for(i in 1:nrow(hyper_grid)) {
  # train model
  gbm.tune <- gbm(
    formula =  離職與否 ~ 加班時數_3m + 案件催辦次數_3m + 事假時數_3m+
      病假時數_3m +  曠職時數_3m +  特休時數_3m + 遲到次數_3m + 未帶卡次數_3m + 遺失卡次數_3m+ as.factor(認定學歷名稱) +
      忘刷卡次數_3m  + 年資 + as.factor(婚姻名稱) + 撫養人數+ as.factor(姓別代號) + year(到職日) + as.factor(考績) +
      `台成清交(最高)` +　`理工科系(最高)` +　`台成清交(次高)` +　`台成清交(次高)` + `理工科系(次高)`,
    data=train,
    n.trees = 20000, # 使用5000個樹模型
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = 1, # 使用75%的訓練資料，並用剩餘資料做OOB成效評估/驗證
    n.cores = NULL, # will use all cores by default
    cv.folds = 0 
    )
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$train.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$train.error))
}

summary(gbm.model)


glm.model<- glm(formula = 離職與否 ~ 加班時數_3m + 案件催辦次數_3m + 事假時數_3m+
                  病假時數_3m +  曠職時數_3m +  特休時數_3m + 遲到次數_3m + 未帶卡次數_3m + 遺失卡次數_3m+ as.factor(認定學歷名稱) +
                  忘刷卡次數_3m  + 年資 + as.factor(婚姻名稱) + 撫養人數+ as.factor(姓別代號) + year(到職日) + as.factor(考績) +
                  `台成清交(最高)` +　`理工科系(最高)` +　`台成清交(次高)` +　`台成清交(次高)` + `理工科系(次高)` 
                  ,data = train,family = 'binomial')

pred <- predict(glm.model, newdata= test2018,type='response')
max(pred)
new_pred<- ifelse(pred >= 0.5 ,1, 0)
tb =table(real=test2018$離職與否, predict=new_pred)
tb
F1 <- F1_Score(y_pred = new_pred, y_true = test2018$離職與否, positive = 1)
F1

summary(glm.model)
