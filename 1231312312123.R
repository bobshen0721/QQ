library(xgboost)
xgb <- xgboost(data = data.matrix(train[,c(3,19,27,28,30,34,36,49)]), 
               label = train$離職與否, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)

y_pred <- predict(xgb, data.matrix(train[,c(3,19,27,28,30,34,36,49)]))
                  
y_pred <-ifelse(y_pred > 0.054 , 1, 0)

y_pred

tb =table(real=train$離職與否, predict=y_pred)
tb
