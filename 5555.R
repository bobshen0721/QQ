
  
  model = glm(離職與否 ~ 年資+撫養人數+婚姻名稱, 
                  data=train,family="binomial")
  pred <- predict(model, newdata=test,type='response')
  new_pred<- ifelse(pred > 0.7 ,1,0)
  tb =table(real=test$離職與否, predict=new_pred)
  tb
  accuracy <- sum(diag(tb)) / sum(tb)
  accuracy
  model
  