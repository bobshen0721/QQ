library(data.table)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(lubridate)
library(stringr)
library(car)
library(MLmetrics)

hr.dt <-  fread('C:\\Users\\user\\Desktop\\2020\\turnover\\hrdt0316分析.csv')
write.csv(hr.dt,'C:\\Users\\user\\Desktop\\2020_1\\分析資料3.csv')
  
str(hr.dt)
hr.dt$`2Mago` <-as.Date(hr.dt$`2Mago`)
hr.dt$離職與否 <-as.factor(hr.dt$離職與否)
hr.dt$離職日西元 <-as.Date(hr.dt$離職日西元)

hr.dt$婚姻名稱 <- as.factor(hr.dt$婚姻名稱)
hr.dt$`台成清交(最高)` <- ifelse(hr.dt$`台成清交(最高)` %in% 'Y',1,0 )
hr.dt$`理工科系(最高)` <- ifelse(hr.dt$`理工科系(最高)` %in% 'Y',1,0 )
hr.dt$`台成清交(次高)` <- ifelse(hr.dt$`台成清交(次高)` %in% 'Y',1,0 )
hr.dt$`理工科系(次高)` <- ifelse(hr.dt$`理工科系(次高)` %in% 'Y',1,0 )
hr.dt[is.na(hr.dt$事假時數_2m),"事假時數_2m"] <- 0
hr.dt[is.na(hr.dt$病假時數_2m),"病假時數_2m"] <- 0
hr.dt[is.na(hr.dt$曠職時數_2m),"曠職時數_2m"] <- 0
hr.dt[is.na(hr.dt$特休時數_2m),"特休時數_2m"] <- 0
hr.dt[is.na(hr.dt$遲到次數_2m),"遲到次數_2m"] <- 0
hr.dt[is.na(hr.dt$未帶卡次數_2m),"未帶卡次數_2m"] <- 0
hr.dt[is.na(hr.dt$遺失卡次數_2m),"遺失卡次數_2m"] <- 0
hr.dt[is.na(hr.dt$忘刷卡次數_2m),"忘刷卡次數_2m"] <- 0
hr.dt <- new_mer



rec <- fread('C:\\Users\\user\\Desktop\\2020\\turnover\\leaverecord.csv')
str(new_rec)
new_rec <- rec %>% group_by(識別碼,年月) %>% 
  summarise(事假時數=sum(事假時數)/2,
            病假時數=sum(病假時數)/2,
            曠職時數=sum(曠職時數)/2,
            特休時數=sum(特休時數)/2,
            遲到次數=sum(遲到次數)/2,
            未帶卡次數=sum(未帶卡次數)/2,
            遺失卡次數=sum(遺失卡次數)/2,
            忘刷卡次數=sum(忘刷卡次數)/2,
  )
new_rec$year <- as.integer(str_sub(new_rec$年月,,-3))+1911
new_rec$mon <- as.integer(str_sub(new_rec$年月,-2,-1))
new_rec$出勤西元 <- str_c(new_rec$year, new_rec$mon,'01',sep = '-')
new_rec$出勤西元 <- as.Date(new_rec$出勤西元)

hr.dt <-as.data.table(hr.dt)
new_rec <-as.data.table(new_rec)
reg.1 <- as.data.table(reg.1)

setkey(hr.dt,"識別碼")
setkey(new_rec,"識別碼",'出勤西元')
setkey(reg.1,"識別碼")


new_mer <- reg.1[hr.dt]
str(new_mer)
hr.dt <- new_mer[,c(-2,-4)]
names(hr.dt)[2] <-'評核分數'
hr.dt[is.na(hr.dt$評核分數),"評核分數"] <- 80
str(new_mer)


hr.dt[is.na(考績),"考績"] <-'缺值'
new_mer <-hr.dt
str(new_mer)

set.seed(98989)
i <- sample(x=1:nrow(new_mer), size=ceiling(0.7*nrow(new_mer) ))
train <- new_mer[i,]

test <- new_mer[-i,]
j <- sample(x=1:nrow(test1), size=ceiling(0.5*nrow(test1) ))
test <- test1[j,]
val <-  test1[-j,]

k <- sample(x=1:nrow(val), size=ceiling(0.5*nrow(val) ))
val.1 <- val[k, ]
val.2 <- val[-k,]


# CART的模型：把離職與否的變數當作Y，剩下的變數當作X
cart.model<- rpart( 離職與否 ~案件催辦次數_3m + 特休時數_2m + 
                      年資 + 婚姻名稱 + 姓別代號 + `台成清交(最高)` + `理工科系(最高)` + 
                      特休時數_3m
                         ,data=train)

glm.model<- glm(formula = 離職與否 ~案件催辦次數_3m + 
                  年資 + 婚姻名稱 + 姓別代號 + `台成清交(最高)` + `理工科系(最高)` + 
                  特休時數_3m  
                  ,family = "binomial", data = train)

lm.model<- lm( 年資 ~ 撫養人數 + 婚姻名稱 + 姓別代號 + `台成清交(最高)` + 
                +                 `理工科系(最高)` + `台成清交(次高)` + `理工科系(次高)` + 
                +                 事假時數_3m + 病假時數_3m + 特休時數_3m + 遲到次數_3m + 未帶卡次數_3m + 
                +                 忘刷卡次數_3m + 事假時數_2m + 特休時數_2m + 遲到次數_2m + 
                +                 未帶卡次數_2m + 忘刷卡次數_2m,data=train)


summary(glm.model)
step(glm.model,direction = 'backward')

plot(cart.model)
#畫圖
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    #shadow.col="gray",  # 最下面的節點塗上陰影
    #varlen =5,
    #type=3,
    #tweak = 1,
    
    #extra = 2, leaf.round = 1, varlen = 0, tweak = 0.8
)  

#預測
pred <- predict(cart.model, newdata=test,type="class")
pred <- predict(glm.model, newdata= test2018,type='response')
new_pred <- ifelse(pred >= 0.25 , 1, 0)

F1 <- F1_Score(y_pred = new_pred, y_true = test201903$離職與否, positive = 1)
F1
recall <- 38/(73+38)
recall
precision <- 38/(253+38)
precision

tb =table(real=test2018$離職與否, predict=new_pred)
tb
summary(glm.model)

# 用table看預測的情況-混淆矩陣(confusion matrix)
tb =table(real=train$離職與否, predict=pred)
tb


#整體準確率(取出對角/總數)
accuracy <- sum(diag(tb)) / sum(tb)
accuracy

#自我增強訓練
train$mark =''
train$mark= ifelse(as.factor(train$離職與否) == new_pred,"Y","N")
glm.model<- glm(as.factor(離職與否) ~案件催辦次數_3m + 特休時數_2m + 
                  年資 + 婚姻名稱 + 姓別代號 + `台成清交(最高)` + `理工科系(最高)` + 
                  特休時數_3m  ,
                    data=rbind(train,train[train$mark=='N',],train[train$mark=='N',],train[train$mark=='N',]) ,family="binomial")



#預測錯的標示出來
test201903$mark=''
test201903$mark= ifelse(test201903$離職與否 == new_pred,"Y","N")

pred <- predict(glm.model, newdata= test201903[test201903$mark=='N',],type='response')
new_pred<- ifelse(pred > 0.9 ,1, 0)
tb =table(real=test201903[test201903$mark=='N',]$離職與否, predict=new_pred)
tb









library(ROCR)
pre <- prediction(pred, test$離職與否)
perf <- performance(pre, measure = "tpr", x.measure = "fpr")
#計算AUC
auc <- performance(pre, "auc")

#畫圖
plot(perf, col = rainbow(7), main = "ROC curve", xlab = "Specificity(FPR)", ylab = "Sensitivity(TPR)")
#AUC = 0.5
abline(0, 1)
#實際AUC值
text(0.5, 0.5, as.character(auc@y.values[[1]]))
##
 
hr.dt.1 <- hr.dt[離職與否 %in% 1,]
hr.dt.0 <- hr.dt[離職與否 %in% 0,]
write.csv(hr.dt.0,'C:\\Users\\user\\Desktop\\2020_1\\hrdt0.csv')






hr.dt2009 <-  hr.dt[hr.dt$離職日西元 >= as.Date('2009-01-01'), ]
test2018 <- test[test$離職日西元 >= as.Date('2018-08-01'),]
test2019 <- test[test$離職日西元 >= as.Date('2019-01-01'),]
