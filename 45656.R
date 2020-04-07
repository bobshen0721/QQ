library(data.table)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(lubridate)
library(stringr)


rec <- fread('C:\\Users\\user\\Desktop\\2020\\turnover\\leaverecord.csv')
new_rec <- rec %>% group_by(識別碼,年月) %>% 
  summarise(sum_事假時數=sum(事假時數)/2,
            sum_病假時數=sum(病假時數)/2,
            sum_曠職時數=sum(曠職時數)/2,
            sum_特休時數=sum(特休時數)/2,
            sum_遲到次數=sum(遲到次數)/2,
            sum_未帶卡次數=sum(未帶卡次數)/2,
            sum_遺失卡次數=sum(遺失卡次數)/2,
            sum_忘刷卡次數=sum(忘刷卡次數)/2,
  )
new_rec$year <- as.integer(str_sub(new_rec$年月,,-3))+1911
new_rec$mon <- as.integer(str_sub(new_rec$年月,-2,-1))
new_rec$出勤西元 <- str_c(new_rec$year, new_rec$mon,'01',sep = '-')
new_rec$出勤西元 <- as.Date(new_rec$出勤西元)



df[ is.na(df$離職日西元),"離職日西元"] = '2019-08-01' #暫時把在職的離職日寫成2019-08-01
df$"3Mago" <- df$離職日西元 -months(3)
df$"2Mago" <- df$離職日西元 -months(2)

df <-as.data.table(df)
new_rec <-as.data.table(new_rec)
setkey(df,"識別碼",'3Mago')
setkey(new_rec,"識別碼",'出勤西元')

new_mer <- new_rec[df]



set.seed(20550)
i <- sample(x=1:nrow(new_mer), size=ceiling(0.8*nrow(new_mer) ))
train <- new_mer[i,]
test <- new_mer[-i,]

# CART的模型：把離職與否的變數當作Y，剩下的變數當作X
cart.model<- rpart(離職與否 ~撫養人數+婚姻名稱+sum_事假時數+sum_病假時數+sum_特休時數+sum_遲到次數+sum_未帶卡次數, 
                   data=train)

glm.model<- glm(離職與否 ~撫養人數+sum_事假時數+sum_病假時數+sum_特休時數+sum_遲到次數+sum_未帶卡次數 ,
                    data=train,family="binomial")


#畫圖
prp(cart.model,         # 模型
    faclen=1,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    #shadow.col="gray",  # 最下面的節點塗上陰影
    #varlen =5,
    type=5,
    #tweak = 1,
    
    #extra = 2, leaf.round = 1, varlen = 0, tweak = 0.8
)  

#預測
pred <- predict(cart.model, newdata=test,type="class")
pred <- predict(glm.model, newdata=test,type='response')
new_pred<- ifelse(pred > 0.6 ,1,0)
tb =table(real=test$離職與否, predict=new_pred)
tb
summary(glm.model)

# 用table看預測的情況-混淆矩陣(confusion matrix)
tb =table(real=test$離職與否, predict=pred)
tb


#整體準確率(取出對角/總數)
accuracy <- sum(diag(tb)) / sum(tb)
accuracy
summary(pred)


#評估
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






xtabs(~merge.df[離職與否 == '1',outyear])
xtabs(~merge.df[離職與否 == '1',outmon])
xtabs(~merge.df[離職與否 == '1',辭職原因大類名稱])
write.csv(a3,'C:\\Users\\user\\Desktop\\2020_1\\a3.csv')
