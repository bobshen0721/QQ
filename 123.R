library(data.table)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)

df <- fread('C:\\Users\\user\\Desktop\\2020\\turnover\\分析檔(108.03.14)v4_1.csv')
str(df)
df$flag <- 0
df[df$`在離職註記(1在職2離職)` %in% '離職','flag'] <- 1
df$年資 <- df$`(離職時)企業年資`
df$離職名稱 <- df$`在離職註記(1在職2離職)`
df[df$離職名稱 %in% "離職" , '離職名稱'] <- '辭職'
df$`理工科系(最高)` <- ifelse(df$`理工科系(最高)` %in% '1' , 'Y',NA)
set.seed(20550)
i <- sample(x=1:nrow(df), size=ceiling(0.8*nrow(df) ))
train <- df[i,]
test <- df[-i,]

# CART的模型：把離職與否的變數當作Y，剩下的變數當作X
cart.model<- rpart(`在離職註記(1在職2離職)` ~`(離職時)企業年資`+`理工科系(最高)`+累計證照加分 , 
                   data=train)

glm.model<- glm(flag ~`(離職時)企業年資`+`理工科系(最高)`+近三個月加班時數+`企業外工作年資(1)`+`理工科系(次高)`+累計證照加分 , 
                   data=train,family="binomial")
#畫圖
prp(cart.model,         # 模型
    faclen=1,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    #shadow.col="gray",  # 最下面的節點塗上陰影
    varlen =6,
    type=4,
    tweak = 1.5,
    
    #extra = 2, leaf.round = 1, varlen = 0, tweak = 0.8
    )  

#預測
pred <- predict(cart.model, newdata=df,type="class")
pred <- predict(glm.model, newdata=test,type='response')
new_pred<- ifelse(pred > 0.3 ,1,0)
tb =table(real=test$flag, predict=new_pred)
tb
summary(glm.model)

# 用table看預測的情況-混淆矩陣(confusion matrix)
tb =table(real=df$離職名稱, predict=pred)
tb


#整體準確率(取出對角/總數)
accuracy <- sum(diag(tb)) / sum(tb)
accuracy
summary(pred)

