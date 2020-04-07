library(stringr)
library(readxl)
library(data.table)
library(rpart)
library(rpart.plot)
library(boot)
library(cramer)


res<- read_excel('C:\\Users\\user\\Desktop\\2020\\turnover\\resign.xls',sheet = 1)
person.1<-  read_excel('C:\\Users\\user\\Desktop\\2020\\turnover\\personalinformation.xls',sheet = 1)
person.2<-  read_excel('C:\\Users\\user\\Desktop\\2020\\turnover\\personalinformation.xls',sheet = 2)
person<- rbind(person.1, person.2)

person$inyear <- as.integer(str_sub(person$進企業日,,-5))+1911
person$inmon <- as.integer(str_sub(person$進企業日,-4,-3))
merge.df <- merge(person,res,all.x = T,by = "識別碼")
merge.df$到職日 <- str_c(merge.df$inyear, merge.df$inmon,'01',sep = '-')
merge.df$到職日 <- as.Date(merge.df$到職日)


merge.df$outyear <- as.integer(str_sub(merge.df$離職日,,-5))+1911
merge.df$outmon <- as.integer(str_sub(merge.df$離職日,-4,-3))
merge.df$離職日西元 <- str_c(merge.df$outyear, merge.df$outmon,'01',sep = '-')
merge.df$離職日西元 <- as.Date(merge.df$離職日西元)

merge.df$年資<- ifelse(merge.df$離職代號 %in% 'HH' ,merge.df$離職日西元-merge.df$到職日,as.Date('2019-08-08')-merge.df$到職日)
merge.df$年資<- merge.df$年資/365
merge.df[ is.na(merge.df$離職名稱)   ,'離職名稱'] <- '在職'
merge.df$離職與否 <- 0
merge.df[merge.df$離職名稱 %in% "辭職","離職與否"] <- 1

merge.df <- as.data.table(merge.df)

fwrite(merge.df,'C:\\Users\\user\\Desktop\\2020_1\\分析檔0206.csv')

df<- merge.df
df<-as.data.frame(df[,c(1,5,8,10,12,13,15,16,18,19,21,22,23,24,25,26,28,29,44,45,46)])

df[,'離職與否'] <- as.factor(df[,'離職與否'])

set.seed(211055022)
i <- sample(x=1:nrow(df), size=ceiling(0.7*nrow(df) ))
train <- df[i,]
test <- df[-i,]

# CART的模型：把離職與否的變數當作Y，剩下的變數當作X
cart.model<- rpart(離職與否 ~ 婚姻名稱+撫養人數, 
                   data=train)
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    #shadow.col="gray",  # 最下面的節點塗上陰影
    varlen =2,
    type=5,
    tweak = 1,
    
    #extra = 2, leaf.round = 1, varlen = 0, tweak = 0.8
)  
pred <- predict(cart.model, newdata=test,type="class")
# 用table看預測的情況-混淆矩陣(confusion matrix)
tb =table(real=test$離職與否, predict=pred)
tb


#整體準確率(取出對角/總數)
accuracy <- sum(diag(tb)) / sum(tb)
accuracy

nrow(merge.df[merge.df$離職與否 == '0',])
