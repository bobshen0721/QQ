library(data.table)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(lubridate)
library(stringr)

hr.dt <- fread('C:\\Users\\user\\Desktop\\2020_1\\hrdt0226.csv')


#日期轉明國字串(JK)
hr.dt$jkey_2m=''
hr.dt$jkey_3m=''

hr.dt$jkey_2m=str_c(as.character(year(hr.dt$`2Mago`)-1911),str_pad(as.character(month(hr.dt$`2Mago`)),2,'left','0'))
hr.dt$jkey_3m=str_c(as.character(year(hr.dt$`3Mago`)-1911),str_pad(as.character(month(hr.dt$`3Mago`)),2,'left','0'))
hr.dt[hr.dt$離職與否 == 0,"jkey_3m"] <-hr.dt[hr.dt$離職與否 == 0,"jkey_new"] 
hr.dt$年度 <-year(hr.dt$`3Mago`)-1
hr.dt[hr.dt$離職與否 == 0,"年度"]  <-year(hr.dt$到職日[hr.dt$離職與否== 0])#特殊寫法

hr.dt[,"jkey_new"]=str_c(as.character(year(hr.dt$到職日+210)-1911),str_pad(as.character(month(hr.dt$`到職日`+210)),2,'left','0'))


#right table
reg <-fread('C:\\Users\\user\\Desktop\\2020\\turnover\\regularworkassessment.csv',sep=',')
names(reg)[1:3]<- c('識別碼','年月','考績')
reg.1 <- distinct(reg,reg$識別碼,reg$年月,.keep_all = T)
reg.1

hr.dt$離職年 <-''
hr.dt$離職年<- as.integer(year(hr.dt$離職日西元))
rec$年月<- as.character(rec$年月)

setkey(hr.dt,"識別碼",'年度')
hr.dt <- as.data.table(hr.dt)
performance_0 <- as.data.table(performance_0)
setkey(performance_0,"識別碼",'年度')




hr.dt.1 <- performance_0[hr.dt]
str(hr.dt.1)
hr.dt.1<- hr.dt.1[,c(-1,-6,-7)]
names(hr.dt.1)[2]<-"over46hr_3m"
summary(hr.dt.1)
hr.dt.1[is.na(hr.dt.1$加班時數),"加班時數"] <- 0
str(hr.dt.1)


hr.dt <- hr.dt.1
summary(hr.dt)


write.csv(hr.dt,'C:\\Users\\user\\Desktop\\2020\\turnover\\hrdt清洗過0313.csv')
