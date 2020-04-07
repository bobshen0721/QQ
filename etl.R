library(data.table)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(lubridate)
library(stringr)

reg <-fread('C:\\Users\\user\\Desktop\\2020\\turnover\\regularworkassessment.csv',sep=',')
names(reg)[1:3]<- c('識別碼','年月','考績')
head(reg,100)
nrow(reg[識別碼 =='AAB/5xAAKAAABP0AAA',])
reg.2 <- reg[識別碼 =='AAB/5xAA2AAAaHuAAi',]
nrow(reg.2)
xtabs(~reg$識別碼)
table(reg$識別碼)

reg <- reg %>% arrange(desc(as.integer(年月))) 
reg.1 <- distinct(reg,reg$識別碼,reg$年月,.keep_all = T)
reg.1

summary(reg.1)
reg.1[is.na(reg.1$考績),'考績'] <- 82
write.csv(reg.1,'C:\\Users\\user\\Desktop\\2020_1\\reg01.csv')
