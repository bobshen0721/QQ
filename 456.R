library(data.table)
library(readxl)
library(dplyr)
#res離職人員資料
res<- read_excel('C:\\Users\\user\\Desktop\\2020\\turnover\\resign.xls',sheet = 1)
#rec出勤紀錄
rec <- fread('C:\\Users\\user\\Desktop\\2020\\turnover\\leaverecord.csv')


 new_rec <- rec %>% group_by(識別碼) %>% 
   summarise(sum_事假時數=sum(事假時數),
             sum_病假時數=sum(病假時數),
             sum_曠職時數=sum(曠職時數),
             sum_特休時數=sum(特休時數),
             sum_遲到次數=sum(遲到次數),
             sum_未帶卡次數=sum(未帶卡次數),
             sum_遺失卡次數=sum(遺失卡次數),
             sum_忘刷卡次數=sum(忘刷卡次數),
             )
 
 View(rec[rec$識別碼 %in% 'AAB/5xAA3AAAZw9AAE',])
 nrow(res)
 nrow(merge_df)
merge_df<-merge(new_rec,res,by ='識別碼')
