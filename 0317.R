library(olsrr)
install.packages('psych')
library(psych)
CVD<-read.csv("C:\\Users\\user\\Desktop\\北醫\\W1多變量分析與實作\\CVD_All.csv",head=T)
names(CVD) #資料欄位
str(CVD) #資料欄位+屬性
mean(CVD$age,na.rm = T)#計算平均數
CVD_a<-CVD[complete.cases(CVD),]##將完整資料留下 若有遺漏就不留下
##na.omit(CVD_a) 方法2→移除遺漏值
CVD_a

CVD_a=CVD_a[,-1]#整理資料 將不需要的ID欄位移除

model<-glm(CVD~.,family = binomial,data = CVD_a)#glm廣義線性模型

k=ols_step_backward(model)
k
kk=step(model,direction = "backward",k=2,steps = 100)

summary(kk)