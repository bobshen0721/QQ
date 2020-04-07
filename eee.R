library(data.table)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(lubridate)
library(stringr)

fcfc <-  fread('C:\\Users\\user\\Desktop\\com\\fcfc.csv',sep=',')
fpg <-  fread('C:\\Users\\user\\Desktop\\com\\fpg.csv',sep=',')
tol <-  fread('C:\\Users\\user\\Desktop\\com\\tol.csv',sep=',')
nanya <-  fread('C:\\Users\\user\\Desktop\\com\\nanya.csv',sep=',')
oil <-  fread('C:\\Users\\user\\Desktop\\com\\oil.csv',sep=',')
names(fcfc)[1] <-'識別碼'
names(fpg )[1] <-'識別碼'
names(tol)[1] <-'識別碼'
names(nanya)[1] <-'識別碼'
names(oil)[1] <-'識別碼'

setkey(hr.dt,"識別碼")
setkey(fcfc,"識別碼")
setkey(fpg,"識別碼")
setkey(tol,"識別碼")
setkey(nanya,"識別碼")
setkey(oil,"識別碼")

fcfc.mer <- merge(fcfc,hr.dt)
fpg.mer <- merge(fpg,hr.dt)
tol.mer <-merge(tol,hr.dt)
nanya.mer <- merge(nanya,hr.dt)
oil.mer <- merge(oil,hr.dt)
write.csv(oil.mer,'C:\\Users\\user\\Desktop\\com\\oil_mer.csv')

