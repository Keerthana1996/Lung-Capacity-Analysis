#load data and READ csv
lungdata<-read.csv("~/Desktop/LungCapData.csv",header=T)
attach(lungdata)
head(lungdata)
range(LungCap)

#Using cut to convert continuos data to categorical data
LungCap_new<-cut(LungCap,c(0,2,4,6,8,10,12,15))
lungdata=data.frame(lungdata,LungCap_new) #adding new column
lungdata=lungdata[,-1]  #removing original LungCap
summary(lungdata)             

Height_new=ifelse(Height>63,"Tall","Short")   #converting to category
lungdata=data.frame(lungdata,Height_new)  #adding new column
lungdata=lungdata[,-2]  #Removing Height column
summary(lungdata)

#table for single variable
table(Height_new) 
#Cross Table for 2 variables
lungdata<- xtabs(~LungCap_new + Height_new,lungdata)
lungdata

lungdata<-read.csv("~/Desktop/LungCapData2.csv",header=T)
attach(lungdata)

#To fit regression using general linear model
logreg<-glm(cbind(Tall,Short)~LungCap, family=binomial)
summary(logreg)
exp(1.3828)
(3.986047-1)*100

plot(LungCap,fitted.values(logreg))

#compare with observed probability values
points(LungCap,Tall/(Short+Tall), col="red")
