#load data
#READ csv
lungdata<-read.csv("~/Desktop/LungCapData.csv",header=T)
attach(lungdata)
head(lungdata)
range(LungCap)

#Using cut to convert continuos data to categorical data
LungCap_new<-cut(LungCap,c(0,2,4,6,8,10,12,15))
lungdata=data.frame(lungdata,LungCap_new) #adding new column
lungdata=lungdata[,-1]  #removing original LungCap
summary(lungdata)             

lungdata=lungdata[,-1:-3]
lungdata=lungdata[,-2]

#Cross Table for 2 variables
lungdata<- xtabs(~LungCap_new + Gender,lungdata)
lungdata

lungdata<-read.csv("~/Desktop/LungCapData3.csv",header=T)
attach(lungdata)

#To fit regression using general linear model
logreg<-glm(cbind(Female,Male)~LungCap, family=binomial)
summary(logreg)

exp(0.11686)
(1.123962-1)*100

plot(LungCap,fitted.values(logreg),type="b")

#compare with observed probability values
points(LungCap,Female/(Female+Male), col="red")
