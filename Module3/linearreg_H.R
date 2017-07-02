#load data
#READ csv
lungdata<-read.csv("~/Desktop/LungCapData.csv",header=T)
attach(lungdata)
summary(lungdata)
#Changing continuos hieght to categorical height
#adding new height column and removing original
Height_new=ifelse(Height>63,"Tall","Short")
lungdata=data.frame(Height_new,lungdata)
summary(lungdata)
lungdata=lungdata[,-4:-7]
#splitting data into training and testing set
training=(LungCap<6)  #contains true or flase
testing= !training
#training data to fit the model
training_data=lungdata[training,]
testing_data=lungdata[testing,]
Height_tesing=Height_new[testing]
#fit the linear model wit htest data
#using glm ie general logistic model
#we are trying to predict height from lungcap and age
logreg=glm(Height_new~ LungCap+ Age, data=training_data, family=binomial)
summary(logreg)

#use the fitting model to do predictions for test data
#this predicts probabilities not class
pred_model_prob=predict(logreg,testing_data,type="response")

#to convert probabilities to class
pred_model_Height=rep("Short",560)
pred_model_Height[pred_model_prob>0.5]="Tall"

#generate confusion matrix
table(pred_model_Height,Height_tesing)
#misclassification error
mean(pred_model_Height!=Height_tesing)
