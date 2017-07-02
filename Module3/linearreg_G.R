#load data
#READ csv
lungdata<-read.csv("~/Desktop/LungCapData.csv",header=T)
attach(lungdata)

summary(lungdata)

lungdata=lungdata[,-4]
lungdata=lungdata[,-5]

#splitting data into training and testing set
training=(LungCap<6)  #contains true or flase
testing= !training

#training data to fit the model
training_data=lungdata[training,]
testing_data=lungdata[testing,]
Gender_tesing=Gender[testing]


#fit the linear model wit htest data
#using glm ie general logistic model
#we are trying to predict height from lungcap and age
logreg=glm(Gender~., data=training_data, family=binomial)

summary(logreg)

#use the fitting model to do predictions for test data
#this predicts probabilities not class
pred_model_prob=predict(logreg,testing_data,type="response")

#to convert probabilities to class
pred_model_Gender=rep("Female",560)
pred_model_Gender[pred_model_prob>0.5]="Male"

#generate confusion matrix
table(pred_model_Gender,Gender_tesing)
#misclassification error
mean(pred_model_Gender!=Gender_tesing)
