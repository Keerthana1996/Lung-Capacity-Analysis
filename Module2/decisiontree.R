#load data
#READ csv
lungdata<-read.csv("~/Desktop/LungCapData.csv",header=T)
library(tree) #fit decision tree
head(lungdata)
attach(lungdata)

#Start Data Manipulation
range(LungCap)  

#Create categorical values on LungCap
large=ifelse(LungCap>7,"Large Volume","Small Volume")
#append Large to dataset
lungdata=data.frame(lungdata,large)
#get rid of LungCap from dataset as it is no longer required
lungdata=lungdata[,-1]

#split data into training and testing set
set.seed(3)

#dividing training set and testing set equally
train=sample(1:nrow(lungdata),nrow(lungdata)/2)
test=-train
training_data=lungdata[train,]
testing_data=lungdata[test,]
testing_large=large[test]   

#fitting model using training data
#predicting large from all other predictors/features
tree_model=tree(large~.,training_data)
plot(tree_model)
text(tree_model,pretty=0)

#checking model with test data
tree_pred=predict(tree_model,testing_data,type="class")
mean(tree_pred!=testing_large)  #gives 

#Tree Pruning
#performing cross validation to check where to stop pruning
set.seed(3)
cv_tree=cv.tree(tree_model,FUN=prune.misclass)
names(cv_tree)
plot(cv_tree$size,cv_tree$dev,type="b")

#from graph plotted, see where deviate i.e error rate is mininmum and prune there
#prune the tree
prune_model=prune.misclass(tree_model,best=3)
plot(prune_model)
text(prune_model,pretty=0)

#Checking accuracy with test data
tree_pred=predict(prune_model,testing_data,type="class")
mean(tree_pred!=testing_large)
