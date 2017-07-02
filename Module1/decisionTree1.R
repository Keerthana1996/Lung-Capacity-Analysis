lungdb <- read.table("C:\\Users\\Ishmita\\Documents\\LungCapData.csv", header = T, sep = ",")
set.seed(13)
train<- sample(1:nrow(lungdb), nrow(lungdb)/2)
test= -train
train_data = lungdb[train, ]
test_data = lungdb[test, ]
tree_model <- tree(Smoke~., train_data)
text(tree_model, pretty=0)

tree_predict = predict(tree_model, test_data, type ="class")
smoke = lungdb[test, c(4)]

mean(tree_predict != smoke)
rpart_tree<- rpart(Smoke~., train_data, method = "class" )

rpart.plot(rpart_tree, type = 4, extra = 101)

table(test_data[, 4], tree_predict)
