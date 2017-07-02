
# to get the same random sampling results
set.seed(77)

# generate as many random numbers as there are rows in the database.
g <- runif(nrow(lungdb))

# order the database in the order of the randon numbers generated.
lungdb <- lungdb[order(g), ]

# to normalize the numerical attributes in the database.
norm<- function(x){
return ( (x-min(x))/(max(x)-min(x)) ) }
lungdb_n <- as.data.frame(lapply(lungdb[, c(1,2,3)], norm))

# partitioning the database into training and testing set.
lungdb_train <- lungdb_n[1:655, ]
lungdb_test <- lungdb_n[656:725, ]

# specifying the class attribute. 
lungdb_train_target <- lungdb[1:655, 4]
lungdb_test_target <- lungdb[656:725, 4]
require(class)

# applying knn model, specifying k as 27.
m1 <- knn(train=lungdb_train, test=lungdb_test, cl=lungdb_train_target, k=27)
m1

# generating the confusion matrix
table(lungdb_test_target, m1)



