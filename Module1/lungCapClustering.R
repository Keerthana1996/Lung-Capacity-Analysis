
lungdb_features <- lungdb

# removing the Smoke attribute from the new Database.
lungdb_features$Smoke<- NULL
str(lungdb_features)

# normalising numeric attributes.
norm<- function(x){
return ( (x-min(x))/(max(x)-min(x)) ) }
lungdb_n<- as.data.frame(lapply(lungdb_features[, c(1,2,3,5,6)], norm))

# applying K-means Clustering algorithm.
results<- kmeans(lungdb_n, 2)
results

# generating the confusion matrix.
table(lungdb$Smoke, results$cluster)

# plotting the predicted clusters.
plot(lungdb[c("LungCap", "Age")], col=results$cluster)

#plotting actual data.
plot(lungdb[c("LungCap", "Age")], col=lungdb$Smoke)
