#generate a box plot
AgeGrp<- cut(Age, breaks = c(0, 13, 15, 17, 25), labels=c("<13", "14-15", "15-17", "18+"))
Age[1:5]
AgeGrp[1:5]
boxplot(LungCap, ylab = "LungCapacity", main = "Lung Capacity box plot", las = 1)
boxplot(LungCap~Smoke*AgeGrp, ylab = "LungCapacity", main= "Lung Capacity vs 
        Smokers for different Age groups",
        las = 2, col = c(3,2))
boxplot(LungCap~Gender, ylab = "LungCapacity", main = "Lung Capacity vs Gender", 
        las = 1, col = c(6,4))
HeightGrp<- cut(Height, breaks = c(0, 64, 82), labels = c("short", "tall"))
boxplot(LungCap~HeightGrp, ylab = "LungCapacity", main = "Lung Capacity vs Height", 
        las = 1, col = c(10,4))

plot(Age[Height<50],LungCap[Height<50], main = "Lung Capacity vs Age", las = 1)
pairs(lungdb[, 1:3])
#generate a histogram
hist(LungCap, freq = F, breaks = 14, main = "Density distribution of Lung 
     Capacity", las = 1)
lines(density(LungCap), col = 4, lwd = 3)
#generate a bar plot
t1 <- table(Smoke, Gender)
t1
barplot(t1, beside = T, legend.text = T, main = "Smoke vs Gender", las = 1)

# generating headmap
library(gplots)
install.packages("gplots")

#generating pie chart
count <- table(Gender)
pie(count, main = "Gender Distribution", col = c(6,4))
box()
