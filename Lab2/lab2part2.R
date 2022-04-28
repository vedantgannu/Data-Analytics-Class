library("readxl")
library("ggplot2")

#EXERCISE 1:
dataset = read.csv(file.choose(), header=TRUE)

mm = lm(ROLL~UNEM+HGRAD, dataset)
#After fitting linear regression model, predict ROLL for UNEM=7.0 and HGRAD=90000
predict(mm, data.frame(UNEM=c(7.0), HGRAD=c(90000)), interval="prediction")
#ROLL -> 81437.04

mm = lm(ROLL~UNEM+HGRAD+INC, dataset)
#After fitting linear regression model, predict ROLL for UNEM=7.0 and HGRAD=90000 and INC=25000
predict(mm, data.frame(UNEM=c(7.0), HGRAD=c(90000), INC=c(25000)), interval="prediction")
#ROLL -> 137452.6





#EXERCISE 2:
library(dplyr)
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,
                    sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight',
                       'rings' )


# As shown above, the "rings" variable has a range between 1-29.
# This is the variable that we want to predict, and predicting this many levels
# might not give us the insight we're looking for.
# For now, we'll break the rings variable
# into 3 levels" "young" for abalones less than 8, "adult" for abalones between 8-11,
# and "old" for abalones older than 11. Create 3 class labels
abalone$rings = as.numeric(abalone$rings)
abalone$rings = cut(abalone$rings, breaks=c(-1, 8, 11, 35), labels=c("young", "adult", "old"))
abalone$rings = as.factor(abalone$rings)
summary(abalone$rings)

# remove the "sex" variable in abalone, because KNN requires all numeric variables for prediction
# z <- abalone
aba <- abalone
aba$sex <- NULL
#Normalize the data so that euclidean distance measure isn't swayed by large ranges
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)

# After Normalization, each variable has a min of 0 and a max of 1.
# in other words, values are in the range from 0 to 1.
# We'll now split the data into training and testing sets.
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]


library(class)
library(gmodels)
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 30)

classification_outcomes = data.frame(KNNtest["rings"], KNNpred)
colnames(classification_outcomes) = c("rings", "prediction")
CrossTable(x = classification_outcomes$rings, y = classification_outcomes$prediction
           , prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)
#Accuracy metric
nrow(classification_outcomes %>% filter(rings == prediction))/nrow(classification_outcomes)




KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)

classification_outcomes = data.frame(KNNtest["rings"], KNNpred)
colnames(classification_outcomes) = c("rings", "prediction")
CrossTable(x = classification_outcomes$rings, y = classification_outcomes$prediction
           , prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)
#Accuracy metric
nrow(classification_outcomes %>% filter(rings == prediction))/nrow(classification_outcomes)



KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 70)

classification_outcomes = data.frame(KNNtest["rings"], KNNpred)
colnames(classification_outcomes) = c("rings", "prediction")
CrossTable(x = classification_outcomes$rings, y = classification_outcomes$prediction
           , prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)
#Accuracy metric
nrow(classification_outcomes %>% filter(rings == prediction))/nrow(classification_outcomes)





#EXERCISE 3:
data("iris")
iris
new_iris = iris[,-5]


colnames(iris)
set.seed(300)
k.max <- 12
# tot.withinss = Total within-cluster sum of square (Inertia)
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss = sapply(1:k.max,function(k){kmeans(new_iris,k,nstart = 20,iter.max = 1000)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
#Since k=3 is the optimal k value according to Elbow Plot
icluster = kmeans(new_iris,3,nstart = 20, iter.max = 1000)
table(icluster$cluster,iris$Species)

icluster = kmeans(new_iris,4,nstart = 20, iter.max = 1000)
table(icluster$cluster,iris$Species)
