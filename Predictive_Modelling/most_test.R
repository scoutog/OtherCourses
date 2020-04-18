## 8
seq(-10,20,3)

## 9
matrix(1:30, nrow = 5, byrow = FALSE)

## 10
t <- 224+3258
b <- t + 85 + 28 
t/b
.9686

## 11
airfare <- read.csv("Airfares(1).csv")
View(airfare)

airfare_11 <- airfare[c(10,20,30,40,50),c(1,3,5,7,9)]
airfare_11

## 12
cor(airfare$FARE,airfare$DISTANCE)
# output = .670016
View(airfare)
## 13
chi_dist <- subset(airfare, airfare$S_CITY == "Chicago             IL")
chi_dist
mean(chi_dist$DISTANCE)
# mean 891.3778

## 14
sapply(airfare,class)
airfare.pca <- prcomp(airfare[,c(5,6,9:13, 16:18)], scale = TRUE)
summary(airfare.pca)
# yes scale the data for best results

## 15
boxplot(airfare$FARE~airfare$SW)

## 16
err <- (12-15) + (15-14) + (16 - 18)
err^2/3
err^2
actual <- c(15,14,18)
predicted <- c(12,15,16)
mean((actual - predicted)^2)
# 4.67

library(Metrics)
mse(actual, predicted)

## 17
q_17 <- data.frame(age = c(25, 53), spent = c(350, 420))
q_17
library(philentropy)
distance(q_17, method = "euclidean")
# 75.39231

## 18


## 19


## 21
x <- seq(-pi,pi,.1)
plot(x,cos(x), xlim = c(-4,4), col = "green", xlab = '')
lines(c(-3,3), c(-1,-1), col = "green")

## 22


## 23
util <- read.csv("Utilities(1).csv")
row.names(util) <- util[,1]
util <- util[,-1]

View(util)
utilities.df.norm <- sapply(util, scale)
View(utilities.df.norm)
row.names(utilities.df.norm) <- row.names(util) 
View(utilities.df.norm)

## 24
d.norm <- dist(utilities.df.norm, method = "euclidean")

hclust1 <- hclust(d.norm, method = "complete") 
plot(hclust1, hang = -1)


## 25 ---- double check
hclust2 <- cutree(hclust1, k = 5.7 )

row.names(utilities.df.norm) <- paste(hclust2, ":", row.names(util), sep = "")
utilities.df.norm

heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, col = rev(paste("gray", 1:99, sep = "")))


###### case questions #####


