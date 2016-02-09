########Exploratory analysis

Interesting websites: plot
                      R bloggers   

## very useful command => adds a new graph to the screen of your old graph 
par(new=True)

##BAsic commands
pollution <- read.csv("data/avgpm25.csv", colClasses = c("numeric", "character", "fcator", "numeric", "numeric"))
head(pollution)
summary(pollution$pm25)

## Boxplot

boxplot(pollution$pm24, col="blue")
abline(h=12)

boxplot(pm25 ~ region, data = pollution, col = "red")

library(datasets)
airquality<- transform(aiquality, MOnth = factor(Month))
boxplot(Ozone ~ MOnth, airquality, xlab = "Month", ylab = "Ozone (ppb)")

## Histogram
hist(pollution$pm25, col = "green")
hist(pollution$pm25, col = "green", breaks = 100)
abline(v= 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)
rug(pollution$pm25)

## The least squares estimate is the empirical mean
meanChild <- mean(galton$child)
lines(rep(meanChild,100),seq(0,150, length=100),col="red",lwd=5) # sets a line in the middle part of the histogram

library(datasets)
hist(airquality$Ozone)
with(airquality, plot(Wind, Ozone))

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

## Barplot
barplot(table(pollution$region), col = "wheat", main = "NUmber of Counties in Each Region")

##Scatterplot
with(pollution, plot(latitude, pm25))
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

par(mfrow = c(1,2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

## Calling a graphic
plot(x, y)
hist(x)

## par() function
las: the orientation of the axis labels on the plot
bg: the background color
mar: the margin size
oma: the outer margin size (default is 0 for all sides)
mfrow: number of plots per row, column (plots are filled row-rise)
mfcol: number of plots per row, column (plots are filled column-wise)

## Plot with Annotation
library(datasets)
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points (Wind, Ozone, col = "blue"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

## BAse plot with regression line
library(datasets)
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

## Multiple plots in a single figure
library(datasets)
par(mfrow = c(1, 2)) ## the first number means the number of rows and the second number means the number of columns
with(airquality, {
     plot(Wind, Ozone, main = "Ozone and Wind")
     plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})      


par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
                plot(Wind, Ozone, main = "Ozone and Wind")
                plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
                plot(Temp, Ozone, main = "Ozone and Temperature")
                mtext("Ozone and Weather in New York City", outer = TRUE)
}) 


##MAking a plot
x <- rnorm(100)
y <- rnorm(100)
plot(x, y, pch = 20)
title ("Scatterplot")
text(-2, -2, "Label")
legend("topleft", legend = "Data")
legend("topleft", legend = "Data", pch = 20)
fit <- lm(y ~ x)
abline(fit)
abline(fit, lwd = 3, col = "blue")
plot(x, y, xlab= "Weight", ylab = "Height", main = "Scatterplot", pch = 20)
legend("topright", legend = "Data", pch = 20)
fit <- lm(y ~ x)
abline(fit, lwd = 3, col = "red")

with(cavebats, plot(Richness, Caves, main = "Distribution of the richness of bats inside caves", pch = 20, xlab="Richness of bats", ylab="Number of caves", cex.lab=1.50 ))
model <- lm(Richness ~ Caves, cavebats)
abline(model, lwd = 2)
with(cavebats, plot ( plot(x, y, main="title", sub="subtitle",
  
plot(x, y, main="title", sub="subtitle", xlab="X-axis label", ylab="y-axix label",   xlim=c(xmin, xmax), ylim=c(ymin, ymax), cex.lab=1.75)

## Copying Plots
library(datasets)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.copy(png, file = "geyserplot.png")
dev.off()

## Simple lattice plot
library(lattice)
library(datasets)
xyplot(Ozone ~ Wind, data = airquality)
## Convert `MOnth` to a factor variable
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))

p <- xyplot(Ozone ~ Wind, data = airquality)
print(p)

xyplot(Ozone ~ Wind, data = airquality)

## LAttice Panel Functions
set.seed(10)
x <- rnom(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group1", "Group 2"))
xyplot(y ~ x | f, layout = c(2, 1)) # Plot with 2 panels

## Regression line
xyplot(y ~ x | f, panel = function(x, y, ...) {
       panel.xyplot(x, y, ...) ## First call default panel function
       panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
})

##### Ggplot 2 package

set.seed(1234)
par(mar = c(0, 0, 0, ))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd =0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x+0.05, y + 0.05, labels = as.character(1:12))

dataFrame <- data.frame(x = x, y = y)
dist(dataFrame)

# Hierarchical clustering - hclust
dataFrame <- data.frame(x=x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

# Heatmap function
dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMAtrix <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMAtrix)

#K-means clustering - example
set.seeds(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

#Kmeans
par(mar = rep(0.2, 4))
plot(x, y, col = kmeans0bj$cluster, pch = 19, cex = 2)
points(kmeans0bj$centers, col = 1:3, pch = 3, lwd = 3)

#Heatmaps
set.seed(1234)
dataMAtrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeans0bj2 <- kmeans(dataMAtrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMAtrix)[,nrow(dataMatrix):1], yaxt = "n")
image(t(dataMAtrix)[, order(kmeans0bj$cluster)], yaxt = "n")

#Matrix data
set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(DataMAtrix)[, nrow(dataMatrix):1])

#Cluster the data
par(mar = rep(0.2, 4))
heatmap(dataMatrix)

#What if we add a pattern to the data set?
set.seed(678910)
for (i in 1:40){
     #flip a coin
     coinFlip <- rbinom(1, sieze = 1, prob = 0.5)
     #if coin is heads add a common pqattern to that row
     if (coinFlip) {
        dataMAtrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
      }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMAtrix)[, nrow(dataMAtrix):1])

#Patterns in rows and columns
hh <- hclust(dist(dataMatrix))
dataMatrix0rdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrix0rdered)[, nrow(dataMatrix0rdered):1])
plot(rowMeans(dataMatrix0rdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrix0rdered), xlab = "Column", ylab = "Column Mean", pch = 19)   

#Reducing scales
svd1 <- svd(scale(dataMatrix0rdered))
par(mfrow = c(1, 3))
image(t(dataMatrix0rdered)[, nrow(dataMatrix0rdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

#Components of the SVD - VAriance explained
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)

#Relationship to pri9ncipal components
svd1 <- svd(scale(dataMatrix0rdered))
pca1 <- prcomp(dataMatrix0rdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1")
abline(c(0, 1))

#Components of the SVD - variance explained
constantMatrix <- dataMatrix0rdered*0
for(i in 1:dim(dataMatrix0rdered) [1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMAtrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)

#What if we add a second pattern?
set.seed(678910)
for (i in 1:40) {
     #flip a coin
     coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
     coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
     #if coin is heads add a common pattern to that row
     if (coinFlip1) {
        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
        }
        if (coinFlip2) {
           dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
           }
}
hh <- hclust(dist(dataMatrix))
dataMatrix0rdered <- dataMatrix[hh$order, ]  

#Singular value decomposition - true patterns
svd2 <- svd(scale(dataMatrix0rdered))
par(mfrow = c(1, 3))
image(t(dataMatrix0rdered)[, nrow(dataMatrix0rdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")

#d and variance explained
svd1 <- svd(scale(dataMatrix0rdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "PErcent of variance explained", pch = 19)

#MIssing values
library(impute)
dataMatrix2 <- dataMatrix0rdered
dataMatrix2[sample(1:100,size=40,replace=FALSE)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrix0rdered)); svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2)); plot(svd1$v[,1],pch=19); plot(svd2$v[,1],pch=19)

#Example
load("data/face.rda")
image(t(faceData)[, nrow(faceData):1])

#Face example - variance explained
svd <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")

svd1 <- svd(scale(faceData))
## Note that %*% is matrix multiplication

#Here svd1$d[1] is a constant
approx1 <- svd1$u[, 1] %*% t(svd1$v[, 1]) * svd1$d[1]

#In these examples we need to make the diagonal matrix ou of d
approx5 <- svd1$u[, 1:5] %*% diag(svd1$d[1:5]) 5*% t(svd1$v[, 1:5])
approx10 <- svd1$u[, 1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[, 1:10])
par(mfrow = c(1, 4))
image(t(approx1)[, nrow(approx1):1], main = "(a)")
image (t(approx5)[, nrow(approx5):1], main = "(b)")
image(t(approx10)[, nrow(approx10):1], main = "(c)")
image(t(faceData)[, nrow(faceData):1], main = "(d)")  #original data

### Plotting and colot in R
# The function pal gives a sequence of collors
pal(seq(0, 1, len = 10))
pal <- colorRampPalette(c("red", "yellow"))
pal(10)

# The smoothScatter function
x <- rnorm(10000)
y <- rnorm(10000)

plot(x, y, pch = 19)
plot(x, y, col= rgb(0, 0, 0, 0.2), pch = 19)
smothScatter(x, y)