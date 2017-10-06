data(iris)

irissubdf <- iris[1:100,c(1,3,5)]

names(irissubdf) <- c("sepal", "petal", "species")
head(irissubdf)

library(ggplot2)
ggplot(irissubdf, aes(x = sepal, y = petal)) + 
  geom_point(aes(colour=species, shape=species), size = 3) +
  xlab("sepal length") + 
  ylab("petal length") + 
  ggtitle("Species vs sepal and petal lengths")

irissubdf[,4] <- 1
irissubdf[irissubdf[,3] == "setosa",4] <- -1

x <- irissubdf[,c(1,2)] 
y <- irissubdf[,4]

head(x)
head(y)


perceptron <- function(x, y, eta, niter) {
  
  # initialize weight vector
  weight <- rep(0, dim(x)[2] + 1)
  errors <- rep(0, niter)
  
  
  # loop over number of epochs niter
  for (jj in 1:niter) {
    
    # loop through training data set
    for (ii in 1:length(y)) {
      
      # Predict binary label using Heaviside activation 
      # function
      z <- sum(weight[2:length(weight)] * 
                 as.numeric(x[ii, ])) + weight[1]
      if(z < 0) {
        ypred <- -1
      } else {
        ypred <- 1
      }
      
      # Change weight - the formula doesn't do anything 
      # if the predicted value is correct
      weightdiff <- eta * (y[ii] - ypred) * 
        c(1, as.numeric(x[ii, ]))
      weight <- weight + weightdiff
      
      # Update error function
      if ((y[ii] - ypred) != 0.0) {
        errors[jj] <- errors[jj] + 1
      }
      
    }
  }
  
  # weight to decide between the two species 
  print(weight)
  return(errors)
}

err <- perceptron(x, y, 1, 10)

plot(1:10, err, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors")
title("Errors vs epoch - learning rate eta = 1")

# iris data subset
irisdata <- iris[, c(1, 3, 5)]
names(irisdata) <- c("sepal", "petal", "species")

# ggplot the data
ggplot(irisdata, aes(x = sepal, y = petal)) + 
  geom_point(aes(colour=species, shape=species), size = 3) +
  xlab("sepal length") + 
  ylab("petal length") + 
  ggtitle("Species vs sepal and petal lengths")


# subset of properties of flowers of iris data set
x <- iris[, 1:4] 
names(x) <- tolower(names(x))

# create species labels
y <- rep(-1, dim(x)[1])
y[iris[, 5] == "virginica"] <- 1

# compute and plot error
err <- perceptron(x, y, 0.01, 50)

plot(1:50, err, type="l", lwd=2, col="red", xlab="epoch #", ylab="errors")
title("Errors in differentiating \n Virginica vs epoch - learning rate eta = 0.01")