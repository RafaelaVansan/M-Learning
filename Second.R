data = read.table("ex2data1.txt",header=F,sep=",",dec=".")
X = data[ ,c(1, 2)]
y = data[, 3]

#We can separe the data to plot
plot(data[y==1,1],data[y==1,2],col="lightpink1",lwd=5,cex=.2)
points(data[y==0,1],data[y==0,2],col="lightblue",lwd=5,cex=.2)


#And we can also do it using ggplot
library(ggplot2)
names(data) <- c("x1", "x2", "y")
p1 = ggplot(data, aes(x=x1,y=x2,colour = as.factor(y))) +  geom_point()
plot(p1)


#In this part of the exercise, you will implement the cost and gradient for logistic regression.
#Setup the data matrix appropriately, and add ones for the intercept term
m = dim(X)[1]
n = dim(X)[2]


#Add intercept term to x and X_test
X = cbind(1,X)

#Initialize fitting parameters
initial_theta = rep(0,3)

#Compute sigmoid function

sigmoid = function(z){
  g = rep(0,length(z))
  g=1/(1 + exp(-z))
return(g)
}

#Cost function
J = 0
grad = rep(0,length(theta))
costFunction = function(theta, X, y){
  J = (1/m)*(sum((-y)%*%log(sigmoid(X%*%theta))-(1-y)%*%log(1 - sigmoid(X%*%theta))))
  grad = (1/m)*(t(sigmoid(X%*%theta) - y)%*%X)

return(list(J,grad))
}

costFunction(initial_theta, X, y)
costFunction(theta, X, y)
test_theta = c(-24, 0.2, 0.2)
costFunction(test_theta, X, y)[1]
costFunction(test_theta, X, y)[2]
