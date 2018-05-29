# M-Learning

data = read.table("ex1data1.txt",header=F,sep=",",dec=".")
X = data[, 1]
y = data[, 2]
m = length(y)
library(ggplot2)
qplot(X,y)

X = cbind(1, data[,1]) #Add a column of ones to x
theta = rep(0,2) #initialize fitting parameters

# Some gradient descent settings
iterations = 1500
alpha = 0.01

#Cust function
J = 0
computeCost = function(X, y, theta) {
  J = sum(((X%*%theta - y)^2))/(2*m)

return (J)
}

J2 = computeCost(X, y, c(-1,2))

gradientDescent = function(X, y, theta, alpha, num_iters){
  for (iter in 1:num_iters){
    aux = t(X)%*%(X%*%theta - y)
    theta = theta - (alpha/m)*aux
  }
return(theta)
}
theta = gradientDescent(X, y, theta, alpha, iterations)

qplot(X[,2],y) #Data
qplot(X[,2], X%*%theta,geom="line") #Linear fit


# Predict values for population sizes of 35,000 and 70,000
predict1 = c(1, 3.5)%*%theta
predict1=predict1*10000
predict2 = c(1, 7)%*%theta
predict2=predict2*10000


# Grid over which we will calculate J
theta0_vals = seq(-10, 10, 0.2001)
theta1_vals = seq(-1, 4, 0.05001)

# initialize J_vals to a matrix of 0
J_vals = matrix(c(rep(0,length(theta0_vals)),rep(0,length(theta1_vals))), nrow=length(theta0_vals),ncol=length(theta1_vals))
# Fill out J_vals
for (i in 1:length(theta0_vals)){
    for (j in 1:length(theta1_vals)){
	  t = c(theta0_vals[i], theta1_vals[j])
	  J_vals[i,j] = computeCost(X, y, t)
    }
}	  

J_vals = t(J_vals)
#Surface plot
install.packages("plot3D")
library(plot3D)
surf3D(theta0_vals, theta1_vals,J_vals)

plot(theta[1],theta[2])
