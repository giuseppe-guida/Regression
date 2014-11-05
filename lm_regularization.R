#1.2
#Multivariate linear regression

rm(list=ls()); #clear memeory

require("lattice");
source("C:/Users/g.guida/workspace/Regression/f_lm_regularization_GD.R"); #if the function is located in the r workspace directory
 

#Loading a file
file = "ex1data2.txt";
ex2 = read.csv(file, header=F);


#plotting data
xyplot(V1 ~ V2, data = ex2);

#Source Matrix
m <- do.call(cbind, ex2);
summary(m);
m <- scale(m);
ones <- matrix(1, nrow(m), 1);
X <- cbind(ones, m);
colnames(X) = c("ones", "Pop", "Type", "Prof");

alpha = 0.01;
iterations = 100;
lambda = 0.01;
error_treshold = 0.01;

gdm <- f_lm_regularization_GD(X[,-4], X[4], alpha, lambda, iterations, error_treshold);


forecast <- (gdm$theta[1] + gdm$theta[2]*X[,"Pop"] + gdm$theta[2]*X[,"Type"] );

par(mfrow = c(1, 2), pch = 20)
plot(1:iterations, gdm$error_tracking, main = "Error");
plot(X[,"Pop"], forecast);

#plot(X[,"Pop"], forecast);
#lines(X[,"Pop"], forecast, type="l");




