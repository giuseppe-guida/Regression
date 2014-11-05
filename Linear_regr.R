#1.1
rm(list=ls()); #clear memeory

library("lattice");
source("C:/Users/g.guida/workspace/Regression/linear_regression_GD.R"); #if the function is located in the r workspace directory


#Loading a file
file = "ex1data1.txt";
ex1 = read.csv(file, header=F, col.names=c("Population","Profit"));

#plotting data
xyplot(Profit ~ Population, data = ex1)

#Source Matrix
Population = matrix(ex1[[1]], ncol=1, dimnames=list(NULL, "Population")); 
Profit = matrix(ex1[[2]], ncol=1, dimnames=list(NULL,"Profit")); 
m = cbind(Population, Profit);

summary(Population)
summary(Profit)

#Gradient Descent

#Building X and Y matrix in order to find the best fitting theta parameters
# y = theta0 + theta1*X

ones = matrix(1, nrow(Population), 1);
X = cbind(ones, Population);
colnames(X) = c("Ones","Population");

alpha = 0.0001;
iterations = 1500;
error_treshold = 0.0000000000000001;

gd <- linear_regression_GD(X, Profit, alpha, iterations, error_treshold);


#Plotting the result
forecast <- (gd$theta[1] + gd$theta[2]*Population);

plot(Population, Profit,col=2); par(new=T);
plot(Population, forecast, type="b"); par(new=F);
dev.off();

png(filename="Linear_regression_GD.png", bg="transparent");
plot(Population, Profit,col=2); par(new=T);
plot(Population, forecast, type="b"); par(new=F);
dev.off();



png(filename="Error_linear_regression_GD.png", bg="transparent");
plot(1:iterations, gd$error_tracking, type="b");


plot(1:iterations, gd$error_tracking, type="b");
dev.off();
