#1.3
#Multivariate linear regression

rm(list=ls()); #clear memeory

library("lattice");
source("C:/Users/g.guida/workspace/Regression/f_logistic_regression_GD.R"); #if the function is located in the r workspace directory


#Loading a file
file = "C:/Users/g.guida/workspace/Regression/ex2data1.txt";
ex2 = read.table(file, header = TRUE, sep = ",", 
		dec = ".",
		colClasses = c("numeric", "numeric", "integer")
		,quote = "\"'",
);

#plotting
par(new=F, mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
xrange <- yrange <- range(ex2$Exam_Score_1, ex2$Exam_Score_2);
with(ex2,
plot(Exam_Score_1, Exam_Score_2,type = "n",
	,xlab = "Exam_Score_1", ylab = "Exam_Score_2",
	ylim = yrange, xlim = xrange));
with(subset(ex2, Admitted == 1), points(Exam_Score_1, Exam_Score_2, col = "blue", pch = 20));
with(subset(ex2, Admitted == 0), points(Exam_Score_1, Exam_Score_2, col = "red", pch = 20));
legend("topright", pch = 20, col = c("blue", "red"), legend = c("Admitted", "Not Admitted"))

#Source Matrix
m <- do.call(cbind, ex2);
summary(m);
ones <- matrix(1, nrow(m), 1);
M <- cbind(ones, m);
colnames(M) = c("ones", "score1", "score2", "adm");

alpha = 0.0000001;
iterations = 1500;
error_treshold = 0.01;

gdm <- f_logistic_regression_GD.R(M[,-4], M[4], alpha, lambda, iterations, error_treshold)

forecast <- (gdm$theta[1] + gdm$theta[2]*M[,"score1"] + gdm$theta[3]*M[,"score2"] );
classification <- ifelse(forecast>0, 1, 0);
#error
plot(1:iterations, gdm$cost_tracking, main = "Cost function");
