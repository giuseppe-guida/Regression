# TODO: Add comment
# 
# Author: g.guida
###############################################################################
sigmoid <- function(z) {
	return(1/(1 + exp(-z)));
}

f_logistic_regression_GD.R <- function(X, Y, alpha, lambda, iterations, error_treshold) {
	
	itemset_length <- length(Y);
	features_number <- ncol(X);
	temp_theta <- vector(length=features_number);
	
	#THETA is a matrix with one column and rows equal to the number of features
	THETA = matrix(0, features_number, 1)
	
	cost_tracking <- vector(mode = "double", length = iterations); #measure the error according to 
	cost_tracking[1] <- 0;
	
	delta_error = 99; #inizialization at a random high value
	
	#for (i in 1:iterations) {
	i <- 1;
	while (i<=iterations) { #&& (delta_error >= error_treshold) ) {
		
		hipothesis <- 1/(1 + exp(-(X %*% THETA)));
		
		#calculating the new coeficients
		for (j in 1:features_number ) {
			res = (hipothesis - Y) * X[ ,j];
			temp_theta[j] = THETA[j] - alpha*sum(res);
		}
		
		#calculating the error
		cost = -1/itemset_length * sum( Y*log(hipothesis) + (1-Y)*log(1-hipothesis) );
		cost_tracking[i] <- cost;
		
		if (i==1) { delta_error <- cost; }
		else { delta_error <- abs(cost - cost_tracking[i-1]); }
		
		THETA <- rep(temp_theta);
		
		i<-i+1;
	}
	
	print(THETA[1]);
	print(THETA[2]);
	print(THETA[3]);
	
	#returning results
	return(list("theta"=THETA, "cost_tracking"=cost_tracking));	
}
