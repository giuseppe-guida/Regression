# TODO: Add comment
# 
# Author: g.guida
###############################################################################


f_logistic_regression_GD.R <- function(X, Y, alpha, lambda, iterations, error_treshold) {
	
	itemset_length <- length(Y);
	features_number <- ncol(X);
	temp_theta <- vector(length=features_number);
	
	#THETA is a matrix with one column and rows equal to the number of features
	THETA = matrix(0, features_number, 1)
	
	error_tracking <- vector(mode = "double", length = iterations); #measure the error according to 
	error_tracking[1] <- 0;
	
	delta_error = 99; #inizialization at a random high value
	
	#for (i in 1:iterations && delta_error > error_treshold) {
	for (i in 1:iterations) {
		
		exponent <- t(t(THETA) %*% t(X))
		hipothesis = y <- 1 / (1+exp(exponent));
		error = ( (Y - log(hipothesis)) + ((1-Y)*log(1-hipothesis)));
		#error = hipothesis - Y;
		
		sse = -1 * (sum(error)/(itemset_length));
		error_tracking[i] <- sse;
		
		#error_tracking[i] <- sse;
		if (i==1) { delta_error <- sse; }
		else { delta_error <- abs(sse - error_tracking[i-1]); }
		
		
		for (j in 1:features_number ) {
			res = error * X[ ,j];
			temp_theta[j] = THETA[j]-( sum(res)*alpha );
		}
		
		THETA <- rep(temp_theta);
	}
	
	print(THETA[1]);
	print(THETA[2]);
	print(THETA[3]);
	
	#returning results
	return_list <- list("theta"=THETA, "error_tracking" = error_tracking);
	return(return_list);
	
	
}
