linear_regression_GD <- function(X, Y, alpha, iterations, error_treshold) {
	
	itemset_length = nrow(Y);
	
	THETA = matrix(0, 2, 1, dimnames=list(c("T1","T2"), NULL))
	
	error_tracking <- vector(mode = "double", length = iterations); #measure the error according to 
	
	
	delta_error = 99;
	
	for (i in 1:iterations && delta_error >= error_treshold) {
		
		hipothesis = t(t(THETA) %*% t(X));
		error = hipothesis - Y;
		
		sse = sum(error^2)/(2*itemset_length);
		error_tracking[1] <- sse;
		if (i!=1) { delta_error <- error_tracking[i] - sse; }
		else { delta_error <- sse; }
		
		res = error * X[,1];
		reduction = sum(res)*alpha/itemset_length;
		t1 = THETA[1,1] - reduction;
		
		res = error * X[,2];
		reduction = sum(res)*alpha/itemset_length;
		t2 = THETA[2,1] - reduction;
		
		THETA[1,1] = t1;
		THETA[2,1] = t2;
		print(i);
		
	}
	
	print(THETA[1,1]);
	print(THETA[2,1]);
	temp <- list("theta"=THETA, "error_tracking" = error_tracking);
	return(temp);
}