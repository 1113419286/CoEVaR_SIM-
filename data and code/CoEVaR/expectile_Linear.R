# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory



library(SALES)
linear <- function(x,y){
  res <- cv.ernet(y = y, x = x, tau = 0.05, eps = 1e-8,standardize = FALSE, intercept = FALSE,
                  lambda2 = 1,nfolds = 3)
  lambda = res$lambda.min
   beta = as.vector(coef(res, s = "lambda.min", type = c("coefficients")))
   if(sum(beta^2) != 0){
     beta = beta/sqrt(sum(beta^2))
   }
  return(list(lambda =lambda,beta = beta))
}

