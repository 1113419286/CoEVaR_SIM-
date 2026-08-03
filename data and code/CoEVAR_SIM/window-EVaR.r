# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR/')

data=as.data.frame(read.csv('logre.csv')[,-1])

expTLlaws <- function(X, y, tau,  max.iter=1000, tol=1e-4)
{
  # X is the model matrix
  # y is the response vector of observed proportion
  # maxIter is the maximum number of iterations
  # tol is a convergence criterion
  X <- cbind(1, X) # add constant
  b <- bLast <- rep(0, ncol(X)) # initialize
  it <- 1 # iteration index
  while (it <= max.iter){
    
    ypred <- c(X %*% b)
    w <- as.vector(tau *(y>= ypred) + (1-tau)* (y<ypred))
    b <- lsfit(X, y, w, intercept=FALSE)$coef
    if (max(abs(b - bLast)/(abs(bLast) + 0.01*tol)) < tol) break
    bLast <- b
    it <- it + 1 # increment index
  }
  if (it > max.iter) warning('maximum iterations exceeded')
  list(coefficients = b,it = it,yhat = as.numeric(X%*%b ) )
}



# read the macro state variables
data_m     = as.matrix(data[, 31:39])

# read the log returns of 50 financial institutions
data_y     = as.matrix(data[, 1:30])

# set the quantile level
tau        = 0.05

nncol      = ncol(data_y)
nnrow      = nrow(data_y)
lengthfull = nnrow

# the moving window size is 48,equal to 1 years
winsize    = 48
VaR        = matrix(0, ncol = nncol, nrow = (lengthfull - winsize+1))

# start the moving window VaR prediction, store the predict values
for (j in 1:nncol) {
  for (i in 1:(lengthfull - winsize+1)) {
    ycut   = data_y[i:(i + winsize-1), j]
    xcut   = data_m[i:(i + winsize-1), ]
    xxcut  = matrix(0, nrow(xcut), ncol(xcut))
    # standardize macro state variables
    for (k in 1:ncol(xcut)) {
      xxcut[, k] = (xcut[, k] - min(xcut[, k]))/(max(xcut[, k]) - min(xcut[, k]))
    }
    fit       = expTLlaws(xxcut,ycut,tau)
    pre       = fit$yhat
    VaR[i, j] = pre[length(pre)]
  }
}
EVaR         = round(VaR, digits = 9)
write.csv(EVaR,file = "Results/EVaR_movingwindows.csv",row.names = F)

