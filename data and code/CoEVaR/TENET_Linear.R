# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR//')

# install and load packages
library(SALES)

source("CoEVaR/expectile_Linear.r")
# read the file which includes log returns of 100 firms and 7 macro state
# variables
lgre=read.csv("logre.csv")[,-1]
date0=read.csv("logre.csv")[,1]
date1=date0[c(-1:-47)] #start of EVaR
write.csv(date0,'date_logre.csv',row.names = F)
write.csv(date1,'date_EVaR.csv',row.names = F)


x0  = lgre #window of EVaR
m   = as.matrix(x0[, 31:39])
# Value at Risk of 30 stock
EVaR = as.matrix(read.csv("Results/EVaR_movingwindows.csv"))[, 1:30]
# log returns of 30 stock
xx0 = x0[, 1:30]
# start the linear quantile lasso estimation for each stock
for (k in 1:30) {
  cat("Firm:", k)
  # log return of stock k
  y              = as.matrix(xx0[, k])
  # log returns of stocks except stock k
  xx1            = as.matrix(xx0[, -k])  
  # combine macro state variables and stock characteristics
  MB             = m
  # number of rows of log return
  n              = nrow(xx1)
  # number of covariates
  p              = ncol(xx1) + ncol(MB)
  # Value at Risk of stock except stock k
  V              = as.matrix(EVaR[, -k])
  # quantile level
  tau            = 0.05
  # moving window size
  ws             = 48
  
  # lambda calculated from linear quantile lasso
  lambda_l       = matrix(0, (n - ws+1), 1)
  # coefficients betas calculated from linear quantile lasso
  beta_l         = matrix(0, (n - ws+1), p)
  # estimated Conditional Value at Risk from linear quantile lasso
  CoVaR_l        = matrix(0, (n - ws+1), 1)
  
  # start the moving window estimation
  for (i in 1:(n - ws+1)) {
    print(i)
    yw  = y[i:(i + ws-1)]
    MBw = MB[i:(i + ws-1), ]
    mb  = matrix(0, ws, ncol(MB))
    # standardize macro state variables 
    for (j in 1:ncol(MB)) {
      mb[, j] = (MBw[, j] - min(MBw[, j]))/(max(MBw[, j]) - min(MBw[, j]))
    }
    mb[is.na(mb)] <- 0
    xx          = xx1[i:(i + ws-1), ]
    # all the independent variables
    xxw         = cbind(xx, mb)
    VaRM_est    = as.numeric(c(1,V[i, ], mb[ws, ]))
    fit         = linear(xxw,yw)
    lambda_l[i] = fit$lambda
    beta_l[i, ] = fit$beta[-1]
    CoVaR_l[i]  = as.numeric(VaRM_est%*%fit$beta) 
  }
  write.csv(beta_l, file   = paste("Results/beta_L", k, ".csv", sep = ""))
  write.csv(CoVaR_l, file  = paste("Results/CoEVaR_L", k, ".csv", sep = ""))
  write.csv(lambda_l, file = paste("Results/lambda_L", k, ".csv", sep = ""))
} 
