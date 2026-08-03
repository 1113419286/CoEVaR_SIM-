# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
library(SALES)
library(Matrix)
source("expectile_SIM.r")
# read the file which includes log returns of 100 firms and 7 macro state
# variables

lgre=read.csv("logre_rs.csv")[,-1]
#get date
{
  # ws = 48
  # date0=read.csv("logre_rs.csv")[,1]
  # date1=date0[c(-1:-(ws-1))] #start of EVaR
  # write.csv(date0,'date_rslogre.csv',row.names = F)
  # write.csv(date1,'date_rsEVaR.csv',row.names = F)
}


x0  = lgre #window of EVaR
m   = as.matrix(x0[, 36:44])
# Value at Risk of 30 stock
EVaR = as.matrix(read.csv("rs_results/rsEVaR_movingwindows.csv"))[, 1:35]
# log returns of 30 stock
xx0 = x0[, 1:35]
# start the linear quantile lasso estimation for each stock
for (k in 1:35) {

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
  
  # lambda calculated from SIM expectile lasso
  lambda_sim       = matrix(0, (n - ws+1), 1)
  # coefficients betas calculated from SIM expectile lasso
  beta_sim         = matrix(0, (n - ws+1), p)
  # estimated Conditional Value at Risk from SIM expectile lasso
  Coevar_sim      = matrix(0, (n - ws+1), 1)
  # the estimated g'()
  first_der   = matrix(0, (n - ws+1), 1)
  # the estimated partial derivatives g'()*beta
  partial_der = matrix(0, (n - ws+1), p)
  
  # start the moving window estimation
  for (i in 1:(n - ws+1)) {
    print(i)
    yw  = y[i:(i + ws-1)]
    MBw = MB[i:(i + ws-1), ] #have been 1 lag
    mb  = matrix(0, ws, ncol(MB))
    # standardize macro state variables 
    for (j in 1:ncol(MB)) {
      mb[, j] = (MBw[, j] - min(MBw[, j]))/(max(MBw[, j]) - min(MBw[, j]))
    }
    mb[is.na(mb)] <- 0
    xx          = xx1[i:(i + ws-1), ]
    # all the independent variables
    xxw         = cbind(xx, mb)
    VaRM_est    = as.numeric(c(V[i, ], mb[ws, ]))
    fit  = sim(yw, xxw, tau, Qmaxiter = 2, LVaRest = VaRM_est)

    lambda_sim[i]    = fit$lambda.fi
    beta_sim[i, ]    = fit$beta_final
    # the final estimated CoVaR
    Coevar_sim[i]     = fit$a.fi
    first_der[i]     = fit$b.fi
    # the estimated partial derivatives
    partial_der[i, ] = fit$c.fi
  }
  write.csv(beta_sim, file   = paste("rs_results/beta_sim", k, ".csv", sep = ""))
  write.csv(Coevar_sim, file  = paste("rs_results/Coevar_sim", k, ".csv", sep = ""))
  write.csv(lambda_sim, file = paste("rs_results/lambda_sim", k, ".csv", sep = ""))
  write.csv(first_der, file = paste("rs_results/first_der", k, ".csv", sep = ""))
  write.csv(partial_der, file = paste("rs_results/partial_der", k, ".csv", sep = ""))
} 


Qy=yw
Qxx=xxw

