#########################################

library(SALES)
erL1 <- function(x,y,tau=0.05){
  res <- cv.ernet(y = y, x = x, lambda = NULL,tau = tau, eps = 1e-8,standardize = FALSE, intercept = FALSE,
                  nfolds=3,lambda2 = 0) 
  lambda = res$lambda.min
   beta = as.vector(coef(res, s = "lambda.min", type = c("coefficients")))[-1]
  return(list('lambda' =lambda,'beta' = beta))
}

library(Matrix)
ER.nonforecast=function(Y,X,xk,theta,h,max.iter=100,tol=1e-6){
  X=as.matrix(X)
  xk=as.matrix(xk)
  nr=nrow(xk)
  nc=ncol(xk)
  par=par.new=matrix(0,nr,(nc+1))
  iter=0
  par.deta=1
  
  while (par.deta>=tol) {
    iter=iter+1
    par=par.new
    
    for (i in 1:nr) {
      x=xk[i,]
      Z=cbind(1,t(t(X)-x)) #n*(k+1)
      R1=dnorm((t(t(X)-x))/h) #n*k
      R=diag(diag(abs(matrix((Y<=Z%*%par[i,])-theta))%*%t(apply(R1, 1, 'prod')))) #n*n
      Am=t(Z)%*%R%*%Z
      if(min(eigen(Am)$values)<=1e-8){Am=as.matrix(nearPD(Am)$mat)} #singular
      par.new[i,]=solve(Am)%*%(t(Z)%*%R%*%Y)#(k+1)*1
    }
    
    par.deta=mean(abs(par.new[,1]-par[,1]))
    if(iter>max.iter){
      print('cannot convergence')
      break
    } 
  }
  return(list('y.theta'=par.new[,1],'par'=par.new[,-1],'iter'=iter))
}

sim = function(Qy, Qxx, tau = 0.05,Qmaxiter=50, LVaRest) {
# set the critical value to control the iteration
crit  = 0.01

beta.t = 0
while (beta.t == 0) {
  fit = erL1(Qxx,Qy)
  beta.t = sum(fit$beta^2)
}

# the initial estimated betas
beta.in = fit$beta
# the initial estimated penalization parameter
lambda_in = fit$lambda
d = length(beta.in)
n = NROW(Qy)
# standardize the initial betas
beta.in  = beta.in/sqrt(sum(beta.in^2))

# initialize the new estimated betas in the iterations
beta.new = rep(0, d)
# initialize the number of iteration
iter = 1
# initialize the link function 'a' and its first derivative 'b'
a.in = rep(0, n)
b.in = rep(0, n) # b = 

# start the iteration
while ((iter < Qmaxiter) & (sum((beta.new - beta.in)^2) > crit)) {
  
  print(iter)
  if (iter > 1) {
    beta.in = beta.new
  }
  
  # step 1: compute a.in and b.in
  index_x = rowSums(t(t(Qxx) * beta.in))
  #calculate bandwidth
  yorder  = Qy[order(index_x)]
  xorder  = sort(index_x)
  hm = tryCatch({
    dpill(xorder[1:(length(index_x))], yorder[1:(length(index_x))])
  }, error=function(e){
    tryCatch(0.1)
  }) 
  hm[is.na(hm)] = 0.1
  hp = 10 * hm * (tau * (1 - tau)/(dnorm(qnorm(tau)))^2)^0.2
  
  fit = ER.nonforecast(Qy,index_x,index_x,theta = tau,h=hp)
  a.in = fit$y.theta
  b.in = fit$par
  
  # step 2: compute beta.new
  ynew = rep(0, n^2)
  xnew = rep(0, n^2 * d)
  xnew = matrix(xnew, ncol = d)
  ynew = (rep(Qy, each = n) - rep(a.in, times = n)) #y-a
  xnew = rep(b.in, times = n) * (Qxx[rep(1:n, each = n), ] - Qxx[rep(1:n,times = n), ]) #b*(x-x')
  wts  = dnorm(rowSums(t(t(xnew) * beta.in))/hp) #kernel-- k(beta*x), k is normal
  ynew = sqrt(wts) * ynew #k*y 
  wtss = replicate(d, wts)
  xnew = xnew * sqrt(wtss) #k*xnew
  fit  = erL1(xnew, ynew)

  beta.new   = fit$beta
  if(sum(beta.new^2) != 0){
    beta.new   = beta.new/sqrt(sum(beta.new^2))
  }
  
  lambda_new = fit$lambda
  iter       = iter + 1
}

print(lambda_new)
print(which(beta.new != 0))
index_final  = rowSums(t(t(Qxx) * beta.new))

# # bandwdiath--> linkfunction
# orderx       = sort(index_final)
# ordery       = Qy[order(index_final)]
# hm = tryCatch({
#   dpill(xorder[1:(length(index_final))], yorder[1:(length(index_final))])
# }, error=function(e){
#   tryCatch(0.08)
# }) 
# hm[is.na(hm)] = 0.08
# value_x       = seq(min(index_final), max(index_final), length = length(Qy))
# linkfunest    = matrix(0, length(value_x), 1)
# for (i in 1:length(value_x)) {
#   fit2          = lprq0(index_final, Qy, hp, Qp, value_x[i])
#   linkfunest[i] = fit2$fv
# }

index_est = LVaRest %*% beta.new
fit1      = ER.nonforecast(Qy, index_final, index_est, theta=tau, h=hp)
# a.fi is the estimated g(), i.e. estimated CoVaR
a.fi = fit1$y.theta
# b.fi is the estimated g'()
b.fi = fit1$par
# c.fi is the estimated betas multiply g'(), i.e. the estimated partial
# derivatives
c.fi = b.fi * beta.new

finalresults            = list() 
finalresults$beta_final = beta.new
finalresults$lambda.fi  = lambda_new
finalresults$a.fi = a.fi
finalresults$b.fi = b.fi
finalresults$c.fi = c.fi
return(finalresults)
}

