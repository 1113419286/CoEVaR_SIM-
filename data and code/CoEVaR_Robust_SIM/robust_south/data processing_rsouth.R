# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

library(DMwR)
library(readxl)
setwd('~/COEVAR/CoEVaR_Robust_SIM/robust_south/')
stock.raw=as.data.frame(read_excel('data/data_total_rs.xlsx'))
macro.data=as.data.frame(read_excel('data/macro_data.xlsx'))

date0 = stock.raw[,1]
stock.imputdata=knnImputation(stock.raw[,-1])

write.csv(data.frame(stock.raw[,1],stock.imputdata),'data/stock_knninput.csv',row.names =F)

macro.logre=data.frame('date'=macro.data[-1,1],diff(as.matrix(log(macro.data[,-1])))*100)

stock.logre=data.frame('date'=date0[-1],diff(as.matrix(log(stock.imputdata)))*100)

date.lag=date0[-length(date0)]

# macro is lag
lgre=data.frame(stock.logre,macro.logre[which(date.lag[1]<= macro.logre$date & 
                                                macro.logre$date<= date.lag[length(date.lag)]),-1])

write.csv(lgre,'logre_rs.csv',row.names = F)


library(tseries)
library(FinTS)
library(moments)

#descriptive statistics
summ=function(X){
  name=colnames(X)
  
  sumfun=function(x){
    matrix(c(mean(x),median(x),max(x),min(x),var(x)^0.5,skewness(x),
             kurtosis(x),(jarque.bera.test(x))$statistic,(jarque.bera.test(x))$p.value,(adf.test(x))$statistic,(adf.test(x))$p.value),11,1)}
  
  sum1=apply(X, 2, sumfun)
  dimnames(sum1)=list(c("mean","median","maximun","minimum","s.d.","skeweness","kurtosis","JB",'p.value','ADF','p.value'),name)
  
  return(round(sum1,3)) #round ????3λ
}



lgre.sum=t(summ(lgre))
write.csv(lgre.sum,'logre_summary.csv')

