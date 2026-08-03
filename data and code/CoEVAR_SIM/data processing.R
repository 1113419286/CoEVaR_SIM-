library(DMwR)
library(readxl)
setwd('~/COEVAR/')

stock.raw=as.data.frame(read_excel('data/data_total.xlsx'))
macro.data=as.data.frame(read_excel('data/宏观变量/o_data.xlsx'))

stock.imputdata=knnImputation(stock.raw[,-1])



write.csv(data.frame(stock.raw[,1],stock.imputdata),'data/stock_input.csv',row.names =F)


macro.logdata=data.frame('date'=macro.data[-1,1],diff(as.matrix(log(macro.data[,-1])))*100)

stock.logimputdata=data.frame('date'=stock.raw[-1,1],diff(as.matrix(log(stock.imputdata)))*100)
n=nrow(stock.logimputdata)

# macro is lag
lgre=data.frame(stock.logimputdata,macro.logdata[1:n,-1])

write.csv(lgre,'logre.csv',row.names = F)


ligetwd()brary(tseries)
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
  
  return(round(sum1,3)) 
}

lgre.sum=t(summ(lgre))
write.csv(lgre.sum,'logre_summary.csv')

