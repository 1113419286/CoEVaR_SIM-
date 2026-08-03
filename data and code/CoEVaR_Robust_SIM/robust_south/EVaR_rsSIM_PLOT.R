
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
setwd('~/COEVAR/CoEVaR_Robust_SIM/robust_south/')

## monthly
########################################################
## monthly plot
library(dplyr)
library(lubridate)

## total group in month
{
  tc_group = read.csv('rs_results/total_in_and_out_rs.csv')
  date = as.Date(read.csv('date_rsEVaR.csv')[,1])
  tc_group = data.frame('date' = date, tc_group)
  
  tc_group %>% mutate(month = floor_date(date,"month")) %>%
    group_by(month) %>%
    summarise('month_Americas_out' = mean(Americas_out),
              'month_Europe_out' = mean(Europe_out),
              'month_Asia_out' = mean(Asia_out),
              'month_Americas_in' = mean(Americas_in), 
              'month_Europe_in' = mean(Europe_in),
              'month_Asia_in' = mean(Asia_in)) -> tc_group_m
  
  tc_group_m = data.frame(as.data.frame(tc_group_m), 
                          'total_out' = rowSums(tc_group_m[,2:4])) #except Africa
  
  write.csv(tc_group_m, 'rs_results/tc_group_month_rs.csv', row.names = FALSE)
  
}





### out and in risk plot in month (bar)
tc_group_m = read.csv('rs_results/tc_group_month_rs.csv')

# total out
{
  svg(paste("rs_results/total_risk_month_rs.svg", sep = ""),width = 12 , height = 6 ,family="GB1")

par(mar=c(2,4.5,0.3,0.3))
plot(as.Date(tc_group_m$month), tc_group_m$total_out, type = "l", col = "black", lwd = 4, lty=1,
     ylab = "总和风险水平", xlab = "", cex.axis=2, cex.lab=2,
     font.axis = 2, ylim = c(25, 2000), mgp = c(2.5, 0.5, 0))

dev.off()
}


# area total in
{
  svg(paste("rs_results/risk_in_month_rs.svg", sep = ""),width = 12 , height = 6 ,family="GB1")

par(mar=c(2,4.5,1.3,0.3))
plot(as.Date(tc_group_m$month), tc_group_m$month_Americas_in, type = "l", col = "#e20612", lwd = 4, lty=1,
     ylab = "尾部风险接收水平", xlab = "", cex.axis=2,cex.lab=2,
     font.axis = 2,ylim = c(0,200),mgp = c(2.5, 0.5, 0)) #America

lines(as.Date(tc_group_m$month), tc_group_m$month_Europe_in, lwd = 4, col ="#ffd401",lty=2)

lines(as.Date(tc_group_m$month), tc_group_m$month_Asia_in, lwd = 4, col ="#00b0eb",lty=3)

lines(as.Date(tc_group_m$month), tc_group_m$month_Africa_in,lwd = 4, col ="#9b3a74",lty=4)

legend("topright", inset=0, legend=c("美洲", "欧洲", "亚洲",'非洲'),
       col=c("#e20612","#ffd401","#00b0eb","#9b3a74"),lty=c(1,2,3,4),
       lwd=c(4,4,4,4),box.lty=1,cex = 1.5)

dev.off()
}

# area total out
{
  svg(paste("CoEVaR_SIMplot/risk_out_month_rs.svg", sep = ""),width = 12 , height = 6 ,family="GB1")

par(mar=c(2,4.5,1.3,0.3))
plot(as.Date(tc_group_m$month), tc_group_m$month_Americas_out, type = "l", col = "#e20612", lwd = 4, lty=1,
     ylab = "尾部风险溢出水平", xlab = "", cex.axis=2,cex.lab=2,
     font.axis = 2,ylim = c(0,800),mgp = c(2.5, 0.5, 0)) #America

lines(as.Date(tc_group_m$month), tc_group_m$month_Europe_out, lwd = 4, col ="#ffd401",lty=2)

lines(as.Date(tc_group_m$month), tc_group_m$month_Asia_out, lwd = 4, col ="#00b0eb",lty=3)

lines(as.Date(tc_group_m$month), tc_group_m$month_Africa_out,lwd = 4, col ="#9b3a74",lty=4)

legend("topright", inset=0, legend=c("美洲", "欧洲", "亚洲",'非洲'),
       col=c("#e20612","#ffd401","#00b0eb","#9b3a74"),lty=c(1,2,3,4),
       lwd=c(4,4,4,4),box.lty=1,cex = 1.5)

dev.off()
}


