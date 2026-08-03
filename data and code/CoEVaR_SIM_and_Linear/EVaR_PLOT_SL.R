
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR/')

###################################
library(readxl)

logre       = read.csv("logre.csv", header = TRUE)

logre = logre[-c(1:47),]

date_y <- as.Date(logre[,1])

EVaR       = read.csv("Results/EVaR_movingwindows.csv", header = TRUE)

name=as.data.frame(read_excel('name.xlsx'))
name.stock=name[,2]

# EVaR对比图 图1/附录图11
{
  i=1
  for (i in 1:30) {
    ## 对数收益率
    y1 = as.vector(logre[,i+1]) 
    
    ## EVaR
    y3 = as.vector(EVaR[,i])
    
    ## CoEVaR_Linear
    y4 = read.csv(paste0("Results/CoEVaR_L", i, ".csv"), header = T)[,2]
    
    ## CoEVaR_SIM
    y5 = read.csv(paste0("SIM_Results/Coevar_sim", i, ".csv"),header = T)[,2]
    y5[which(abs(y5) > 60)] = (60 + 2*log(abs(y5[which(abs(y5) > 60)]-60)))*sign(y5[which(abs(y5) > 60)])
    
    svg(paste0("CoEVaR_SIM_and_Linear/SL_plot/p",i, '_',name.stock[i], ".svg", sep = ""),
        width = 16 , height = 8 ,family="GB1")
    par(mar = c(2,3,3.5,0.2))
    
    plot(date_y,y1, type = "p", col = "gray60",cex = 1,xaxt='n',
         cex.axis = 1.8, font.axis = 2,
         ylab = "", xlab = "", pch = 20,ylim = c(-80,70)) 
    
    lines(date_y, y4, lwd = 3.5, col ="#ff00ff",lty = 2) #L 
    
    lines(date_y, y5, lwd = 4, col = "#0000ff", lty = 1)#SIM 
    
    lines(date_y,y3 , lwd = 3, col ="#008a00",lty = 4) #EvaR
    
    datey = format(date_y,"%Y")
    aty = match(unique(datey),datey)
    axis(1,date_y[aty[-1]],labels = unique(datey)[-1],mgp = c(2.5, 0.5, 0),
         cex.axis=1.8,font.axis = 2)
    
    legend("bottomright",  
           legend=c("对数收益率","EVaR",expression(CoEVaR^{L}),expression(CoEVaR^{SIM})),inset=0.01,
           col=c("gray60","#008a00","#ff00ff","#0000ff"),lty=c(NA,3,2,1),
           pch = c(20,NA,NA,NA),lwd=c(0,2.5,2.5,2.5),box.lty=1,cex = 1.7)
    #col=c("gray70","#00b0eb","#2e409a","#e20612")
    title(main=name.stock[i],cex.main=3)
    dev.off()
    
  }
  
}


# EVaR对比图 pdf 图1/附录图11
library(Cairo)
library(showtext)
{
  i=1
  for (i in 1:30) {
    ## 对数收益率
    y1 = as.vector(logre[,i+1]) 
    ## EVaR
    y3 = as.vector(EVaR[,i])
    ## CoEVaR_Linear
    y4 = read.csv(paste0("Results/CoEVaR_L", i, ".csv"), header = TRUE)[,2]
    ## CoEVaR_SIM
    y5 = read.csv(paste0("SIM_Results/Coevar_sim", i, ".csv"),header = T)[,2]
    y5[which(abs(y5) > 60)] = (60 + 2*log(abs(y5[which(abs(y5) > 60)]-60)))*sign(y5[which(abs(y5) > 60)])
    
    CairoPDF(paste0("CoEVaR_SIM_and_Linear/pdf/p",i, ".pdf"),
             width = 16 , height = 8)
    showtext_begin()
    par(mar = c(2,3,3.5,0.2))
    
    plot(date_y,y1, type = "p", col = "gray60",cex = 1,xaxt='n',
         cex.axis = 1.8, font.axis = 2,
         ylab = "", xlab = "", pch = 20,ylim = c(-80,70)) 
    
    lines(date_y, y4, lwd = 3.5, col ="#ff00ff",lty = 2) #L
    
    lines(date_y, y5, lwd = 4, col = "#0000ff", lty = 1)#SIM 
    
    
    lines(date_y,y3 , lwd = 3, col ="#008a00",lty = 4) #EvaR
    
    datey = format(date_y,"%Y")
    aty = match(unique(datey),datey)
    axis(1,date_y[aty[-1]],labels = unique(datey)[-1],mgp = c(2.5, 0.5, 0),
         cex.axis=1.8,font.axis = 2)
    
    legend("bottomright",  
           legend=c("对数收益率","EVaR",expression(CoEVaR^{L}),expression(CoEVaR^{SIM})),inset=0.01,
           col=c("gray60","#008a00","#ff00ff","#0000ff"),lty=c(NA,3,2,1),
           pch = c(20,NA,NA,NA),lwd=c(0,2.5,2.5,2.5),box.lty=1,cex = 1.7)
    #col=c("gray70","#00b0eb","#2e409a","#e20612")
    title(main=name.stock[i],cex.main=3)
    dev.off()
    
  }
}



############################################

### total out risk plot by month  图2
tc_group_sim = read.csv('sim_Results/tc_group_month_sim.csv')
tc_group_linear = read.csv('Results/tc_group_month.csv')
date_m = as.Date(tc_group_sim$month)

# total out
{
  svg(paste0("CoEVaR_sim_and_linear/SL_plot/total_risk_month.svg"),width = 16 , height = 8 ,family="GB1")
  
  par(mar=c(2,4.5,0.3,0.3))
  plot(date_m, tc_group_sim$total_out,xaxt = 'n',
       type = "l", col = "#0000ff", lwd = 3, lty=1,
       cex.lab=2,font.axis = 2, cex.axis = 1.8,
       ylab = "风险溢出水平", xlab = "", ylim = c(20, 1600), mgp = c(2.5, 0.5, 0))
  
  lines(date_m,tc_group_linear$total_out,lwd = 3,
        col ="#ff00ff",lty = 5)
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset=0.01, 
         legend=c(expression(CoEVaR^{L}),expression(CoEVaR^{SIM})),
         col=c("#ff00ff","#0000ff"),lty=c(5,1),lwd=c(2.5,2.5),box.lty=1,cex = 1.7)
  
  dev.off()
}


# total out pdf
library(Cairo)
library(showtext)
{
  CairoPDF(paste0("CoEVaR_sim_and_linear/pdf/total_risk.pdf"),width = 16, height = 8)
  showtext_begin()
  
  par(mar=c(2,4.5,0.3,0.3))
  plot(date_m, tc_group_sim$total_out,xaxt = 'n',
       type = "l", col = "#0000ff", lwd = 3, lty=1,
       cex.lab=2,font.axis = 2, cex.axis = 1.8,
       ylab = "风险溢出水平", xlab = "", ylim = c(20, 1600), mgp = c(2.5, 0.5, 0))
  
  lines(date_m,tc_group_linear$total_out,lwd = 3,
        col ="#ff00ff",lty = 5)
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset=0.01, 
         legend=c(expression(CoEVaR^{L}),expression(CoEVaR^{SIM})),
         col=c("#ff00ff","#0000ff"),lty=c(5,1),lwd=c(2.5,2.5),box.lty=1,cex = 1.7)
  
  dev.off()
}




## the average coevar and evar for sim and linear
{
  
  evar = as.matrix(read.csv
                   ('SIM_Results/EVaR_movingwindows.csv'))
  mean_evar = apply(evar,2,'mean')
  sd_evar = apply(evar,2,'sd')
  
  mean_and_sd_coevar_sim = mean_and_sd_coevar_linear = matrix(NA,30,2)
  for (j in 1:30) {
    mean_and_sd_coevar_sim[j,1] = mean(as.matrix
                                       (read.csv(paste0("SIM_Results/Coevar_sim", j, ".csv"))[,-1]))
    mean_and_sd_coevar_sim[j,2] = sd(as.matrix
                                     (read.csv(paste0("SIM_Results/Coevar_sim", j, ".csv"))[,-1]))
    
    mean_and_sd_coevar_linear[j,1] = mean(as.matrix
                                          (read.csv(paste0("Results/Coevar_l", j, ".csv"))[,-1]))
    mean_and_sd_coevar_linear[j,2] = sd(as.matrix
                                        (read.csv(paste0("Results/Coevar_l", j, ".csv"))[,-1]))
    
  }
  mean_and_sd_coevar = cbind(mean_evar, sd_evar, 
                             mean_and_sd_coevar_linear,
                             mean_and_sd_coevar_sim)
  
  colnames(mean_and_sd_coevar) = c('EVaR_mean','EVaR_sd',
                                   'CoEVaR_linear_mean','CoEVaR_linear_sd',
                                   'CoEVaR_sim_mean','CoEVaR_sim_sd')
  
  stock.name = as.data.frame(read_excel('name.xlsx'))
  rownames(mean_and_sd_coevar) = stock.name[,1]
  
  write.csv(mean_and_sd_coevar,"coevar_sim_and_linear/mean_and_sd_coevar.csv", row.names = FALSE)
  
  
}





