# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR/')

#############################################
library(readxl)

logre       = read.csv("logre.csv", header = TRUE)
logre = logre[-c(1:47),]
date_y <- as.Date(logre[,1]) #date week

name=as.data.frame(read_excel('name.xlsx'))
name.stock=name[,2]

##################################################
library(Cairo)
library(showtext) #pdf for Chinese

### single stock risk out plot in month 
stock_group_m = read.csv('SIM_Results/stock_group_month_sim.csv')

for (i in 1:30) {
  i=24 #SSEC  图12
  
  date_month = stock_group_m$month
  
  svg(paste0("CoEVaR_SIMplot/s_out ",i, '_',name.stock[i], ".svg"),width = 16 , height = 8 ,family="GB1")
  
  par(mar=c(2,4.5,0.3,0.3))
  plot(as.Date(date_month),stock_group_m[,(i+1)], type = "l", col = "black", xaxt = 'n',
       lwd = 3, ylab = "风险溢出水平", xlab = "", cex.lab = 1.5,
       ylim = c(0,150),mgp = c(2.5, 0.5, 0))
  datey = format(date_y,"%Y")
  aty = match(unique(datey),datey)
  axis(1,date_y[aty[-1]],labels = unique(datey)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  #title(main=name.stock[i],cex.main=2)
  dev.off()
  
  # pdf
  CairoPDF(paste0("CoEVaR_SIMplot/s_out ",i, '_',name.stock[i], ".pdf"),
           width = 16, height = 8)
  showtext_begin()
  
  par(mar=c(2,4.5,0.3,0.3))
  plot(as.Date(date_month),stock_group_m[,(i+1)], type = "l", col = "black", xaxt = 'n',
       lwd = 3, ylab = "风险溢出水平", xlab = "", 
       cex.lab = 2, font.axis = 2, cex.axis = 1.8,
       ylim = c(0,150),mgp = c(2.5, 0.5, 0))
  datey = format(date_y,"%Y")
  aty = match(unique(datey),datey)
  axis(1,date_y[aty[-1]],labels = unique(datey)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  #title(main=name.stock[i],cex.main=2)
  showtext_end()
  dev.off()
  
  
}



########################################################
library(Cairo)
library(showtext) #pdf for Chinese

### out and in risk plot in month (bar) 图3 SIM
tc_group_m = read.csv('SIM_Results/tc_group_month_sim.csv')
date_m = as.Date(tc_group_m$month)

# area total in
{
  svg(paste0("CoEVaR_SIMplot/risk_in_month_sim.svg"),width = 16 , height = 8 ,family="GB1")
  
  par(mar=c(2,4.5,1.3,0.3))
  plot(date_m, tc_group_m$month_Americas_in, type = "l", col = "#ff8f19", 
       xaxt = 'n', lwd = 3.5, lty=1,
       ylab = "尾部风险接收水平", xlab = "", cex.lab = 2, font.axis = 2, cex.axis = 1.8,
       ylim = c(0,1200),mgp = c(2.5, 0.5, 0)) #America
  
  lines(date_m, tc_group_m$month_Europe_in, lwd = 3.5, col ="#c72228",lty=2) 
  
  lines(date_m, tc_group_m$month_Asia_in, lwd = 3.5, col ="#00b0eb",lty=3)  #0c4e9b
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset = 0.02, legend=c("美洲", "欧洲", "亚洲"),
         col=c("#ff8f19","#c72228","#00b0eb"),lty=c(1,2,3),
         lwd=c(3,3,3),box.lty=1,cex = 1.7)
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_SIMplot/pdf/risk_in_month_sim.pdf"),width = 16, height = 8)
  showtext_begin()
  
  par(mar=c(2,4.5,1.3,0.3))
  plot(date_m, tc_group_m$month_Americas_in, type = "l", col = "#ff8f19", 
       xaxt = 'n', lwd = 3.5, lty=1,
       ylab = "尾部风险接收水平", xlab = "", cex.lab = 2, font.axis = 2, cex.axis = 1.8,
       ylim = c(0,1200),mgp = c(2.5, 0.5, 0)) #America
  
  lines(date_m, tc_group_m$month_Europe_in, lwd = 3.5, col ="#c72228",lty=2) 
  
  lines(date_m, tc_group_m$month_Asia_in, lwd = 3.5, col ="#00b0eb",lty=3)  #0c4e9b
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset = 0.02, legend=c("美洲", "欧洲", "亚洲"),
         col=c("#ff8f19","#c72228","#00b0eb"),lty=c(1,2,3),
         lwd=c(3,3,3),box.lty=1,cex = 1.7)
  
  dev.off()
}


# area total out 图3
{
  svg(paste0("CoEVaR_SIMplot/risk_out_month_sim.svg"),width = 16 , height = 8 ,family="GB1")
  
  par(mar=c(2,4.5,1.3,0.3))
  plot(date_m, tc_group_m$month_Americas_out, type = "l", col = "#ff8f19", 
       xaxt = 'n', lwd = 5, lty=1,
       ylab = "尾部风险溢出水平", xlab = "", cex.lab = 2, font.axis = 2, cex.axis = 1.8,
       ylim = c(0,800),mgp = c(2.5, 0.5, 0)) #America
  
  lines(date_m, tc_group_m$month_Europe_out, lwd = 3, col ="#c72228",lty=2)
  
  lines(date_m, tc_group_m$month_Asia_out, lwd = 3, col ="#00b0eb",lty=3)
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset=0.02, legend=c("美洲", "欧洲", "亚洲"),
         col=c("#ff8f19","#c72228","#00b0eb"),lty=c(1,2,3),
         lwd=c(3,2.5,2.5),box.lty=1, cex = 1.7)
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_SIMplot/pdf/risk_out_month_sim.pdf"), width = 16, height = 8)
  showtext_begin()
  
  par(mar=c(2,4.5,1.3,0.3))
  plot(date_m, tc_group_m$month_Americas_out, type = "l", col = "#ff8f19", 
       xaxt = 'n', lwd = 5, lty=1,
       ylab = "尾部风险溢出水平", xlab = "", cex.lab = 2, font.axis = 2, cex.axis = 1.8,
       ylim = c(0,800),mgp = c(2.5, 0.5, 0)) #America
  
  lines(date_m, tc_group_m$month_Europe_out, lwd = 3, col ="#c72228",lty=2)
  
  lines(date_m, tc_group_m$month_Asia_out, lwd = 3, col ="#00b0eb",lty=3)
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset=0.02, legend=c("美洲", "欧洲", "亚洲"),
         col=c("#ff8f19","#c72228","#00b0eb"),lty=c(1,2,3),
         lwd=c(3,2.5,2.5),box.lty=1, cex = 1.7)
  
  showtext_end()
  dev.off()
}






### area out and risk plot in year  图4 SIM
area_in_y = read.csv('SIM_Results/area_in_year_sim.csv')

library(ggplot2)
library(RColorBrewer)
library(reshape2)

# Americas in 
{
  area_Americas_in = data.frame('name'=c('Americas','Europe','Asia'), t(area_in_y[,2:4]))
  area_Americas_in = melt(area_Americas_in, id='name')
  area_Americas_in$variable = rep(as.Date(area_in_y$year), each=3)
  
  
  svg(paste0("CoEVaR_SIMplot/CoEVaR_area_Americas_year_sim.svg"),width = 8, height = 4)
  
  par(mar=c(2,0.3,0.3,0.8))
  ggplot(data=area_Americas_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,250))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = 'none',
          panel.background = element_blank(),axis.line =element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold'))+
    labs(x='',y='', title = '')
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_SIMplot/pdf/CoEVaR_area_Americas_year_sim.pdf"),width = 8, height = 4)
  showtext_begin()
  
  par(mar=c(2,0.3,0.3,0.8))
  ggplot(data=area_Americas_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,250))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = 'none',
          panel.background = element_blank(),axis.line =element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold'))+
    labs(x='',y='', title = '')
  
  dev.off()
}


# Europe in 
{
  area_Europe_in = data.frame('name'=c('Americas','Europe','Asia'), t(area_in_y[,5:7]))
  area_Europe_in = melt(area_Europe_in, id='name')
  area_Europe_in$variable = rep(as.Date(area_in_y$year), each=3)
  
  
  svg(paste0("CoEVaR_SIMplot/CoEVaR_area_Europe_year_sim.svg"),width = 8 , height = 4 )
  
  par(mar=c(2,1,0.3,0.3))
  ggplot(data=area_Europe_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,250))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = "none",
          panel.background = element_blank(),axis.line = element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold') )+
    labs(x='',y='', title = '')
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_SIMplot/pdf/CoEVaR_area_Europe_year_sim.pdf"), width = 8, height = 4)
  showtext_begin()
  
  par(mar=c(2,1,0.3,0.3))
  ggplot(data=area_Europe_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,250))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = "none",
          panel.background = element_blank(),axis.line = element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold') )+
    labs(x='',y='', title = '')
  
  showtext_end()
  dev.off()
}


# Asia in 
{
  area_Asia_in = data.frame('name'=c('Americas','Europe','Asia'), t(area_in_y[,8:10]))
  area_Asia_in = melt(area_Asia_in, id='name')
  area_Asia_in$variable = rep(as.Date(area_in_y$year), each=3)
  
  
  svg(paste0("CoEVaR_SIMplot/CoEVaR_area_Asia_year_sim.svg"),width = 8 , height = 4)
  
  par(mar=c(2,1,0.3,0.3))
  ggplot(data=area_Asia_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,250))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = 'none',
          panel.background = element_blank(),axis.line =element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold') )+
    labs(x='',y='', title = '')
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_SIMplot/pdf/CoEVaR_area_Asia_year_sim.pdf"),width = 8, height = 4)
  showtext_begin()
  
  par(mar=c(2,1,0.3,0.3))
  ggplot(data=area_Asia_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,250))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = 'none',
          panel.background = element_blank(),axis.line =element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold') )+
    labs(x='',y='', title = '')
  
  showtext_end()
  dev.off()
}





