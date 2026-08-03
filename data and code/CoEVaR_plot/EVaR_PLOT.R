# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR/')
###################################
library(readxl)

logre       = read.csv("logre.csv", header = TRUE)
logre = logre[-c(1:47),]

date_y <- as.Date(logre[,1])

name=as.data.frame(read_excel('name.xlsx'))[]
name.stock=name[,2]

############################################
library(Cairo)
library(showtext) #pdf Chinese

### out and in risk plot in month 图3 L
tc_group_m = read.csv('Results/tc_group_month.csv')
date_m = as.Date(tc_group_m$month)


# area total in
{
  svg(paste("CoEVaR_plot/risk_in_month.svg", sep = ""),width = 16 , height = 8 ,family="GB1")
  
  par(mar=c(2,4.5,1.3,0.3))
  plot(date_m, tc_group_m$month_Americas_in, type = "l", col = "#ff8f19", 
       xaxt = 'n', lwd = 3, lty=1,
       ylab = "尾部风险接收水平", xlab = "", cex.lab = 2, font.axis = 2, cex.axis = 1.8,
       ylim = c(15,45),mgp = c(2.5, 0.5, 0)) #America
  
  lines(date_m, tc_group_m$month_Europe_in, lwd = 2.5, col ="#c72228",lty=2)
  
  lines(date_m, tc_group_m$month_Asia_in, lwd = 2.5, col ="#00b0eb",lty=3)
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset = 0.02, legend=c("美洲", "欧洲", "亚洲"),
         col=c("#ff8f19","#c72228","#00b0eb"),lty=c(1,2,3),
         lwd=c(2.5,2.5,2.5),box.lty=1,cex = 1.7)
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_plot/pdf/risk_in_month.pdf"),width = 16, height = 8)
  showtext_begin()
  
  par(mar=c(2,4.5,1.3,0.3))
  plot(date_m, tc_group_m$month_Americas_in, type = "l", col = "#ff8f19", 
       xaxt = 'n', lwd = 3, lty=1,
       ylab = "尾部风险接收水平", xlab = "", cex.lab = 2, font.axis = 2, cex.axis = 1.8,
       ylim = c(15,45),mgp = c(2.5, 0.5, 0)) #America
  
  lines(date_m, tc_group_m$month_Europe_in, lwd = 2.5, col ="#c72228",lty=2)
  
  lines(date_m, tc_group_m$month_Asia_in, lwd = 2.5, col ="#00b0eb",lty=3)
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset = 0.02, legend=c("美洲", "欧洲", "亚洲"),
         col=c("#ff8f19","#c72228","#00b0eb"),lty=c(1,2,3),
         lwd=c(2.5,2.5,2.5),box.lty=1,cex = 1.7)
  
  showtext_end()
  dev.off()
}


# area total out 图3 L
{
  svg(paste("CoEVaR_plot/risk_out_month.svg", sep = ""),width = 16 , height = 8 ,family="GB1")
  
  par(mar=c(2,4.5,1.3,0.3))
  plot(date_m, tc_group_m$month_Americas_out, type = "l", col = "#ff8f19", 
       xaxt = 'n', lwd = 3, lty=1,
       ylab = "尾部风险溢出水平", xlab = "", cex.lab = 2, font.axis = 2, cex.axis = 1.8,
       ylim = c(15,45),mgp = c(2.5, 0.5, 0)) #America
  
  lines(date_m, tc_group_m$month_Europe_out, lwd = 2.5, col ="#c72228",lty=2)
  
  lines(date_m, tc_group_m$month_Asia_out, lwd = 2.5, col ="#00b0eb",lty=3)
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset=0.02, legend=c("美洲", "欧洲", "亚洲"),
         col=c("#ff8f19","#c72228","#00b0eb"),lty=c(1,2,3),
         lwd=c(2.5,2.5,2.5),box.lty=1, cex = 1.7)
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_plot/pdf/risk_out_month.pdf"), width = 16, height = 8)
  showtext_begin()
  
  par(mar=c(2,4.5,1.3,0.3))
  plot(date_m, tc_group_m$month_Americas_out, type = "l", col = "#ff8f19", 
       xaxt = 'n', lwd = 3, lty=1,
       ylab = "尾部风险溢出水平", xlab = "", cex.lab = 2, font.axis = 2, cex.axis = 1.8,
       ylim = c(15,45),mgp = c(2.5, 0.5, 0)) #America
  
  lines(date_m, tc_group_m$month_Europe_out, lwd = 2.5, col ="#c72228",lty=2)
  
  lines(date_m, tc_group_m$month_Asia_out, lwd = 2.5, col ="#00b0eb",lty=3)
  
  datem = format(date_m,"%Y")
  atm = match(unique(datem),datem)
  axis(1,date_m[atm[-1]],labels = unique(datem)[-1],mgp = c(2.5, 0.5, 0),
       cex.axis = 1.8, font.axis = 2)
  
  legend("topright", inset=0.02, legend=c("美洲", "欧洲", "亚洲"),
         col=c("#ff8f19","#c72228","#00b0eb"),lty=c(1,2,3),
         lwd=c(2.5,2.5,2.5),box.lty=1, cex = 1.7)
  
  showtext_end()
  dev.off()
}





### area out and risk plot in year 图4 L
area_in_y = read.csv('Results/area_in_year.csv')

library(ggplot2)
library(RColorBrewer)
library(reshape2)

# Americas in 
{
  area_Americas_in = data.frame('name'=c('Americas','Europe','Asia'), t(area_in_y[,2:4]))
  area_Americas_in = melt(area_Americas_in, id='name')
  area_Americas_in$variable = rep(as.Date(area_in_y$year), each=3)
  
  svg(paste("CoEVaR_plot/CoEVaR_area_Americas_year.svg", sep = ""),width = 8 , height = 4)
  
  par(mar=c(2,1,0.3,0.3))
  ggplot(data=area_Americas_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,35))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = 'none',
          panel.background = element_blank(),axis.line =element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold'))+
    labs(x='',y='', title = '')
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_plot/pdf/CoEVaR_area_Americas_year.pdf"),width = 8, height = 4)
  showtext_begin()
  
  par(mar=c(2,0.3,0.3,0.3))
  ggplot(data=area_Americas_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,35))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = 'none',
          panel.background = element_blank(),axis.line =element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold'))+
    labs(x='',y='', title = '')
  
  showtext_end()
  dev.off()
}


# Europe in 
{
  area_Europe_in = data.frame('name'=c('Americas','Europe','Asia'), t(area_in_y[,5:7]))
  area_Europe_in = melt(area_Europe_in, id='name')
  area_Europe_in$variable = rep(as.Date(area_in_y$year), each=3)
  
  svg(paste("CoEVaR_plot/CoEVaR_area_Europe_year.svg", sep = ""),width = 8 , height = 4 )
  
  par(mar=c(2,1,0.3,0.3))
  ggplot(data=area_Europe_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,35))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = "none",
          panel.background = element_blank(),axis.line = element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold'))+
    labs(x='',y='', title = '')
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_plot/pdf/CoEVaR_area_Europe_year.pdf"), width = 8, height = 4)
  showtext_begin()
  
  par(mar=c(2,1,0.3,0.3))
  ggplot(data=area_Europe_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,35))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = "none",
          panel.background = element_blank(),axis.line = element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold'))+
    labs(x='',y='', title = '')
  
  showtext_end()
  dev.off()
}


# Asia in 
{
  area_Asia_in = data.frame('name'=c('Americas','Europe','Asia'), t(area_in_y[,8:10]))
  area_Asia_in = melt(area_Asia_in, id='name')
  area_Asia_in$variable = rep(as.Date(area_in_y$year), each=3)
  
  svg(paste("CoEVaR_plot/CoEVaR_area_Asia_year.svg", sep = ""),width = 8 , height = 4)
  
  par(mar=c(2,1,0.3,0.3))
  ggplot(data=area_Asia_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,35))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = 'none',
          panel.background = element_blank(),axis.line =element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold'))+
    labs(x='',y='', title = '')
  
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_plot/pdf/CoEVaR_area_Asia_year.pdf"),width = 8, height = 4)
  showtext_begin()
  
  par(mar=c(2,1,0.3,0.3))
  ggplot(data=area_Asia_in, aes(variable,value,fill = name))+
    geom_bar(stat="identity")+
    geom_col(width = 1)+
    scale_fill_manual(values = c('Americas'="#ff8f19", 'Europe'="#c72228",'Asia'="#00b0eb"),
                      limits = c("Americas","Europe","Asia"))+
    scale_y_continuous(expand = c(0,0),limits = c(0,35))+
    scale_x_date(date_breaks="1 year",date_labels="%Y")+
    theme(legend.position = 'none',
          panel.background = element_blank(),axis.line =element_line(),
          axis.text.x=element_text(size=12,face = 'bold'),
          axis.text.y=element_text(size=12,face = 'bold'))+
    labs(x='',y='', title = '')
  
  showtext_end()
  dev.off()
}





