# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR/')
##########################################
# Group network plot 图5 and 图7


# install and load packages
library(qgraph)
library(readxl)

library(Cairo)
library(showtext)

# Individuals
name=as.data.frame(read_excel('name.xlsx'))
names.fi=as.vector(name[,1])

# read the connectedness matrix 
load("SIM_Results/sim_co.RData")

timelist = list('201911'=c(499:501),'202003'=c(516:519),
                        '202201'=c(611:614),'202205'=c(628:632))
mon = c('2019_11','2020_03','2022_01','2022_05')

{
  t=1
for (t in 1:length(timelist)) {
  
  m = mon[t]
  t_con = matrix(0,30,30)
  for (i in timelist[[t]]) {
  t_con = t_con+abs(as.matrix(con[,,i]))
  }
  
  t_con = ifelse(abs(t_con) >= ((1/400) * sum(t_con[order(t_con, decreasing = T)[1:400]])), 
                 t_con, 0)/length(timelist)
  max(t_con)
  groups   = list(1:30)
  
  maxi = 1 #max(t_con)
  
  # plot a network based on the adjacency matrix 't_con' 
  svg(paste0("CoEVaR_network_SIMplot/p", m, "_sim.svg"),width = 7.8 , height = 5.3 ,family="GB1")
  par(mar=c(4,2,4,2))
  plot_g = qgraph(t_con, directed=T,groups = groups, layout = "groups", layoutScale = c(1.2, 1.2), 
                  label.font = 2, label.cex = 1, shape = "circle", labels = names.fi,
                  maximum = maxi, color = "white", node.width = 0.8,  label.color = "black", 
                  edge.color = "#146077", curve = 1, border.width = 1.2, border.color ="black", asize = 2.5)
  dev.off()
  
  ## pdf
  CairoPDF(paste0("CoEVaR_network_SIMplot/pdf/p", m, "_sim.pdf"),width = 7.8 , height = 5.3)
  showtext_begin()
  
  par(mar=c(4,2,4,2))
  plot_g = qgraph(t_con, directed=T,groups = groups, layout = "groups", layoutScale = c(1.2, 1.2), 
                  label.font = 2, label.cex = 1, shape = "circle", labels = names.fi,
                  maximum = maxi, color = "white", node.width = 0.8,  label.color = "black", 
                  edge.color = "#146077", curve = 1, border.width = 1.2, border.color ="black", asize = 2.5)
  
  showtext_end()
  dev.off()
  
  
  ## Groups 
  # divide the stocks into three groups: 
  groups   = list(1:10, 11:20, 21:30)
  col3 =  c("#ff8f19","#c72228","#00b0eb")
  col      = rep(col3,each = 10)
  
  # plot a network based on the adjacency matrix 't_con'  
  svg(paste0("CoEVaR_network_SIMplot/p", m, "group_sim.svg"),width = 16 , height = 16 ,family="GB1")
  par(mai=c(3,3,2,2))
  plot_g = qgraph(t_con, directed=T,minimum = 0, legend = F , groups = groups, 
                  layout = "groups", layoutScale = c(1.2, 1.2), 
                  shape = rep(c('circle','square',"triangle"),each = 10),
                  labels = names.fi,maximum = maxi, 
                  color = rep("white", 10), label.color = col, label.cex = 1.15,
                  edge.color = col, curve = 1, border.width = 1.2, 
                  border.color = col, asize = 2.5)
  
  legend(0.75, 1.2,xpd=TRUE,legend= c("美洲","欧洲", "亚洲"),box.lwd = 0.1,
         box.col = NA, bg = NA, 
         pch=c(1,0,2), col = col3,
         text.col = col3, cex = 3)
  dev.off()
  
  
  ## pdf
  CairoPDF(paste0("CoEVaR_network_SIMplot/pdf/p", m, "group_sim.pdf"),width = 16 , height = 16 )
  showtext_begin()
  
  par(mai=c(3,3,2,2))
  plot_g = qgraph(t_con, directed=T,minimum = 0, legend = F , groups = groups, layout = "groups", layoutScale = c(1.2, 1.2), 
                  shape = rep(c('circle','square',"triangle"),each = 10),
                  labels = names.fi, maximum = maxi, 
                  color = rep("white", 10), label.color = col, label.cex = 1.15,
                  edge.color = col, curve = 1, border.width = 1.2, 
                  border.color = col, asize = 2.5)
  
  legend(0.75, 1.2,xpd=TRUE,legend= c("美洲","欧洲", "亚洲"),box.lwd = 0.1,
         box.col =NA, bg = NA, 
         pch=c(1,0,2), col = col3,
         text.col = col3, cex = 3)
  
  showtext_end()
  dev.off()
}
  
}


