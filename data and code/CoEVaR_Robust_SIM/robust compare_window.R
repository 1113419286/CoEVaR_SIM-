rm(list = ls(all = TRUE))
graphics.off()

setwd('~//COEVAR/CoEVaR_Robust_SIM//')


#####################################
# window size robust 图9

tot.risk=read.csv('robust_Results/tc_group_month_sim.csv')[-c(1:5),]
tot.riskR=read.csv('robust_Results/tc_group_month_rsim.csv')

dt=as.Date(tot.riskR$month)

svg("robust_window.svg",width = 16 , height = 8 ,family="GB1")
par(mar=c(2,4.5,0.3,0.3))

plot(dt, tot.risk$total_out, ylab = "风险总和关联水平", xlab = "", pch = 16, xaxt = 'n',
     col = "white", cex.lab = 2, font.axis = 2, cex.axis = 1.8, ylim = c(30,500))
lines(smooth.spline(dt, tot.risk$total_out, spar = 0.7), lwd = 3, col = "black")
lines(smooth.spline(dt, tot.riskR$total_out, spar = 0.7),col = "blue", lty = 2, 
      lwd = 3)

dty = format(dt,"%Y")
aty = match(unique(dty),dty)
axis(1,dt[aty[-1]],labels = unique(dty)[-1],mgp = c(2.5, 0.5, 0),
     cex.axis = 1.8, font.axis = 2)

legend("topright", inset=0.01, legend=c("T = 48","T = 72"),
        col=c("black","blue"),lty=c(1,2),lwd=c(2,2),box.lty=1,cex=1.7)
dev.off()


## pdf
library(Cairo) #pdf show chinese
library(showtext) #pdf chinese font

CairoPDF("robust_window.pdf",width = 16 , height = 8 )

showtext_begin()
par(mar=c(2,4.5,0.3,0.3))

plot(dt, tot.risk$total_out, ylab = "风险总和关联水平", xlab = "", pch = 16, xaxt = 'n',
     col = "white", cex.lab = 2, font.axis = 2, cex.axis = 1.8, ylim = c(30,500))
lines(smooth.spline(dt, tot.risk$total_out, spar = 0.7), lwd = 3, col = "black")
lines(smooth.spline(dt, tot.riskR$total_out, spar = 0.7),col = "blue", lty = 2, 
      lwd = 3)

dty = format(dt,"%Y")
aty = match(unique(dty),dty)
axis(1,dt[aty[-1]],labels = unique(dty)[-1],mgp = c(2.5, 0.5, 0),
     cex.axis = 1.8, font.axis = 2)

legend("topright", inset=0.01, legend=c("T = 48","T = 72"),
       col=c("black","blue"),lty=c(1,2),lwd=c(2,2),box.lty=1,cex=1.7)
showtext_end()

dev.off()
