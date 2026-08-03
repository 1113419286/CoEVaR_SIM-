
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR/')

# read the total connecteness matrix aggregated over windows
tot.ct = as.matrix(read.csv("SIM_Results/tot_c_overtime_sim.csv"))

# there are 688 networks
#rpd = 183 #688 all #505 before #183 after

rpd = 688
# ranks the risk receivers
sif_in = rep(0, 30)
for (i in 1:30) {
  in_firms  = tot.ct[i, ]
  sif_in[i] = (sum((in_firms/rpd)))
}

names(sif_in) = colnames(tot.ct)
print(sort(sif_in, decreasing = TRUE))
write.csv(sif_in,'CoEVaR_SIFIs_SIM/sif_in_sim.csv')

print(sif_in)
# ranks the risk emitters
sif_out = rep(0, 30)
for (i in 1:30) {
  out_firms  = tot.ct[, i]
  sif_out[i] =  (sum((out_firms/rpd)))
}

names(sif_out) = colnames(tot.ct)
print(sort(sif_out, decreasing = TRUE))
write.csv(sif_out,'CoEVaR_SIFIs_SIM/sif_out_sim.csv')



