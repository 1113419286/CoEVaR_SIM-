
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# read the total connecteness matrix aggregated over windows
tot.ct = as.matrix(read.csv("Results/tot_c_overtime.csv"))

# there are 688 windows
rpd = 688

# ranks the risk receivers
sif_in = rep(0, 30)
for (i in 1:30) {
  in_firms  = tot.ct[i, ]
  sif_in[i] = (sum((in_firms/rpd)))
}

names(sif_in) = colnames(tot.ct)
print(sort(sif_in, decreasing = TRUE))
write.csv(sif_in,'CoEVaR_SIFIs/sif_in.csv')


# ranks the risk emitters
sif_out = rep(0, 30)
for (i in 1:30) {
  out_firms  = tot.ct[, i]
  sif_out[i] =  (sum((out_firms/rpd)))
}

names(sif_out) = colnames(tot.ct)
print(sort(sif_out, decreasing = TRUE))
write.csv(sif_out,'CoEVaR_SIFIs/sif_out.csv')
  
