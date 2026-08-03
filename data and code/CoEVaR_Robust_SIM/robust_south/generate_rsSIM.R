rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR/CoEVaR_Robust_SIM/robust_south/')

library(readxl)

n = 706
ws  = 48

# number of columns in each partial derivative matrix
cpd = 35
# number of rows in each partial derivative matrix
rpd = (n - ws+1)

#####################################
library(miscTools)
# since each firm does not regress on itself, we need to insert a zero column
# vector in the position of every stock's partial derivative matrix
vec_zero = matrix(0, rpd, 1)
der.c = array(0, dim = c(rpd, cpd, cpd))
for (i in 1:35) {
  der.c[, , i] = insertCol(as.matrix(read.csv(file = paste("rs_results/partial_der", i, ".csv", 
                                                           sep = "")))[, 2:35], i, vec_zero)
}

# generate the connnectedness matrix
con = array(0, dim = c(cpd, cpd, rpd))
for (i in 1:rpd) {
  con.v = rep(0, 35)
  for (j in 1:cpd) {
    con.v = rbind(con.v, der.c[i, , j])
  }
  con[, , i] = con.v[-1, ]
}


save(con,file = "rs_Results/rs_co.RData")



##########################################
setwd('D:/Desktop/CoEVaR/?й???????ѧ/code/COEVAR/CoEVaR_Robust_SIM/robust_south')

load("rs_results/rs_co.RData")

### total_connectedness_and_averaged_lambda
{
## the total connectedness
total.c = rep(0, rpd)
for (i in 1:rpd) {
  total.c[i] = sum(abs(con[, , i]))
}

## the average lambda series
full.lambda = matrix(0, rpd, cpd)
for (j in 1:cpd) {
  lambda.firm      = read.csv(file = paste("rs_results/lambda_sim", j, ".csv", sep = ""))
  full.lambda[, j] = as.matrix(lambda.firm)[, 2]
}
average_lambda = 1/cpd * (rowSums(full.lambda))
tc_l           = cbind(total.c, average_lambda)
# generate the necessory file for the quantlet TENET_total_connectedness
write.csv(tc_l, file = "rs_results/total_connectedness_and_averaged_lambda_rs.csv", row.names = FALSE)

}


## the average coevar and evar
average_evar = apply(as.matrix(read.csv
                             ('rs_results/rsEVaR_movingwindows.csv')),2,'mean')
average_coevar=c()
for (j in 1:cpd) {
  average_coevar[j] = mean(as.matrix
                          (read.csv(paste0("rs_results/Coevar_sim", j, ".csv"))[,-1]))
}
average_co_and_evar = cbind(average_evar,average_coevar)
colnames(average_co_and_evar) = c('EVaR','CoEVaR_sim')

stock.name = as.data.frame(read_excel('name_rs.xlsx'))
rownames(average_co_and_evar) = stock.name[,1]

write.csv(average_co_and_evar,"rs_results/averaged_evar_and_coevar_rs.csv", row.names = FALSE)


### total in and out group with africa
{
## total in groups 
# in Americas
total.in.Americas = matrix(0, rpd, 1)
for (i in 1:rpd) {
  total.in.Americas[i] = sum(abs(con[, , i])[1:10,-c(31:35)]) #,-c(1:10)
}

# in EU
total.in.Asia = matrix(0, rpd, 1)
for (i in 1:rpd) {
  total.in.Asia[i] = sum(abs(con[, , i])[11:20,-c(31:35)]) #,-c(11:20)
}

# in Asia
total.in.EU = matrix(0, rpd, 1)
for (i in 1:rpd) {
  total.in.EU[i] = sum(abs(con[, , i])[21:30,-c(31:35)]) #,-c(21:30)
}

# # in Africa
# total.in.Africa = matrix(0, rpd, 1)
# for (i in 1:rpd) {
#   total.in.Africa[i] = sum(abs(con[, , i])[31:35,]) #,-c(31:35)
# }

tc_in = cbind(total.in.Americas, total.in.Asia, total.in.EU)
colnames(tc_in) = c("Americas_in", "Europe_in", "Asia_in")


# total out groups 
# out Americas
total.out.Americas = matrix(0, rpd, 1)
for (i in 1:rpd) {
  total.out.Americas[i] = sum(abs(con[, , i])[-c(31:35), 1:10])
}

# out Asia
total.out.Asia = matrix(0, rpd, 1)
for (i in 1:rpd) {
  total.out.Asia[i] = sum(abs(con[, , i])[-c(31:35),11:20])
}

# out Eu
total.out.EU = matrix(0, rpd, 1)
for (i in 1:rpd) {
  total.out.EU[i] = sum(abs(con[, , i])[-c(31:35),21:30])
}

# # out Africa
# total.out.Africa = matrix(0, rpd, 1)
# for (i in 1:rpd) {
#   total.out.Africa[i] = sum(abs(con[, , i])[,31:35]) 
# }

tc_out = cbind(total.out.Americas, total.out.Asia, total.out.EU)
colnames(tc_out) = c("Americas_out", "Europe_out", "Asia_out")

tc_group = cbind(tc_in, tc_out)

# generate the necessary file for the expectile TENET_total_in_out_groups
write.csv(tc_group, file = "rs_results/total_in_and_out_rs.csv", row.names = FALSE)

}



# the total connecteness matrix aggregated over windows for different country
tot.ct = matrix(0, cpd, cpd)
for (i in 1:rpd) {
  tot.ct = tot.ct + abs(con[, , i])
}
colnames(tot.ct) = stock.name[,1]
# generate the necessary file for the expectlie TENET_total_in_out_individual and
# TENET_SIFIs
write.csv(tot.ct, file = "rs_results/tot_c_overtime_rs.csv", row.names = FALSE) 




