# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

setwd('~/COEVAR/')

library(dplyr)
library(lubridate)


## stock group out by month
{
  stock_group = read.csv('SIM_Results/stock_out_sim.csv')
  date = as.Date(read.csv('date_EVaR.csv')[,1])
  stock_group = data.frame('date' = date, stock_group)
  
  stock_group %>% mutate(month = floor_date(date,"month")) %>%
    group_by(month) %>%as.data.frame()-> stock_group_mm
  
  date_month = unique(stock_group_mm$month)
  
  stock_group_m = matrix(NA,length(date_month),30)
  colnames(stock_group_m) = colnames(stock_group)[-1]
  for (i in 1:length(date_month)) {
    stock_group_m[i,] = apply(stock_group_mm[stock_group_mm$month==date_month[i],-c(1,32)],2,'sum')
  }
  stock_group_m = data.frame('month' = date_month,stock_group_m)
  
  write.csv(stock_group_m, 'SIM_Results/stock_group_month_sim.csv', row.names = FALSE)
}


## total group in month
{
  tc_group = read.csv('SIM_Results/total_in_and_out_sim.csv')
  date = as.Date(read.csv('date_EVaR.csv')[,1])
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
                        'total_out' = rowSums(tc_group_m[,5:7]))

write.csv(tc_group_m, 'SIM_Results/tc_group_month_sim.csv', row.names = FALSE)

}


## area in year
{
  area_in = read.csv('SIM_Results/area_in_sim.csv')
  date = as.Date(read.csv('date_EVaR.csv')[,1])
  area_in = data.frame('date' = date, area_in)
  
  area_in %>% mutate(year=floor_date(date,"year")) %>%
    group_by(year) %>%
    summarise('year_Am_out_Am' = mean(Americas_out_Americas),
              'year_Eu_out_Am' = mean(Europe_out_Americas),
              'year_As_out_Am' = mean(Asia_out_Americas),
              'year_Am_out_Eu' = mean(Americas_out_Europe),
              'year_Eu_out_Eu' = mean(Europe_out_Europe),
              'year_As_out_Eu' = mean(Asia_out_Europe),
              'year_Am_out_As' = mean(Americas_out_Asia),
              'year_Eu_out_As' = mean(Europe_out_Asia),
              'year_As_out_As' = mean(Asia_out_Asia)) -> area_in_y

write.csv(area_in_y, 'SIM_Results/area_in_year_sim.csv', row.names = FALSE)

}

