# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

library(dplyr)
library(lubridate)


## total group in month
{
  tc_group = read.csv('robust_Results/total_in_and_out_rsim.csv')
  date = as.Date(read.csv('date_rEVaR.csv')[,1])
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

write.csv(tc_group_m, 'robust_Results/tc_group_month_rsim.csv', row.names = FALSE)

}




