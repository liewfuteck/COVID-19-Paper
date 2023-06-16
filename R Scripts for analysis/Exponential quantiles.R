library(deSolve)
library(reshape2)
library(ggplot2)
library(gridExtra)

# time intervals

sg_data = read.table("C:/Users/User/OneDrive/Desktop/Singapore COVID-19 data.csv", sep = ",", header = TRUE)
sg_data = sg_data[1:75,4:5]
sg_data$date = as.Date(sg_data$date, '%d/%m/%Y')

par(mfrow = c(1,1))
plot(sg_data$date, sg_data$total_cases, col = 'red', pch = '*', xlab = "Date", ylab = "Cumulative Number of COVID-19 cases", ylim = c(0,2000))
lines(sg_data$date,sg_data$total_cases, col = 'red', lty = 1, pch = "*")
abline(h = sg_data$total_cases[75], col = 'magenta', lty = 1)




sg_data_exp = read.table("C:/Users/User/OneDrive/Desktop/Singapore COVID-19 data.csv", sep = ",", header = TRUE)
sg_data_exp = as.data.frame(sg_data_exp[1:nrow(sg_data),5])
colnames(sg_data_exp) = c('total_cases')
sg_data_exp$growth = 0
for (i in 2:nrow(sg_data_exp)){
  sg_data_exp$growth[i] = (sg_data_exp$total_cases[i] - sg_data_exp$total_cases[i-1])/ sg_data_exp$total_cases[i-1]
}

mu_max = numeric(5)
ranges = c(0.70,0.75,0.80,0.85,0.90)
for (k in 1:5){
  mu_max[k] = 1 + quantile(sg_data_exp$growth, ranges[k])
}

exp_results_1 = numeric(75)
exp_results_2 = numeric(75)
exp_results_3 = numeric(75)
exp_results_4 = numeric(75)
exp_results_5 = numeric(75)

y_0 = 1
time = c(1:75)

for (j in 1:75){
  exp_results_1[j]= y_0 * mu_max[1]^j
}

for (j in 1:75){
  exp_results_2[j]= y_0 * mu_max[2]^j
}

for (j in 1:75){
  exp_results_3[j]= y_0 * mu_max[3]^j
}

for (j in 1:75){
  exp_results_4[j]= y_0 * mu_max[4]^j
}

for (j in 1:75){
  exp_results_5[j]= y_0 * mu_max[5]^j
}

points(sg_data$date, exp_results_1, col = "green", pch = "*", lty = 1)
lines(sg_data$date, exp_results_1, col = "green", lty = 2)

points(sg_data$date, exp_results_2, col='blue', pch = "*", lty = 1)
lines(sg_data$date, exp_results_2, col = 'blue', lty = 2)

points(sg_data$date, exp_results_3, col='black', pch = "*", lty = 1)
lines(sg_data$date, exp_results_3, col = 'black', lty = 2)

points(sg_data$date, exp_results_4, col='darkorchid', pch = "*", lty = 1)
lines(sg_data$date, exp_results_4, col = 'darkorchid', lty = 2)

points(sg_data$date, exp_results_5, col='gold', pch = "*", lty = 1)
lines(sg_data$date, exp_results_5, col = 'gold', lty = 2)

legend("topleft", legend = c("Quantile = 0.70", "Quantile = 0.75",
                             "Quantile = 0.80", "Quantile = 0.85",
                             "Quantile = 0.90", "Reported number of cases", 
                             expression(paste("Reported cumulative case count as of ", "6"^"th" ," April"))),
       col = c("green", "blue", "black","darkorchid", "gold","red","magenta"),
       lty = c(2,2,2,2,2,1,1), pch = c("*","*","*","*","*","*",""), cex = 0.75)

