library(deSolve)
library(reshape2)
library(ggplot2)
library(gridExtra)

# time intervals
time_1 <- seq(0,15,by=1)

sg_pop = 5.704 * 10^6

beta_value <- c(0.3804805, 0.95 * 0.3804805, (0.95^2) * 0.3804805)


# Parameters
parameters_asymptomatic <- c(beta = beta_value[1],
                             N = sg_pop,
                             r = 1/3.85, 
                             sigma = 1/5.1,
                             p = 0.40,
                             k = 1/21,
                             tau = 1/4,
                             a = 0.003,
                             b = 1/17, 
                             c = 1/27,
                             d = 1/9,
                             e = 1/9,
                             gamma = 2.6585186,
                             epsilon = 1/3.85)

# set initial conditions

states_asymptomatic <- c(S = 5.704 * 10^6,
                         E = 10,
                         A = 10,
                         I = 10,
                         H_icu = 0,
                         H_noicu = 0,
                         R = 0,
                         D = 0,
                         check = 0)

covid_asymptomatic <- function(t, states_asymptomatic, parameters_asymptomatic){
  with(as.list(c(states_asymptomatic, parameters_asymptomatic)),{
    dS = (-beta * S * I) / N - (r * beta * S * A) / N - (epsilon * beta * S * E) / N
    dE = (beta * S * I)/N + (r * beta * S * A) / N + (epsilon * beta * S * E) / N - sigma * E
    dA = p * sigma * E - k * A
    dI = (1-p) * sigma * E - gamma * tau * I
    dH_icu = a * gamma * tau * I - b * gamma * H_icu - c / gamma * H_icu
    dH_noicu = (1-a) * gamma * tau * I - gamma * d * H_noicu - e / gamma * H_noicu
    dR = k * A + gamma * b * H_icu + gamma * d * H_noicu
    dD = c / gamma * H_icu + e / gamma * H_noicu
    dcheck = dS + dE + dA + dI + dH_icu + dH_noicu + dR + dD
    return(list(c(dS, dE, dA, dI, dH_icu, dH_noicu, dR, dD, dcheck)))
  })
}

out_asymptomatic_1 <- ode(y = states_asymptomatic, times = time_1, 
                        func = covid_asymptomatic, parms = parameters_asymptomatic)

out_asymptomatic_1.df = as.data.frame(out_asymptomatic_1)

out_asymptomatic_1.df$'total' <- rowSums(out_asymptomatic_1.df) - out_asymptomatic_1.df$S










time_2 <- seq(16,63,by=1)
parameters_asymptomatic <- c(beta = beta_value[2],
                             N = sg_pop,
                             r = 1/3.85, 
                             sigma = 1/5.1,
                             p = 0.40,
                             k = 1/21,
                             tau = 1/4,
                             a = 0.003,
                             b = 1/17, 
                             c = 1/27,
                             d = 1/9,
                             e = 1/9,
                             gamma = 2.6585186,
                             epsilon = 1/3.85)

states_asymptomatic <- c(S = out_asymptomatic_1.df[nrow(out_asymptomatic_1.df),2],
                         E = out_asymptomatic_1.df[nrow(out_asymptomatic_1.df),3],
                         A = out_asymptomatic_1.df[nrow(out_asymptomatic_1.df),4],
                         I = out_asymptomatic_1.df[nrow(out_asymptomatic_1.df),5],
                         H_icu = out_asymptomatic_1.df[nrow(out_asymptomatic_1.df),6],
                         H_noicu = out_asymptomatic_1.df[nrow(out_asymptomatic_1.df),7],
                         R = out_asymptomatic_1.df[nrow(out_asymptomatic_1.df),8],
                         D = out_asymptomatic_1.df[nrow(out_asymptomatic_1.df),9],
                         check = out_asymptomatic_1.df[nrow(out_asymptomatic_1.df),10])

covid_asymptomatic <- function(t, states_asymptomatic, parameters_asymptomatic){
  with(as.list(c(states_asymptomatic, parameters_asymptomatic)),{
    dS = (-beta * S * I) / N - (r * beta * S * A) / N - (epsilon * beta * S * E) / N
    dE = (beta * S * I)/N + (r * beta * S * A) / N + (epsilon * beta * S * E) / N - sigma * E
    dA = p * sigma * E - k * A
    dI = (1-p) * sigma * E - gamma * tau * I
    dH_icu = a * gamma * tau * I - b * gamma * H_icu - c / gamma * H_icu
    dH_noicu = (1-a) * gamma * tau * I - gamma * d * H_noicu - e / gamma * H_noicu
    dR = k * A + gamma * b * H_icu + gamma * d * H_noicu
    dD = c / gamma * H_icu + e / gamma * H_noicu
    dcheck = dS + dE + dA + dI + dH_icu + dH_noicu + dR + dD
    return(list(c(dS, dE, dA, dI, dH_icu, dH_noicu, dR, dD, dcheck)))
  })
}

out_asymptomatic_2 <- ode(y = states_asymptomatic, times = time_2, 
                        func = covid_asymptomatic, parms = parameters_asymptomatic)

out_asymptomatic_2.df = as.data.frame(out_asymptomatic_2)

out_asymptomatic_2.df$'total' <- rowSums(out_asymptomatic_2.df) - out_asymptomatic_2.df$S








time_3 <- seq(64,74,by=1)
parameters_asymptomatic <- c(beta = beta_value[3],
                             N = sg_pop,
                             r = 1/3.85, 
                             sigma = 1/5.1,
                             p = 0.40,
                             k = 1/21,
                             tau = 1/4,
                             a = 0.003,
                             b = 1/17, 
                             c = 1/27,
                             d = 1/9,
                             e = 1/9,
                             gamma = 2.6585186,
                             epsilon = 1/3.85)

states_asymptomatic <- c(S = out_asymptomatic_2.df[nrow(out_asymptomatic_2.df),2],
                         E = out_asymptomatic_2.df[nrow(out_asymptomatic_2.df),3],
                         A = out_asymptomatic_2.df[nrow(out_asymptomatic_2.df),4],
                         I = out_asymptomatic_2.df[nrow(out_asymptomatic_2.df),5],
                         H_icu = out_asymptomatic_2.df[nrow(out_asymptomatic_2.df),6],
                         H_noicu = out_asymptomatic_2.df[nrow(out_asymptomatic_2.df),7],
                         R = out_asymptomatic_2.df[nrow(out_asymptomatic_2.df),8],
                         D = out_asymptomatic_2.df[nrow(out_asymptomatic_2.df),9],
                         check = out_asymptomatic_2.df[nrow(out_asymptomatic_2.df),10])

covid_asymptomatic <- function(t, states_asymptomatic, parameters_asymptomatic){
  with(as.list(c(states_asymptomatic, parameters_asymptomatic)),{
    dS = (-beta * S * I) / N - (r * beta * S * A) / N - (epsilon * beta * S * E) / N
    dE = (beta * S * I)/N + (r * beta * S * A) / N + (epsilon * beta * S * E) / N - sigma * E
    dA = p * sigma * E - k * A
    dI = (1-p) * sigma * E - gamma * tau * I
    dH_icu = a * gamma * tau * I - b * gamma * H_icu - c / gamma * H_icu
    dH_noicu = (1-a) * gamma * tau * I - gamma * d * H_noicu - e / gamma * H_noicu
    dR = k * A + gamma * b * H_icu + gamma * d * H_noicu
    dD = c / gamma * H_icu + e / gamma * H_noicu
    dcheck = dS + dE + dA + dI + dH_icu + dH_noicu + dR + dD
    return(list(c(dS, dE, dA, dI, dH_icu, dH_noicu, dR, dD, dcheck)))
  })
}

out_asymptomatic_3 <- ode(y = states_asymptomatic, times = time_3, 
                          func = covid_asymptomatic, parms = parameters_asymptomatic)

out_asymptomatic_3.df = as.data.frame(out_asymptomatic_3)

out_asymptomatic_3.df$'total' <- rowSums(out_asymptomatic_3.df) - out_asymptomatic_3.df$S


out_asymptomatic.df <- do.call("rbind", list(out_asymptomatic_1.df, out_asymptomatic_2.df, out_asymptomatic_3.df))



susceptible_asymptomatic_1 = out_asymptomatic.df$S
exposed_asymptomatic_1 = out_asymptomatic.df$E
asymptomatic_asymptomatic_1 = out_asymptomatic.df$A
infected_asymptomatic_1 = out_asymptomatic.df$I
icu_asymptomatic_1 = out_asymptomatic.df$H_icu
no_icu_asymptomatic_1 = out_asymptomatic.df$H_noicu
recovered_asymptomatic_1 = out_asymptomatic.df$R
death_asymptomatic_1 = out_asymptomatic.df$D
total_asymptomatic_1 = out_asymptomatic.df$total
check_asymptomatic_1 = out_asymptomatic.df$check

# par(mfrow = c(5,2))
# 
# plot(out_asymptomatic.df$time, susceptible_asymptomatic_1, xlab = "Date")
# 
# plot(out_asymptomatic.df$time, exposed_asymptomatic_1, xlab = "Date")
# 
# plot(out_asymptomatic.df$time, asymptomatic_asymptomatic_1, xlab = "Date")
# 
# plot(out_asymptomatic.df$time, infected_asymptomatic_1, xlab = "Date")
# 
# plot(out_asymptomatic.df$time, icu_asymptomatic_1, xlab = "Date")
# 
# plot(out_asymptomatic.df$time, no_icu_asymptomatic_1, xlab = "Date")
# 
# plot(out_asymptomatic.df$time, recovered_asymptomatic_1, xlab = "Date")
# 
# plot(out_asymptomatic.df$time, death_asymptomatic_1, xlab = "Date")
# 
# plot(out_asymptomatic.df$time, total_asymptomatic_1, xlab = "Date")
# 
# plot(out_asymptomatic.df$time, check_asymptomatic_1, xlab = "Date", ylim = c(-1,1))

total_asymptomatic_p_040 = total_asymptomatic_1


sg_data = read.table("C:/Users/User/OneDrive/Desktop/Singapore COVID-19 data.csv", sep = ",", header = TRUE)
sg_data = sg_data[1:75,4:5]
sg_data$date = as.Date(sg_data$date, '%d/%m/%Y')

# par(mfrow = c(1,1))
# plot(sg_data$date, sg_data$total_cases, col = 'red', pch = '*', xlab = "Date", ylab = "Cumulative Number of COVID-19 cases", ylim = c(0,5000))
# lines(sg_data$date, sg_data$total_cases, col = 'red', lty = 1)
# 
# lines(sg_data$date, total_asymptomatic_20pt, col = 'blue', pch = '.', lty = 4)
# lines(total_asymptomatic_20pt, col = 'blue', lty = 2)







### Second half

### KIV on finding the median of degree of under-estimation

# sg_data = read.table("C:/Users/User/OneDrive/Desktop/Singapore COVID-19 data.csv", sep = ",", header = TRUE)
# sg_data = sg_data[1:75,4:5]
# sg_data$date = as.Date(sg_data$date, '%d/%m/%Y')
# 
# total_merged <- cbind(as.data.frame(sg_data$date), sg_data$total_cases, total_asymptomatic_20pt, total_asymptomatic_40pt, total_asymptomatic_60pt, total_asymptomatic_exp)
# colnames(total_merged) <- c("date", "total_cases", "seaihrd_20pct", "seaihrd_40pct", "seaihrd_60pct", "seaihrd_exp")
# 
# total_merged$date[total_merged$seaihrd_20pct > 1375][1]
# total_merged$date[total_merged$seaihrd_40pct > 1375][1]
# total_merged$date[total_merged$seaihrd_60pct > 1375][1]
# total_merged$date[total_merged$seaihrd_exp > 1375][1]




par(mfrow = c(1,1))

plot(sg_data$date, total_asymptomatic_p_016, col = "green", pch = "*", xlab = "Date", ylab = "Cumulative Number of COVID-19 cases", ylim = c(0,5000), lty = 1)
lines(sg_data$date, total_asymptomatic_p_016, col = "green", lty = 2)

points(sg_data$date, total_asymptomatic_p_025, col='blue', pch = "*", lty = 1)
lines(sg_data$date, total_asymptomatic_p_025, col = 'blue', lty = 2)

points(sg_data$date, total_asymptomatic_p_040, col='black', pch = "*", lty = 1)
lines(sg_data$date, total_asymptomatic_p_040, col = 'black', lty = 2)

points(sg_data$date, sg_data$total_cases, col = 'red', pch = '*', lty = 1)
lines(sg_data$date, sg_data$total_cases, col = 'red', lty = 1)


legend("topleft", legend = c("p = 0.16", 
                             "p = 0.25",
                             "p = 0.40",
                             "Reported number of cases"),
       col = c("green", "blue", "black", "red"),
       lty = c(2,2,2,1), pch = c("*","*","*","*"), cex = 1.2)

# pt_compare_20 <- cbind(sg_data$total_cases, total_asymptomatic_20pt)
# pt_compare_20 = as.data.frame(pt_compare_20)
# colnames(pt_compare_20) <- c('actual','predicted')
# pt_compare_20$degree = ((pt_compare_20$actual - pt_compare_20$predicted) / pt_compare_20$actual) * 100
# 
# 
# pt_compare_40 <- cbind(sg_data$total_cases, total_asymptomatic_40pt)
# pt_compare_40 = as.data.frame(pt_compare_40)
# colnames(pt_compare_40) <- c('actual','predicted')
# pt_compare_40$degree = ((pt_compare_40$actual - pt_compare_40$predicted) / pt_compare_40$actual) * 100
# 
# 
# pt_compare_60 <- cbind(sg_data$total_cases, total_asymptomatic_60pt)
# pt_compare_60 = as.data.frame(pt_compare_60)
# colnames(pt_compare_60) <- c('actual','predicted')
# pt_compare_60$degree = ((pt_compare_60$actual - pt_compare_60$predicted) / pt_compare_60$actual) * 100
# 
# 
# pt_compare_exp <- cbind(sg_data$total_cases, total_asymptomatic_exp)
# pt_compare_exp = as.data.frame(pt_compare_exp)
# colnames(pt_compare_exp) <- c('actual','predicted')
# pt_compare_exp$degree = ((pt_compare_exp$actual - pt_compare_exp$predicted) / pt_compare_exp$actual) * 100
# 
# par(mfrow = c(2,2))
# hist(pt_compare_20$degree, freq = FALSE)
# median(pt_compare_20$degree)
# hist(pt_compare_40$degree, freq = FALSE)
# median(pt_compare_40$degree)
# hist(pt_compare_60$degree, freq = FALSE)
# median(pt_compare_60$degree)
# hist(pt_compare_exp$degree, freq = FALSE)
# median(pt_compare_exp$degree)
# 
# 
# 
# 
# par(mfrow = c(2,2))
# plot(total_merged_seihrd$total_exp)
# lines(total_merged$seaihrd_exp)
# 
# plot(total_merged_seihrd$total_60)
# lines(total_merged$seaihrd_60pct)
# 
# plot(total_merged_seihrd$total_40)
# lines(total_merged$seaihrd_40pct)
# 
# plot(total_merged_seihrd$total_20)
# lines(total_merged$seaihrd_20pct)
# 
# 

p = c(0.156,0.25,0.40)
r = 1/3.85
beta = c(0.3804805, 0.95 * 0.3804805, (0.95^2) * 0.3804805)
beta_array <- c(rep(beta[1],16), rep(beta[2],48), rep(beta[3], 11))
k = 1/21
gamma = 2.6585186
b = 1/17 
c = 1/27
d = 1/9
e = 1/9


1/(gamma*b + c/gamma) + 1/(gamma * d + e/gamma)




p_1 = p[1] * r * beta_array / k + (1-p[1])*beta_array*(1/(gamma*b + c/gamma) + 1/(gamma*d + e/gamma))
p_2 = p[2] * r * beta_array / k + (1-p[2])*beta_array*(1/(gamma*b + c/gamma) + 1/(gamma*d + e/gamma))
p_3 = p[3] * r * beta_array / k + (1-p[3])*beta_array*(1/(gamma*b + c/gamma) + 1/(gamma*d + e/gamma))





par(mfrow = c(1,1))

plot(sg_data$date, p_1, col = "green", pch = "*", xlab = "Date", ylab = "Effective reproduction number", ylim = c(2.5,4), lty = 1)
lines(sg_data$date, p_1, col = "green", lty = 2)

points(sg_data$date, p_2, col='blue', pch = "*", lty = 1)
lines(sg_data$date, p_2, col = 'blue', lty = 2)

points(sg_data$date, p_3, col='black', pch = "*", lty = 1)
lines(sg_data$date, p_3, col = 'black', lty = 2)

legend("topleft", legend = c("p = 0.16", 
                             "p = 0.25",
                             "p  = 0.40"),
       col = c("green", "blue", "black"),
       lty = c(2,2,2), pch = c("*","*","*"), cex = 1)




