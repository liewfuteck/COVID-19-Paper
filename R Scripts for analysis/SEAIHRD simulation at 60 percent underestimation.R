# for 60 percent, beta = 0.4397397

library(deSolve)
library(reshape2)
library(ggplot2)
library(gridExtra)

# time intervals
time_1 <- seq(0,15,by=1)

sg_pop = 5.704 * 10^6

beta_value <- c(0.4397397, 0.95 * 0.4397397, (0.95^2) * 0.4397397)


# Parameters
parameters_asymptomatic <- c(beta = beta_value[1],
                             N = sg_pop,
                             r = 1/3.85, 
                             sigma = 1/5.1,
                             p = 0.156,
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
                             p = 0.156,
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
                             p = 0.156,
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

total_asymptomatic_60pt = total_asymptomatic_1


sg_data = read.table("C:/Users/User/OneDrive/Desktop/Singapore COVID-19 data.csv", sep = ",", header = TRUE)
sg_data = sg_data[1:75,4:5]
sg_data$date = as.Date(sg_data$date, '%d/%m/%Y')

par(mfrow = c(1,1))
plot(sg_data$date, sg_data$total_cases, col = 'red', pch = '*', xlab = "Date", ylab = "Cumulative Number of COVID-19 cases", ylim = c(0,5000))
lines(sg_data$total_cases, col = 'red', lty = 4)

lines(sg_data$date, total_asymptomatic_60pt, col = 'blue', pch = '-', lty = 4)
lines(total_asymptomatic_60pt, col = 'blue', lty = 4)

lines(sg_data$date, total_asymptomatic_60pt_80beta, col = 'green', pch = '-', lty = 4)
lines(total_asymptomatic_60pt_80beta, col = 'green', lty = 4)

legend("topleft", legend = c("5% reduction in \u03B2 at selected time points", 
                             "20% reduction in \u03B2 at selected time points",
                             "Reported number of cases"),
       col = c("blue", "green", "red"),
       lty = c(2,2,1), pch = c("*","*","*"), cex = 1)