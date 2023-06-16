# Based on exponential trend, beta = 0.3712713

# New value: beta = 0.5141141

library(deSolve)
library(reshape2)
library(ggplot2)
library(gridExtra)

# time intervals
time <- seq(0,74,by=1)

beta_value = 0.5141141

parameters <- c(beta = beta_value,
                N = 5.704 * 10^6,
                sigma = 1/5.1,
                tau = 1/4,
                alpha = 1/13,
                delta = 1/18)

# set initial conditions

states <- c(S = 5.704 * 10^6,
            E = 10,
            I = 10,
            H = 0,
            R = 0,
            D = 0,
            check = 0)

covid <- function(t, states, parameters){
  with(as.list(c(states, parameters)),{
    dS = (-beta * S * I) / N
    dE = (beta * S * I)/N - sigma * E
    dI = sigma * E - tau * I
    dH = tau * I - alpha * H - delta * H
    dR = alpha * H
    dD = delta * H
    dcheck = dS + dE + dI + dH + dR + dD
    return(list(c(dS, dE, dI, dH, dR, dD, dcheck)))
  })
}

out <- ode(y = states, times = time, 
           func = covid, parms = parameters)

out.df = as.data.frame(out)

out.df$'total' <- rowSums(out.df) - out.df$S

susceptible_1 = out.df$S
exposed_1 = out.df$E
infected_1 = out.df$I
hospital_1 = out.df$H
recovered_1 = out.df$R
death_1 = out.df$D
total_1 = out.df$total
check_1 = out.df$check

total_exp = total_1


sg_data = read.table("C:/Users/User/OneDrive/Desktop/Singapore COVID-19 data.csv", sep = ",", header = TRUE)
sg_data = sg_data[1:75,4:5]
sg_data$date = as.Date(sg_data$date, '%d/%m/%Y')

par(mfrow = c(1,1), cex = 0.75)
plot(sg_data$date, sg_data$total_cases, col = 'red', pch = '*', xlab = "Date", ylab = "Cumulative Number of COVID-19 cases", ylim = c(0,2000))
lines(sg_data$total_cases, col = 'red', lty = 2)

points(sg_data$date, total_exp, col='green', pch = "*", lty = 1)
lines(sg_data$date, total_exp, col = 'green', lty = 2)