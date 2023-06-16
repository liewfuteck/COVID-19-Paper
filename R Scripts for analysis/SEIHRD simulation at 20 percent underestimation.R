# Based on 20% under-diagnosis, beta = 0.3474474

library(deSolve)
library(reshape2)
library(ggplot2)
library(gridExtra)

# time intervals
time <- seq(0,74,by=1)

beta_value = 0.3474474

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

total_20 = total_1


sg_data = read.table("C:/Users/User/OneDrive/Desktop/Singapore COVID-19 data.csv", sep = ",", header = TRUE)
sg_data = sg_data[1:75,4:5]
sg_data$date = as.Date(sg_data$date, '%d/%m/%Y')


total_merged_seihrd <- cbind(as.data.frame(sg_data$date), sg_data$total_cases, total_20, total_40, total_60, total_exp)
colnames(total_merged_seihrd) <- c("date", "total_cases", "total_20", "total_40", "total_60", "total_exp")

total_merged_seihrd$date[total_merged_seihrd$total_20 > 1375][1]
total_merged_seihrd$date[total_merged_seihrd$total_40 > 1375][1]
total_merged_seihrd$date[total_merged_seihrd$total_60 > 1375][1]
total_merged_seihrd$date[total_merged_seihrd$total_exp > 1375][1]



par(mfrow = c(1,1))

plot(sg_data$date, total_20, col = "green", pch = "*", xlab = "Date", ylab = "Cumulative Number of COVID-19 cases", ylim = c(0,5000), lty = 1)
lines(sg_data$date, total_20, col = "green", lty = 2)

points(sg_data$date, total_40, col='blue', pch = "*", lty = 1)
lines(sg_data$date, total_40, col = 'blue', lty = 2)

points(sg_data$date, total_60, col='black', pch = "*", lty = 1)
lines(sg_data$date, total_60, col = 'black', lty = 2)

points(sg_data$date, total_exp, col='darkorchid', pch = "*", lty = 1)
lines(sg_data$date, total_exp, col = 'darkorchid', lty = 2)

points(sg_data$date, sg_data$total_cases, col='red', pch = "*", lty = 1)
lines(sg_data$date, sg_data$total_cases, col = 'red', lty = 1)

legend("topleft", legend = c("20 percent asymptomatic cases; \u03B2 = 0.35", 
                             "40 percent asymptomatic cases; \u03B2 = 0.36",
                             "60 percent asymptomatic cases; \u03B2 = 0.39",
                             "Exponential trend; \u03B2 = 0.51", 
                             "Reported number of cases"),
       col = c("green", "blue", "black","darkorchid", "red"),
       lty = c(2,2,2,2,1), pch = c("*","*","*","*","*"), cex = 1.2)







