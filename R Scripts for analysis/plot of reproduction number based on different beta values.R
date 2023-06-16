# for 20 percent, beta = 0.3966967

# Based on 40% under-diagnosis, beta = 0.4157157

# for 60 percent, beta = 0.4397397

# if we follow the 0, 25th, 50th, 75th for exponential trend --> beta = 0.563964

# For no underestimation, beta = 0.3804805



# time intervals
time_1 <- seq(0,15,by=1)

time_2 <- seq(16,63,by=1)

time_3 <- seq(64,74,by=1)

beta_value_no_under <- c(0.3804805, 0.95 * 0.3804805, (0.95^2) * 0.3804805)
beta_value_20 <- c(0.3966967, 0.95 * 0.3966967, (0.95^2) * 0.3966967)
beta_value_40 <- c(0.4157157, 0.95 * 0.4157157, (0.95^2) * 0.4157157)
beta_value_60 <- c(0.4397397, 0.95 * 0.4397397, (0.95^2) * 0.4397397)
beta_value_exp <- c(0.563964, 0.95 * 0.563964, (0.95^2) * 0.563964)

tau = 9

r0_value_no_under = beta_value_no_under * tau
r0_value_20 = beta_value_20 * tau
r0_value_40 = beta_value_40 * tau
r0_value_60 = beta_value_60 * tau
r0_value_exp = beta_value_exp * tau

r0_value_no_under = c(rep(r0_value_no_under[1],length(time_1)),
                      rep(r0_value_no_under[2],length(time_2)),
                      rep(r0_value_no_under[3],length(time_3)))

r0_value_20 = c(rep(r0_value_20[1],length(time_1)),
                rep(r0_value_20[2],length(time_2)),
                rep(r0_value_20[3],length(time_3)))

r0_value_40 = c(rep(r0_value_40[1],length(time_1)),
                rep(r0_value_40[2],length(time_2)),
                rep(r0_value_40[3],length(time_3)))

r0_value_60 = c(rep(r0_value_60[1],length(time_1)),
                rep(r0_value_60[2],length(time_2)),
                rep(r0_value_60[3],length(time_3)))

r0_value_exp = c(rep(r0_value_exp[1],length(time_1)),
                rep(r0_value_exp[2],length(time_2)),
                rep(r0_value_exp[3],length(time_3)))


par(mfrow = c(1,1))

plot(sg_data$date, r0_value_no_under, col = "green", pch = "*", xlab = "Date", ylab = "Effective reproduction number", ylim = c(2.5,7), lty = 1)
lines(sg_data$date, r0_value_no_under, col = "green", lty = 2)

points(sg_data$date, r0_value_20, col='blue', pch = "*", lty = 1)
lines(sg_data$date, r0_value_20, col = 'blue', lty = 2)

points(sg_data$date, r0_value_40, col='black', pch = "*", lty = 1)
lines(sg_data$date, r0_value_40, col = 'black', lty = 2)

points(sg_data$date, r0_value_60, col='darkorchid', pch = "*", lty = 1)
lines(sg_data$date, r0_value_60, col = 'darkorchid', lty = 2)

points(sg_data$date, r0_value_exp, col='red', pch = "*", lty = 1)
lines(sg_data$date, r0_value_exp, col = 'red', lty = 2)

# legend("topleft", legend = c(as.expression(bquote("No underestimation:" ~ R[0] ~ "= 0.38")), 
#                              as.expression(bquote("20 percent underestimation:" ~ R[0] ~ "= 0.40")),
#                              as.expression(bquote("40 percent underestimation:" ~ R[0] ~ "= 0.42")),
#                              as.expression(bquote("60 percent underestimation:" ~ R[0] ~ "= 0.44")),
#                              as.expression(bquote("Exponential trend:" ~ R[0] ~ "= 0.56"))),
#        col = c("green", "blue", "black", "darkorchid", "red"),
#        lty = c(2,2,2,2,2), pch = c("*","*","*","*","*"), cex = 0.875)

legend("topleft", legend = c("No underestimation: \u03B2(0) = 0.38",
                             "20 percent underestimation: \u03B2(0) = 0.40",
                             "40 percent underestimation: \u03B2(0) = 0.42",
                             "60 percent underestimation: \u03B2(0) = 0.44",
                             "Exponential trend: \u03B2(0) = 0.56"),
       col = c("green", "blue", "black", "darkorchid", "red"),
       lty = c(2,2,2,2,2), pch = c("*","*","*","*","*"), cex = 0.875)



