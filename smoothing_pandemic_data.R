library(forecast)

# Let's do some smoothing on the Sweden data
sweden <- df %>% 
  filter(Country == "Sweden") %>% 
  mutate(asmr = as.ts(asmr))

# Moving average using the ma() function
sweden$asmr_12_w <- ma(sweden$asmr, 12) # 12 weeks
sweden$asmr_24_w <- ma(sweden$asmr, 24) # 24 weeks
sweden$asmr_36_w <- ma(sweden$asmr, 36) # 36 weeks

# plot of the different moving averages
plot(sweden$asmr, col = "grey", font.main = 1,
     main = "Moving average Sweden ASMR, weekly, 2015 - 2022", las = 1,
     xlab = "weeks", ylab = "rate per 100k")
lines(sweden$asmr_12_w, col = "blue", lwd = 1.5)
lines(sweden$asmr_24_w, col = "red", lwd = 1.5)
lines(sweden$asmr_36_w, col = "chartreuse4", lwd = 1.5)
legend("topleft", legend = c("ma = 12w", "ma = 24w", "ma = 36w"),
       col = c("red", "blue", "chartreuse4"), lwd = 1.5)

# kernel smoothing
plot(time(sweden$asmr), sweden$asmr, col = "grey", font.main = 1,
     main = "Kernel smoothed Sweden ASMR, weekly, 2015 - 2022", las = 1,
     xlab = "weeks", ylab = "rate per 100k", type = 'l')
lines(ksmooth(time(sweden$asmr), sweden$asmr, "box", 
              bandwidth = 8), col = "red", lwd = 1.5)
lines(ksmooth(time(sweden$asmr), sweden$asmr, "box", 
              bandwidth = 16), col = "blue", lwd = 1.5)
lines(ksmooth(time(sweden$asmr), sweden$asmr, "box", 
              bandwidth = 24), col = "chartreuse4", lwd = 1.5)
legend("topleft", title = "bandwidth", legend = c("8", "16", "24"),
       col = c("red", "blue", "chartreuse4"), lwd = 1.5)

sweden <- sweden %>% filter(!is.na(asmr))
# spline smoothing
plot(sweden$asmr, col = 'grey', main = "Spline smoothing Sweden ASMR, weekly, 2015-2022",
     las = 1, xlab = "weeks", ylab = "rate per 100k", font.main = 1, type = 'l')
lines(smooth.spline(time(sweden$asmr), sweden$asmr, spar=.2), lwd=1.5, col="red")
lines(smooth.spline(time(sweden$asmr), sweden$asmr, spar=.4), lwd=1.5, col="blue")
lines(smooth.spline(time(sweden$asmr), sweden$asmr, spar=.6), lwd=1.5, col="chartreuse4")
legend("topleft", legend = c('spar = .2', 'spar = .4', 'spar = .6'), lwd = 1.5,
       col = c("red", "blue", "chartreuse4"), cex = .8)

# smoothing using the supsmu function
plot(time(sweden$asmr), sweden$asmr, col = "grey",
     main = "Smoothing using supsmu()")
lines(supsmu(time(sweden$asmr), sweden$asmr), col = "red")
