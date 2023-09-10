#---Q1---
library(fpp)
library(fpp2)
library(forecast)
library(ggplot2)

#a) time plot of data
fancy
plot(fancy[], main = "Monthly Sales of Souvenir Shop",
     xlab= "Year",ylab="Monthly Sales") 

# b) tslm to fit regression model to log of fancy
fancy_log <- log(fancy)
fit <- tslm(fancy_log ~ trend + season, data = fancy)
summary(fit)

#c)Use multiple regression with trend variable and seasonal dummy variables to redo the regression
trend <- 1:length(fancy_log)
seasons <- seasonaldummy(fancy_log)
multi_reg.fancy <-lm(fancy_log ~ trend + seasons)
plot(multi_reg.fancy$fitted.values,type = "l")
summary(multi_reg.fancy)

#---Q2---
#plastics
library(unikn)

#a) Plot the time series of sales of product A
plot(plastics, main="Monthly Sales",
     xlab="Months",ylab="Sales",col = 1:10, pch=15)
seasonplot(plastics, main="Monthly Sales",
           xlab="Months",ylab="Sales",col = 1:10, pch=15)

#b) Perform a classical additive decomposition using stl function.
fit1 <- stl(plastics,  s.window = "periodic", robust = TRUE)
plot(fit1, main="Plot with s.window = periodic")
fit2 <- stl(plastics,  s.window = "periodic", t.window = 5, robust = TRUE)
plot(fit2, main="Plot with s.window = periodic, t.window = 5")
fit3 <- stl(plastics,  s.window = "periodic", t.window = 50, robust = TRUE)
plot(fit3, main="Plot with s.window = periodic, t.window = 50")
fit4 <- stl(plastics,  s.window = 5, t.window = 50, robust = TRUE)
plot(fit4, main="Plot with s.window = 5, t.window = 50")


#c) Compute and plot the seasonally adjusted data
season.adj = seasadj(fit3)
plot(season.adj,col='blue',ylab='seasonally adjusted')

#d) Change one observation to be an outlier and recompute
# outlier in the middle of the series
p2 <- plastics
p2[35] <- p2[35] + 500
fit5 <- stl(p2,  s.window = "periodic", t.window = 50, robust = TRUE)
fit5.adj <- seasadj(fit5)
plot(fit5.adj)

# outlier at the end of the series
p3 <- plastics
p3[60] <- p3[60] + 500
fit6 <- stl(p3,  s.window = "periodic", t.window = 50, robust = TRUE)
fit6.adj <- seasadj(fit6)
plot(fit6.adj)

