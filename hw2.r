#------Question 1-------

#load quantmod library
library(quantmod)

#get t-bill data
tb3=getSymbols('TB3MS',src='FRED', auto.assign = FALSE)
tb=as.vector(tb3[937:1040])

#get stock data
ADBE=getSymbols('ADBE',from='2012-01-01',to='2020-08-31',auto.assign=FALSE)
ADBE.adj=ADBE$ADBE.Adjusted
ADBE.rtn.monthly=monthlyReturn(ADBE.adj,type='log')
ADBE.excess=ADBE.rtn.monthly-tb/(100*12)

#get S&P500 data
sp500=getSymbols('^GSPC',from='2012-01-01',to='2020-08-31',auto.assign=FALSE)
sp500.adj=sp500$GSPC.Adjusted
sp500.rtn.monthly=monthlyReturn(sp500.adj,type='log')
sp500.excess=sp500.rtn.monthly-tb/(100*12)


linear_model <- lm(ADBE.excess ~ sp500.excess)
summary(linear_model)

output.fitted = as.vector(linear_model$fitted.values)
head(output.fitted)

#-------Question 2---------
# ---- a -----
library(MASS)
data(Boston)
fit<-lm(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,data = Boston)
summary(fit)
step(fit, k=2) # AIC; default is AIC and k=2 also gives genuine aic
step(fit, k=log(506)) # BIC or SBC; k=log(n) gives bic or sbc

# ----- b -----
hist(Boston $medv)

fit1<-lm(log(medv) ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,data = Boston)
summary(fit1)
step(fit1, k=2) # aic 
step(fit1,k=log(506)) # bic
