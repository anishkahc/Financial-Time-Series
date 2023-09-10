#----------- Question 1 ----------------
a<-seq(1,149, by =2.0)
a
b <- a[a < 40 | a > 100]
b
var(b)

# ---------- Question 2 ---------------
2*1:5
(2*1):5

# ---------- Question 3 -------------
x <- seq(2, 99, by = 2)
x

# ---------- Question 4 ------------
y=c(-1,2,-3,4,-5,7,-8,9,-10,13)
z<-prod(y [y>0])
z

# ----------- Question 5 ----------
qchisq(0.95, df=10)

# ----------- Quesion 6 -----------
x=rnorm(1000)
quantile(x, probs = c(0.01,0.05,0.95,0.99))
mean(x)         
var(x)         

y=rt(1000,3)
quantile(y, probs = c(0.01,0.05,0.95,0.99))
mean(y)
var(y)

hist(x)
hist(y)
