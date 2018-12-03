library(lmtest)
library(car)
library(forecast)
library(fpp)
detach()
dev.off()
###################  chap 6 #####################  #
hotel <- read.table(file.choose(), header=T)
y <- hotel$y
n <- length(y)
t <- c(1:n)


##########################Autocorrelation ########################
ybar <- mean(y)
y1 <- y[2:n]
y2 <- y[1:(n-1)]
sp <- sum((y1-ybar)*(y2-ybar))
ss <- sum((y-ybar)^2)  
r.k <- sp/ss
acf <- acf(cod, type = "correlation")

########################  #######################################

######################Durbin-Watson ###################
fit <- lm(y~t)
yhat <- fitted(fit)
e <- y-yhat
x1 <- e[2:n]
x2 <- e[1:(n-1)]
d <- (sum((x1-x2)^2))/ (sum(e^2))

dwtest(y~t,data=motel)
##################### seasonal variation ##################################
hotelts <- ts(hotel,frequency=12, start =c(1980,1))
plot.ts(hotelts,ylab="Room Averages(y)", xlab="Time")
abline(mean(y),0, col="red")

loghotelts <- log(hotelts)
plot.ts(loghotelts,ylab="Room Averages(log y)", xlab="Time")
abline(mean(log(y)),0, col="red")

sqrthotelts <- sqrt(hotelts)
plot.ts(sqrthotelts,ylab="Room Averages(log y)", xlab="Time")
abline(mean(sqrt(y)) ,0, col="red")
############### Decomposing seasonal data ##################
hotelts.compo <- decompose(hotelts)

hotelts.compo$seasonal
hotelts.compo$trend
plot(hotelts.compo)


############################ residual ####################
res <- ts(resid(fit1))
plot.ts(res,ylab="residual")
abline(0,0, col="blue")


###################################  Trend and Seasonality   ##################################
fit <- tslm(loghotelts ~ trend + season)
summary(fit)
b <- coefficients(fit)

f <- forecast(fit, h=12, level =95)
dwtest(fit)



plot.ts(loghotelts,ylab="Room Averages(log y)", xlab="Time",col="blue")
lines(fitted(fit),col="red")



######################################### Arima ##########################################
df <-data.frame(log(y), trend=t, Month=rep(1:12,14))
xreg <- cbind(trend=df$trend, Month=model.matrix(~as.factor(df$Month)))
xreg <- xreg[,-2]  # remove intercept
hotelarima <- ts(df$log.y, frequency = 12)
colnames(xreg) <- c("trend","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec")

modArima <- arima(hotelarima, xreg=xreg, order=c(1,0,0))
summary(modArima)
b <- coefficients(modArima)
s <- 0.01867544

epsilon <- log(y[168])- (b[2] + b[3]*168)
yhat <- b[2]+ b[3]*169 +b[1]*epsilon

########################### PI  #############################

ll <- yhat - qnorm(.975)*s
ul <- yhat + qnorm(.975)*s

##########################################################################################

















