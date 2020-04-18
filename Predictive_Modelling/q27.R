df <- read.csv("Sept11Travel.csv")
names(df)[names(df) == "Air.RPM..000s."] <- "Air"
names(df)[names(df) == "Rail.PM"] <- "Rail"
names(df)[names(df) == "VMT..billions."] <- "Vehicle"

## A
library(forecast)
air.ts <- ts(df$Air,start = c(1990,1),end = c(2004,4),freq=12)
rail.ts <- ts(df$Rail,start = c(1990,1),end = c(2004,4),freq=12)
vehicle.ts <- ts(df$Vehicle,start = c(1990,1),end = c(2004,4),freq=12)

nValid <- 32
air_train <- window(air.ts, start = c(1990,1), end = c(2001,8))
air_valid <- window(air.ts, start = c(2001,9), end = c(2004,4))

plot(air_train,xlab="Time",ylab="Air",ylim=c(min(df$Air), max(df$Air)),bty="l")

## B
train.lm.season <- tslm(air_train ~ trend + season, lambda = 0)
train.lm.season.pred <- forecast(train.lm.season, h=nValid, level=0)
print(summary(train.lm.season))

## C Done

## D
Acf(train.lm.season$residuals,lag.max=12, main="")

## E
rail_train <- window(rail.ts, start = c(1990,1), end = c(2001,8))
rail_valid <- window(rail.ts, start = c(2001,9), end = c(2004,4))
train.lm.quad <- tslm(rail_train ~ poly(trend, 2, raw=TRUE) + season)
train.lm.quad.pred <- forecast(train.lm.quad, h=nValid, level=0)
print(summary(train.lm.quad))

## F
car_train <- window(vehicle.ts, start = c(1990,1), end = c(2001,8))
car_valid <- window(vehicle.ts, start = c(2001,9), end = c(2004,4))
train.linear.season <- tslm(car_train ~ trend + season)
train.linear.pred <- forecast(train.linear.season, h=nValid, level=0)
print(summary(train.linear.season))

## G
# Air
plot(train.lm.season.pred,  ylab = "Air", ,bty='l',xlab = "Time",xaxt="n", ylim=c(min(df$Air), max(df$Air)),xlim = c(1990,2004), main = "", flty = 2)
axis(1, at = seq(1990, 2004, 1)) 
lines(train.lm.season$fitted, lwd = 2)
lines(air_valid)
grid()
lines(c(2001.67, 2001.67), c(min(df$Air), max(df$Air)),lwd=3,col="red") 
text(1998, 30000000, "Training",cex=1)
text(2002, 40000000, "Validation",cex=1)
text(1992, 65000000, "Air", cex=1.5)

# Rail
plot(train.lm.quad.pred,  ylab = "Rail", ,bty='l',xlab = "Time",xaxt="n", ylim=c(min(df$Rail), max(df$Rail)),xlim = c(1990,2004), main = "", flty = 2)
axis(1, at = seq(1990, 2004, 1)) 
lines(train.lm.quad$fitted, lwd = 2)
lines(rail_valid)
grid()
lines(c(2001.67, 2001.67), c(min(df$Rail), max(df$Rail)),lwd=3,col="red") 
text(1992, 350000000, "Training",cex=1)
text(2002, 350000000, "Validation",cex=1)
text(1997, 650000000, "Rail", cex=1.5)

# Car
plot(train.linear.pred,  ylab = "Car", ,bty='l',xlab = "Time",xaxt="n", ylim=c(min(df$Vehicle), max(df$Vehicle)),xlim = c(1990,2004), main = "", flty = 2)
axis(1, at = seq(1990, 2004, 1)) 
lines(train.linear.season$fitted, lwd = 2)
lines(car_valid)
grid()
lines(c(2001.67, 2001.67), c(min(df$Vehicle), max(df$Vehicle)),lwd=3,col="red") 
text(1996, 160, "Training",cex=1)
text(2002, 160, "Validation",cex=1)
text(1994, 250, "Car", cex=1.5)