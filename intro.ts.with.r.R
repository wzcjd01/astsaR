## introductory time series with R
## Use R!
## Paul S.P. Cowpertwait, Andrew V. Metcalfe
## Springer 2009

setwd("E:/R/cowpertwait")
# setwd("~/doc/R/cowpertwait")

############################################################
## section 1.4.1 air passenger bookings
############################################################
data(AirPassengers)
AP <- AirPassengers
class(AP)
start(AP); end(AP); frequency(AP)
plot(AP, ylab="Passengers (1000's)")

# to get a clearer view of the trend, the seasonal effect can be removed by aggregate the data to the annual level.
layout(1:2)
plot(aggregate(AP)) # 类似于excel中的数据透视表
boxplot(AP ~ cycle(AP))  # seasonal effect

############################################################
## section 1.4.2 unemployment: Maine
############################################################
Maine.month <- read.table("Maine.dat", header=TRUE)
attach(Maine.month)
class(Maine.month)
Maine.month.ts <- ts(unemploy, start=c(1996,1), freq=12)
Maine.annual.ts <- aggregate(Maine.month.ts) / 12

layout(1:2)
plot(Maine.month.ts, ylab='unemployment (%)')
plot(Maine.annual.ts, ylab='unemployment (%)')

Maine.Feb <- window(Maine.month.ts, start=c(1996,2), freq=TRUE)
Maine.Aug <- window(Maine.month.ts, start=c(1996,8), freq=TRUE)
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts) # on average, unemployment is 22% higher in February and 18% lower in August.

############################################################
## section 1.4.5 global temperature series
############################################################
Global <- scan('global.dat')
Global.ts <- ts(Global, start=c(1856,1), end=c(2005,12), freq=12)
Global.annual <- aggregate(Global.ts, FUN=mean)
par(mfrow=c(2,1))
plot(Global.ts)
plot(Global.annual)

New.series <- window(Global.ts, start=c(1970,1), end=c(2005,12))
New.time <- time(New.series)
plot(New.series)
abline(reg=lm(New.series ~ New.time))

############################################################
## decomposition in R
############################################################
CBE <- read.table('cbe.dat', header=TRUE)
class(CBE)
elec.ts <- ts(CBE[,3], start=1958, freq=12)
beer.ts <- ts(CBE[,2], start=1958, freq=12)
choc.ts <- ts(CBE[,1], start=1958, freq=12)

plot(decompose(elec.ts)) # additive model
elec.decom <- decompose(elec.ts, type='mult') # multiplicative model
plot(elec.decom)
trend <- elec.decom$trend
seasonal <- elec.decom$seasonal
ts.plot(cbind(trend, trend * seasonal), lty=1:2)

############################################################

############################################################
beer.ts <- ts(CBE[,2], start=1958, freq=12)
plot.ts(beer.ts)
beer.annual <- aggregate(beer.ts, FUN=sum)
plot.ts(beer.annual)
boxplot(beer.ts ~ cycle(beer.ts))  # NZ is in the south hemisphere

beer.decomp <- decompose(beer.ts)
beer.trend <- beer.decomp$trend
beer.seasonal <- beer.decomp$seasonal
beer.random <- beer.decomp$random
ts.plot(cbind(beer.trend, beer.trend + beer.seasonal), lty=1:2, col=c('blue', 'red'))

############################################################
## exercise 1.7.2
############################################################
# Laspeyre Index
base.qty <- c(.33, 2000, 40, 3, 2)
base.price <- c(18000, .80, 40, 80, 200)
now.qty <- c(.5, 1500, 20, 2, 1)
now.price <- c(20000, 1.6, 60, 120, 360)

LI.now <- sum(base.qty * now.price) / sum(base.qty * base.price) # Laspeyre Price Index at now relative to base year
LI.now

############################################################
## exercise 1.7.3
############################################################
PI.now <- sum(now.qty * now.price) / sum(now.qty * base.price) # Paasche Price Index at now relative to base year
PI.now

IFI.now <- sqrt(LI.now * PI.now) # Irving-Fisher Price Index
IFI.now


############################################################
## section 2.2
############################################################
wave <- read.table('wave.dat', header=TRUE)
plot(ts(wave$waveht))
plot(ts(wave$waveht[1:60]))

############################################################
## section 2.3 the correlogram
############################################################
par(mfrow=c(2,1))
acf(ts(wave$waveht))
pacf(ts(wave$waveht))

# whilst the population acf was defined only for a stationary model, the sample acf can be calculated for any time series, including deterministic signals.
trend.only <- ts(1:1000)
par(mfrow=c(2,1))
acf(trend.only)
pacf(trend.only)

sinusoidal <- 20 * sin(2*pi*(1:1000)*12/1000)
plot.ts(sinusoidal)
par(mfrow=c(2,1))
acf(sinusoidal)
pacf(sinusoidal)

rand.rep <- ts(rep(rnorm(20), 50))
plot.ts(rand.rep)
par(mfrow=c(2,1))
acf(rand.rep)
pacf(rand.rep)

############################################################
## section 2.3.2 example based on air passenger series
############################################################
data(AirPassengers)
ap <- AirPassengers
ap.decom <- decompose(ap, 'multiplicative')
plot(ts(ap.decom$random[7:138]))

acf(ap.decom$random[7:138]) # suggust AR(2) or I(1)_12 (seasonal adustment has not been entirely effective)
pacf(ap.decom$random[7:138])

sd(ap[7:138])  # all
sd(ap[7:138] - ap.decom$trend[7:138]) # trend removed
sd(ap.decom$random[7:138]) # only random series
# the reduction in the standard deviation shows that the seasonal adjustment has been very effective

############################################################
## section 2.3.3 example based on the Font Reservoir series
############################################################
fontdsdt <- read.table('Fontdsdt.dat', header=TRUE)
head(fontdsdt)
class(fontdsdt)
attach(fontdsdt)
plot(ts(adflow), ylab='adflow')
acf(adflow, xlab='lag (months)', main='')

############################################################
## section 3.2.2 ccvf, ccf: building approvals and building activity
############################################################
build.dat <- read.table('ApprovActiv.dat', header=TRUE)
attach(build.dat)

app.ts <- ts(Approvals, start=c(1996,1), freq=4)
act.ts <- ts(Activity, start=c(1996,1), freq=4)

ts.plot(app.ts, act.ts, lty=c(1,3))
acf(ts.union(app.ts, act.ts))

# random component of the time series
app.ran <- decompose(app.ts)$random
app.ran.ts <- window(app.ran, start=c(1996,3), end=c(2006,1))
act.ran <- decompose(act.ts)$random
act.ran.ts <- window(act.ran, start=c(1996,3), end=c(2006,1))

acf(ts.union(app.ran.ts, act.ran.ts))
ccf(app.ran.ts, act.ran.ts)
print(acf(ts.union(app.ran.ts, act.ran.ts)))


############################################################
## 3.3 Bass model
############################################################
# adoption and diffusion of a new product by society
T79 <- 1:10
Tdelta <- 1:100 / 10
Sales <- c(840, 1470, 2110, 4000, 7590, 10950, 10530, 9470, 7790, 5890)
Cum.sales <- cumsum(Sales)
Bass.nls <- nls(Sales ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * T79)) /
                (1+(Q/P) * exp(-(P+Q) * T79))^2,
                start=list(M=60630, P=.03, Q=.38))
summary(Bass.nls)

Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p+q) * Tdelta)

Bpdf <- m * ((p+q)^2 / p) * ngete / (1+(q/p) * ngete)^2
plot(Tdelta, Bpdf, xlab='Year from 1979', ylab='Sales per year', type='l')
points(T79, Sales)

Bcdf <- m * (1-ngete)/(1+(q/p)*ngete)
plot(Tdelta, Bcdf, xlab='Year from 1979', ylab='cumulative sales', type='l')
points(T79, Cum.sales)

############################################################
## 3.4 exponential smoothing & the Holt-Winters method
############################################################
