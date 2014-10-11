library(astsa)
help(astsa)
astsadata()

## example 1.1
plot(jj, ylab = 'Quarterly Earnings per Share')

## example: earthquakes and explosion
head(EQ5)
oldpar <- par(mfrow = c(2,  1))
plot.ts(EQ5, main = 'earthquake', ylab = 'EQ5')
plot.ts(EXP6, main = 'explosion', ylab = 'EXP6')
# ex 1.1
plot.ts(EQ5, lty=1, col='blue')
lines.ts(EXP6, lty=2, col='red')
legend('topleft', lty=1:2, col=c('blue', 'red'),
       legend=c('earthquake', 'explosion'))

## example 1.8 gausian white noise
wn <- rnorm(500, 0, 1)
plot.ts(wn)

## example 1.9 moving average
v <- filter(wn, sides=2, rep(1,3)/3)
par(mfrow = c(2,1))
plot.ts(wn)
plot.ts(v)

## example 1.10 autoregressions
w <- rnorm(550, 0, 1)
x <- filter(w, filter=c(1, -.9), method='recursive')
plot.ts(x[51:550])

## example 1.11 randow walk with drift
set.seed(154)
w <- rnorm(200, 0, 1)
x <- cumsum(w)
wd <- w + .2
xd <- cumsum(wd)
plot.ts(xd, ylim=c(-5, 55))
lines(x)
lines(.2*(1:200), lty='dashed')

## example 1.12 signal in noise
t <- 1:500
c <- 2 * cos(2*pi*t/50 + .6*pi)
w <- rnorm(500, 0, 1)
par(mfrow=c(3,1))
plot.ts(c)
plot.ts(c + w)  # signal is easily dicernible
plot.ts(c + 5*w) # signal is obscured by the noise

## stationarity of a moving average of white noise
# figure 1.12 page 26
plot(-2:2, c(1,2,3,2,1)/9, type='h')
plot(-2:2, c(1,2,3,2,1)/9, type='h', xlim=c(-10,10), xlab='Logs (h)',
     ylab='Autocovariance', lwd=2,
     sub='autocovariance function of a three-point MV of a white noise')

## example 1.24 ACF of speech signal
acf(speech, 250)

## example 1.25 correlation analysis of SOI and Recruitment Data
par(mfrow=c(2,1))
plot.ts(soi)
plot.ts(rec)

par(mfrow=c(3,1))
acf(soi, 50)
acf(rec, 50)
ccf(soi, rec, 50)

## example 1.26
data(soiltemp)
plot(soiltemp)

## ex.1.2
wn <- rnorm(200, 0, 1)
sa <- c(); sb <- c(); sc <- c()

t <- 1:100
sa[t] <- 0
sa[100+t] <- 10 * exp(-t/20) * cos(2*pi*(t+100)/4)
sa <- sa + wn

sb[t] <- 0
sb[100+t] <- 10 * exp(-t/200) * cos(2*pi*(t+100)/4)
sb <- sb + wn

par(mfrow=c(2,1))
plot.ts(sa, lty=1, col='blue')
lines.ts(sb, lty=2, col='red')
legend('topleft', lty=1:2, col=c('blue', 'red'),
       legend=c('series a', 'series b'))
plot.ts(EXP6, lty=1, col='blue')
lines.ts(EQ5, lty=1, col='red')
legend('topleft', lty=1:2, col=c('blue', 'red'),
       legend=c('explosion', 'earthquake'))

curve(exp(-x/20), 1, 100, col='red')
curve(exp(-x/200), 1, 100, add=TRUE)

## exercise 1.3
wn <- rnorm(120, 0, 1)
x <- filter(wn, filter=c(0, -.9), method='recursive')
v <- filter(x, sides=1, filter=c(1,1,1,1)/4, method='convolution')
plot.ts(x)
lines.ts(v, lty='dashed', col='red')

x <- cos(2*pi*1:100/4)
v <- filter(x, sides=1, filter=c(1,1,1,1)/4, method='convolution')
plot.ts(x)
lines.ts(v, lty='dashed', col='red')


x <- cos(2*pi*1:100/4) + rnorm(100, 0, 1)
v <- filter(x, sides=1, filter=c(1,1,1,1)/4, method='convolution')
plot.ts(x)
lines.ts(v, lty='dashed', col='red')

