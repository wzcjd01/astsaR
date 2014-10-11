## ARIMA model, Box-Jenkens method
# 1. build linear models based on classical regression theory for exploiting the associations indicated by large values of the ACF or CCF that may occur between time series. Static Analysis
# 2. autoregression with the time series. Dynamic Forecasting

## example 3.1 the sample path of an AR(1) process
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0), ar=.9), n=100),
     ylab='x', main=(expression('AR(1) '*phi*' = +.9')))
plot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100),
     ylab='x', main=(expression('AR(1) '*phi*' = -.9')))

## exmaple 3.3 autocorrelation and sample path of an MA(1) process
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(0,0,1), ma=.5), n=100),
     ylab='x', main=(expression('MA(1) '*theta*' = .5')))
plot(arima.sim(list(order=c(0,0,1), ma=-.5), n=100),
     ylab='x', main=(expression('MA(1) '*theta*' = -.5')))

## example 3.9 the sample path of an AR(2) with complex roots
set.seed(5)
ar2 <- arima.sim(list(order=c(2,0,0), ar=c(1.5, -.75)), n=144)
plot.ts(ar2, axes=F); box(); axis(2)
axis(1, seq(0,144,24))
abline(v=seq(0,144,12), lty='dotted')

acf <- ARMAacf(ar=c(1.5, -.75), ma=0, 50)
plot(acf, type='h', xlab='lag')
abline(h=0)

## example 3.10 determining the psi-weights for a causal ARMA(p, q)
ARMAtoMA(ar=.9, ma=.5, 50)
plot(ARMAtoMA(ar=.9, ma=.5, 50))
