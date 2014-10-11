library(zoo)
library(xts)
library(timeSeries)

args(zoo)
args(xts)
args(timeSeries)
args(as.POSIXct) # default OS LOCALE timezone
args(ISOdatetime) # default OS LOCALE timezone
args(ISOdate) # default GMT timezone
args(timeDate) # not dependent on OS environment timezone

Sys.timezone()

## create date stamp for time series from date of character format
set.seed(1953)
data <- rnorm(6)
charvec <- paste('2014-0', 1:6, '-10', sep='')
zoo(data, as.Date(charvec))
xts(data, as.Date(charvec))
timeSeries(data, charvec)
# timeSeries(data, as.timeDate(charvec, FinCenter='Shanghai'))

set.seed(1953)
data <- matrix(rnorm(22), ncol=2)
now <- '2014-01-10'
zoo(data, as.Date(now)-0:10)
xts(data, as.Date(now)-0:10)
timeSeries(data, as.Date(now)-0:10)

## 基于POSIXct标识创建时间按序列对象
set.seed(1953)
data <- rnorm(6)
charvec <- paste('2014-0', 1:6, '-10', sep='')

zoo(data, as.POSIXct(charvec))
zoo(data, ISOdatetime(2014, 1:6, 10, 0, 0, 0))
zoo(data, ISOdate(2014, 1:6, 10, 0))

xts(data, as.POSIXct(charvec))
xts(data, ISOdatetime(2014, 1:6, 10, 0, 0, 0))
xts(data, ISOdate(2014, 1:6, 10, 0))

timeSeries(data, as.POSIXct(charvec))
timeSeries(data, ISOdatetime(2014, 1:6, 10, 0, 0, 0))
timeSeries(data, ISOdate(2014, 1:6, 10, 0))

## regular time series object
data <- round(rnorm(24), 4)
tm <- ts(data, start=c(2008,3), frequency=12)
zm <- zooreg(data, start=c(2008,3), frequency=12)
xm <- as.xts(tm)
sm <- as.timeSeries(tm)

print(sm, style='h')
print(sm, style='h', format='%Y %b')
print(sm, style='h', format='%Y(%m)')
