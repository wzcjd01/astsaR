library(astsa)
astsadata()

## example 2.1
class(gtemp)
start(gtemp); end(gtemp); frequency(gtemp)
t <- start(gtemp):end(gtemp)
fit <- lm(gtemp ~ t)
summary(fit)
plot(t, gtemp, type='o', xlab='year', ylab='temp deviation')
abline(fit)
