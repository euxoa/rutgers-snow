library(plyr)
library(reshape2)
library(ggplot2)
library(rstan)

setwd("~/Desktop/rutgers-snow")

snow <- read.table("snow.txt")
names(snow) <- c("year", "month", "area")

plot(snow, pch=".")

ggplot(snow, aes(x=year, y=area, color=as.factor(month))) + geom_line() + geom_smooth() 
ggplot(snow, aes(x=year, y=area, color=as.factor(month))) + geom_line() + 
  geom_smooth(method="lm") + facet_wrap(~ month, scales="free_y")


sdat <- list(T = nrow(snow), y = snow$area/mean(snow$area), month=as.integer(snow$month))
n.iter <- 1000
m <- stan_model("snowtrends.stan")
fit <- sampling(m, data = sdat, pars=c("err", "mum", "trend", "trend2", "theta", "phi", "sigma"), 
            iter = n.iter, chains = 1, thin=1, nondiag_mass=T, init=0)
plot(fit)

samples <- as.data.frame(fit)
snow$err <- get_posterior_mean(fit, pars="err")[,1]
plot(snow$err, type="l") 
plot(get_posterior_mean(fit, pars="mum"), type="l")

ggplot(snow, aes(x=year+(month-1)/12, y=err)) + geom_line() + geom_smooth()
ggplot(snow, aes(x=year, y=err, color=as.factor(month))) + geom_point()  + facet_wrap(~ month)
ggplot(snow, aes(x=year, y=err, color=as.factor(month))) + geom_line() + 
  geom_smooth(method="lm") + facet_wrap(~ month, scale="free_y")

ddply(snow, .(month), function (d) sd(d$err))

tquant <- as.data.frame(t(apply(as.data.frame(fit, "trend"), 2, 
                                function (x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))))
names(tquant) <- c("llow", "low", "trend", "high", "hhigh")
tquant$month <- as.factor(1:12)
ggplot(tquant, aes(x=month, y=trend)) + 
  geom_pointrange(aes(ymin=llow, ymax=hhigh)) +
  geom_pointrange(aes(ymin=low, ymax=high), size=1) + theme_bw(30)

tquant <- as.data.frame(t(apply(as.data.frame(fit, "trend2"), 2, 
                                function (x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))))
names(tquant) <- c("llow", "low", "trend", "high", "hhigh")
tquant$month <- as.factor(1:12)
ggplot(tquant, aes(x=month, y=trend)) + 
  geom_pointrange(aes(ymin=llow, ymax=hhigh)) +
  geom_pointrange(aes(ymin=low, ymax=high), size=1) + theme_bw(30)

