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

month.plot <- function (fit, par) {
  tquant <- as.data.frame(t(apply(as.data.frame(fit, par), 2, 
                                  function (x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))))
  names(tquant) <- c("llow", "low", par, "high", "hhigh")
  tquant$month <- as.factor(1:12)
  ggplot(tquant, aes_string(x="month", y=par)) + 
    geom_pointrange(aes(ymin=llow, ymax=hhigh)) +
    geom_pointrange(aes(ymin=low, ymax=high), size=1) + theme_bw(30)
}

sdat <- list(T = nrow(snow), y = snow$area/mean(snow$area), month=as.integer(snow$month))
m <- stan_model("snowacc.stan")
n.iter <- 1000
fit <- sampling(m, data = sdat, pars=c("err", "acc", "trend", "sigma"), 
                iter = n.iter, chains = 1, thin=1, init=0, nondiag_mass=T)
plot(fit)

snow$err <- get_posterior_mean(fit, pars="err")[,1]
ggplot(snow, aes(x=year+(month-1)/12, y=err)) + geom_line() + geom_smooth()
hist(snow$err, n=100)
ggplot(snow, aes(x=year, y=err, color=as.factor(month))) + geom_point()  + facet_wrap(~ month, scales="free_y")
month.plot(fit, "acc")
month.plot(fit, "trend")

