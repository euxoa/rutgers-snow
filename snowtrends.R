library(plyr)
library(reshape2)
library(ggplot2)

setwd("~/Desktop/rutgers-snow")

snow <- read.table("http://climate.rutgers.edu/snowcover/files/moncov.nhland.txt")
names(snow) <- c("year", "month", "area")

#plot(snow, pch=".")

#ggplot(snow, aes(x=year, y=area, color=as.factor(month))) + geom_line() + geom_smooth() 
#ggplot(snow, aes(x=year, y=area, color=as.factor(month))) + geom_line() + 
#  geom_smooth(method="lm") + facet_wrap(~ month, scales="free_y")

month.plot <- function (fit, par) {
  tquant <- as.data.frame(t(apply(as.data.frame(fit, par), 2, 
                                  function (x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))))
  names(tquant) <- c("llow", "low", par, "high", "hhigh")
  tquant$month <- as.factor(1:12)
  ggplot(tquant, aes_string(x="month", y=par)) + 
    geom_pointrange(aes(ymin=llow, ymax=hhigh)) +
    geom_pointrange(aes(ymin=low, ymax=high), size=1) + theme_bw()
}

#library(rstan)
#library(rstan, lib.loc="/Users/scellus/rlib/")
library(foreach)
m <- stan_model("snowtrends.stan")
sdat <- list(T = nrow(snow), y = sqrt(snow$area/mean(snow$area)), month=as.integer(snow$month))
neffps <- 
foreach(i=1:10, .combine=c) %do% {
  st <- system.time(fit <- sampling(m, data = sdat, 
                                    #pars=c("err", "mum", "trend", "trend2", "sigma", "theta", "model_snow"), 
                iter = 5000, chains = 1, thin=1, init=0, nondiag_mass=T))
  perf <- exp(mean(log(as.data.frame(summary(fit)$summary)$n_eff)))/st["user.self"]
  cat(perf,"\n")
  perf
}


table(as.data.frame(get_sampler_params(fit))["treedepth__"])
traceplot(fit, inc_warmup=F, ask=T)
traceplot(fit, "trend2_sigma", inc_warmup=F, ask=T)
traceplot(fit, "trend_sigma", inc_warmup=F, ask=T)
plot(fit)

snow$err <- get_posterior_mean(fit, pars="err")[,1]
mpred <- as.data.frame(t(apply(as.data.frame(fit, "model_snow"), 2, 
                              function (x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))))
names(mpred) <- c("llow", "low", "pred", "high", "hhigh")
snow$pred <- get_posterior_mean(fit, pars="model_snow")[,1]
ggplot(snow, aes(x=year+(month-1)/12, y=err)) + geom_line() + geom_smooth()
ggplot(snow, aes(x=year, y=pred, color=as.factor(month))) + geom_line() +
   geom_ribbon(aes(ymin=mpred$llow, ymax=mpred$hhigh, linetype=NA, fill=as.factor(month)), alpha=0.2)
hist(snow$err, n=100)
acf(ts(snow$err))

pdf(file="trendplots.pdf")
month.plot(fit, "mum") + ylab("sqrt area") + ggtitle("Monthly sqrt area (standardized)")
month.plot(fit, "trend") + geom_hline(yintercept=0, color="red") + ggtitle("Sqrt area linear trends") + ylab("coef")
month.plot(fit, "trend2") + geom_hline(yintercept=0, color="red") + ggtitle("Trends: quadratic coeffs") + ylab("coef")
month.plot(fit, "sigma") + ggtitle("Sigma of t4(0, sigma) monthly residual")
month.plot(fit, "theta") + geom_hline(yintercept=0, color="red") + ggtitle("Modelled residual autocorrelation")
ggplot(snow, aes(x=year, y=err)) + 
  geom_point()  + facet_wrap(~ month, scales="free_y") + geom_hline(yintercept=0, color="red") + 
  ggtitle("Monthly residuals")
dev.off()
