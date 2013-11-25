library(plyr)
library(reshape2)
library(ggplot2)
library(rstan) # Works with 1.3

snow <- read.table("http://climate.rutgers.edu/snowcover/files/moncov.nhland.txt")
names(snow) <- c("year", "month", "area")
# sqrt() is on the scale of latitude (roughly)
snow$y <- sqrt(snow$area/mean(snow$area))

# Plots of raw data
pdf(file="data-plots.pdf")
ggplot(snow, aes(x=1:nrow(snow), y=y)) + geom_line()
ggplot(snow, aes(x=year, y=y)) + facet_wrap(~ month) + geom_line() 
ggplot(snow, aes(x=year, y=y)) + facet_wrap(~ month, scales="free_y") + 
  geom_line() + geom_smooth(method="loess")
dev.off()

month.plot <- function (fit, par) {
  tquant <- as.data.frame(t(apply(as.data.frame(fit, par), 2, 
                                  function (x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))))
  names(tquant) <- c("llow", "low", par, "high", "hhigh")
  tquant$month <- as.factor(1:12)
  ggplot(tquant, aes_string(x="month", y=par)) + 
    geom_pointrange(aes(ymin=llow, ymax=hhigh)) +
    geom_pointrange(aes(ymin=low, ymax=high), size=1) + theme_bw()
}

m <- stan_model("snowtrends.stan")
sdat <- list(T = nrow(snow), y = snow$y, month=as.integer(snow$month))
fit <- sampling(m, data = sdat, seed=5, refresh=10,
                   iter = 5000, chains = 1, init=0, thin=5, nondiag_mass=T)

fit
plot(fit)
traceplot(fit, "lp__", inc_warmup=F)
plot(fit, par="theta")
plot(fit, par="trend")
plot(fit, par="trend2")

# Not much autocorrelation in the residuals
snow$err <- get_posterior_mean(fit, pars="err")[,1]
acf(ts(snow$err)) 
hist(snow$err, n=100)

# Still some structure on the long-range means
snow$pred <- get_posterior_mean(fit, pars="expected")[,1]
ggplot(snow, aes(x=year+(month-1)/12, y=err)) + geom_line() + geom_smooth()

# Predictions from trends, with quantiles
mpred <- as.data.frame(t(apply(as.data.frame(fit, "expected"), 2, 
                              function (x) quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)))))
names(mpred) <- c("llow", "low", "pred", "high", "hhigh")

pdf(file="model-plots.pdf")
month.plot(fit, "baseline") + ylab("sqrt area") + ggtitle("Monthly sqrt area (standardized)")
month.plot(fit, "trend") + geom_hline(yintercept=0, color="red") + 
  ggtitle("Linear trends of sqrt area") + ylab("coef")
month.plot(fit, "trend2") + geom_hline(yintercept=0, color="red") + 
  ggtitle("Quadratic trends") + ylab("coef")
month.plot(fit, "sigma") + ggtitle("Sigma of t4(0, sigma) monthly residual")
month.plot(fit, "theta") + geom_hline(yintercept=0, color="red") + 
  ggtitle("Modelled residual autocorrelation coeffs") + xlab("lag, months")
ggplot(snow, aes(x=year, y=err)) + 
  geom_point()  + facet_wrap(~ month, scales="free_y") + geom_hline(yintercept=0, color="red") + 
  ggtitle("Residuals by month (note incomparable scales)")
ggplot(snow, aes(x=year, y=pred, color=as.factor(month))) + geom_line() +
  geom_ribbon(aes(ymin=mpred$llow, ymax=mpred$hhigh, linetype=NA, fill=as.factor(month)), alpha=0.2) + 
  ylab("Predicted sqrt area") + ggtitle("Posteriors of the underlying trends")
dev.off()
