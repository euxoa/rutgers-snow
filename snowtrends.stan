data {
  int<lower=1> T;  
  real y[T];
  int<lower=0> month[T];
}
parameters {
  real baseline[12]; real trend[12]; real trend2[12];
  real<lower=-1, upper=1> theta[12]; 
  real<lower=0> sigma[12];
}
transformed parameters {
  vector[T] expected;
  vector[T] err;
  real st;
  for (t in 1:T) {
    st <- (t-T/2.0)/120.0;
    expected[t] <- baseline[month[t]] + st*trend[month[t]] + st*st*trend2[month[t]];
    err[t] <- y[t] - expected[t];
    for (lag in 1:12) { if (t>lag) err[t] <- err[t] - theta[lag] * err[t-lag]; } 
  }
}
model {
  sigma ~ gamma(.1, 1);
  baseline ~ normal(0, 5);
  trend ~ normal(0, .1); trend2 ~ normal(0, .01);

  for(t in 1:T) err[t] ~ student_t(4, 0, sigma[month[t]]);
}
