data {
  int<lower=1> T;  
  real y[T];
  int<lower=0> month[T];
}
parameters {
  real mum[12]; real trend[12]; real trend2[12];
  real theta; real phi;
  real<lower=0> sigma[12];
}
transformed parameters {
  vector[T] nu;
  vector[T] err;
  vector[T] mu;
  real st;
  st <- (1-T/2.0)/120.0;
  mu[1] <- mum[month[1]] + st*trend[month[1]] + st*st*trend2[month[1]];
  nu[1] <- (1+phi)*mu[1];
  err[1] <- y[1] - nu[1];
  for (t in 2:T) {
    st <- (t-T/2.0)/120.0;
    mu[t] <- mum[month[t]] + st*trend[month[t]] + st*st*trend2[month[t]];
    nu[t] <- mu[t] + phi * y[t-1] + theta * err[t-1];
    err[t] <- y[t] - nu[t];  
  }
}
model {
  mum ~ normal(0,10); trend ~ normal(0, 10); trend2 ~ normal(0, 10);
  theta ~ normal(0,2); phi ~ normal(0, 2);
  sigma ~ cauchy(0,5);

  for(t in 1:T) err[t] ~ cauchy(0, sigma[month[t]]);
}