data {
  int<lower=1> T;  
  real y[T];
  int<lower=0> month[T];
}
parameters {
  real acc[12]; real trend[12]; 
  real<lower=0> sigma[12];
  real y0;
}
transformed parameters {
  vector[T] nu;
  vector[T] err;
  real st;
  nu[1] <- y0; err[1] <- y[1] - nu[1];
  for (t in 2:T) {
    st <- t/(T+0.0);
    nu[t] <- y[t-1]*exp(acc[month[t]] + st*trend[month[t]]);
    err[t] <- y[t] - nu[t]; 
  }
}
model {
  y0 ~ normal(0, 100);
  sigma ~ cauchy(0,5);
  acc ~ normal(0, 10); 
  trend ~ normal(0, 10); 
  for(t in 1:T) err[t] ~ student_t(4, 0, sigma[month[t]]);
}
generated quantities
{
  real year0[12]; real year1[12]; 
  real yprev0; real yprev1;
  for (t in 1:12) {
    if (t==1) {yprev0 <- 1; yprev1 <- 1;} 
        else  {yprev0 <- year0[t-1]; yprev1 <- year1[t-1]; }
    year0[t] <- yprev0 + acc[t] + 0.0*trend[t]; 
    year1[t] <- yprev1 + acc[t] + 1.0*trend[t];
  }  
}