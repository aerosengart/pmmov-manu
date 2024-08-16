functions {
  real generalized_gaussian_lpdf(vector x, vector mu, real sigma, real beta) {
    real log_numer = log(beta); 
    real log_denom = log(2) + log(sigma) + lgamma(inv(beta));
    vector[num_elements(x)] expon = -pow((fabs(x - mu) / sigma), beta);
    return sum(log_numer - log_denom + expon);
  }

  real generalized_gaussian_rng(real mu, real sigma, real beta) {
    real gamma = gamma_rng(1 + (1 / beta), (1 / pow(2, (beta / 2))));
    real delta = sigma * pow(gamma, (1 / beta)) / sqrt(2);
    real sample = uniform_rng(mu - delta, mu + delta);
    return sample;
  }
}

data {
  real<lower=1, upper=2> beta;
  int<lower=0> obs;                // number of observations
  int<lower=1> p;                  // number of coefficients
  matrix[obs, p] X;                // matrix of covariates
  vector[obs] y;                   // response

  int<lower=0> obs_rep;           // number of observations in stan data frame
  matrix[obs_rep, p] X_rep;
}

parameters {
  real<lower=0> sigma;             // error scale parameter
  real alpha;                      // intercept

  vector<lower=0>[p] sigma_coef;   // coefficients prior std dev
  vector[p] B_raw; 
}

transformed parameters {
  vector[p] B;
  B = B_raw .* sigma_coef;
}

model {
  // hyperpriors
  sigma ~ student_t(5.0, 0.0, 1.0); 
  sigma_coef ~ student_t(5.0, 0.0, 1.0);
  B_raw ~ normal(0.0, 1.0);
  
  // sampling distribution of observations
  vector[obs] mu; 
  mu = alpha + X * B;  
  target += generalized_gaussian_lpdf(y | mu, sigma, beta);
}

generated quantities {
  array[obs_rep] real y_rep;
  for (i in 1:obs_rep) {
    y_rep[i] = generalized_gaussian_rng(alpha + dot_product(X_rep[i,], B), sigma, beta);
  }
}
