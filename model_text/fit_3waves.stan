// generated with brms 2.12.0
functions {
}
data {
  int<lower=1> N;  // number of observations
  int<lower=1> N_y1;  // number of observations
  vector[N_y1] Y_y1;  // response variable
  int<lower=1> K_y1;  // number of population-level effects
  matrix[N_y1, K_y1] X_y1;  // population-level design matrix
  int<lower=1> N_y2;  // number of observations
  vector[N_y2] Y_y2;  // response variable
  int<lower=1> K_y2;  // number of population-level effects
  matrix[N_y2, K_y2] X_y2;  // population-level design matrix
  int<lower=1> nresp;  // number of responses
  int nrescor;  // number of residual correlations
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1_y1[N_y1];  // grouping indicator per observation
  int<lower=1> J_1_y2[N_y2];  // grouping indicator per observation
  // group-level predictor values
  vector[N_y1] Z_1_y1_1;
  vector[N_y1] Z_1_y1_2;
  vector[N_y2] Z_1_y2_3;
  vector[N_y2] Z_1_y2_4;
  int<lower=1> NC_1;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc_y1 = K_y1 - 1;
  matrix[N_y1, Kc_y1] Xc_y1;  // centered version of X_y1 without an intercept
  vector[Kc_y1] means_X_y1;  // column means of X_y1 before centering
  int Kc_y2 = K_y2 - 1;
  matrix[N_y2, Kc_y2] Xc_y2;  // centered version of X_y2 without an intercept
  vector[Kc_y2] means_X_y2;  // column means of X_y2 before centering
  vector[nresp] Y[N];  // response array
  for (i in 2:K_y1) {
    means_X_y1[i - 1] = mean(X_y1[, i]);
    Xc_y1[, i - 1] = X_y1[, i] - means_X_y1[i - 1];
  }
  for (i in 2:K_y2) {
    means_X_y2[i - 1] = mean(X_y2[, i]);
    Xc_y2[, i - 1] = X_y2[, i] - means_X_y2[i - 1];
  }
  for (n in 1:N) {
    Y[n] = [Y_y1[n], Y_y2[n]]';
  }
}
parameters {
  vector[Kc_y1] b_y1;  // population-level effects
  real Intercept_y1;  // temporary intercept for centered predictors
  real<lower=0> sigma_y1;  // residual SD
  vector[Kc_y2] b_y2;  // population-level effects
  real Intercept_y2;  // temporary intercept for centered predictors
  real<lower=0> sigma_y2;  // residual SD
  cholesky_factor_corr[nresp] Lrescor;  // parameters for multivariate linear models
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_y1_1;
  vector[N_1] r_1_y1_2;
  vector[N_1] r_1_y2_3;
  vector[N_1] r_1_y2_4;
  // compute actual group-level effects
  r_1 = (diag_pre_multiply(sd_1, L_1) * z_1)';
  r_1_y1_1 = r_1[, 1];
  r_1_y1_2 = r_1[, 2];
  r_1_y2_3 = r_1[, 3];
  r_1_y2_4 = r_1[, 4];
}
model {
  // initialize linear predictor term
  vector[N_y1] mu_y1 = Intercept_y1 + Xc_y1 * b_y1;
  // initialize linear predictor term
  vector[N_y2] mu_y2 = Intercept_y2 + Xc_y2 * b_y2;
  // multivariate predictor array
  vector[nresp] Mu[N];
  vector[nresp] sigma = [sigma_y1, sigma_y2]';
  // cholesky factor of residual covariance matrix
  matrix[nresp, nresp] LSigma = diag_pre_multiply(sigma, Lrescor);
  for (n in 1:N_y1) {
    // add more terms to the linear predictor
    mu_y1[n] += r_1_y1_1[J_1_y1[n]] * Z_1_y1_1[n] + r_1_y1_2[J_1_y1[n]] * Z_1_y1_2[n];
  }
  for (n in 1:N_y2) {
    // add more terms to the linear predictor
    mu_y2[n] += r_1_y2_3[J_1_y2[n]] * Z_1_y2_3[n] + r_1_y2_4[J_1_y2[n]] * Z_1_y2_4[n];
  }
  // combine univariate parameters
  for (n in 1:N) {
    Mu[n] = [mu_y1[n], mu_y2[n]]';
  }
  // priors including all constants
  target += normal_lpdf(b_y1[1] | 0,2.5);
  target += normal_lpdf(Intercept_y1 | 0, 1);
  target += weibull_lpdf(sigma_y1 | 2, 1);
  target += normal_lpdf(b_y2[1] | 0,2.5);
  target += normal_lpdf(Intercept_y2 | 0, 1);
  target += weibull_lpdf(sigma_y2 | 2, 1);
  target += lkj_corr_cholesky_lpdf(Lrescor | 1);
  target += cauchy_lpdf(sd_1 | 0,2.5)
    - 4 * cauchy_lccdf(0 | 0,2.5);
  target += normal_lpdf(to_vector(z_1) | 0, 1);
  target += lkj_corr_cholesky_lpdf(L_1 | 1);
  // likelihood including all constants
  if (!prior_only) {
    target += multi_normal_cholesky_lpdf(Y | Mu, LSigma);
  }
}
generated quantities {
  // actual population-level intercept
  real b_y1_Intercept = Intercept_y1 - dot_product(means_X_y1, b_y1);
  // actual population-level intercept
  real b_y2_Intercept = Intercept_y2 - dot_product(means_X_y2, b_y2);
  // residual correlations
  corr_matrix[nresp] Rescor = multiply_lower_tri_self_transpose(Lrescor);
  vector<lower=-1,upper=1>[nrescor] rescor;
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // extract upper diagonal of correlation matrix
  for (k in 1:nresp) {
    for (j in 1:(k - 1)) {
      rescor[choose(k - 1, 2) + j] = Rescor[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
}
