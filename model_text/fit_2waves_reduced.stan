// generated with brms 2.13.0
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
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1_y1[N_y1];  // grouping indicator per observation
  // group-level predictor values
  vector[N_y1] Z_1_y1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2_y1[N_y1];  // grouping indicator per observation
  int<lower=1> J_2_y2[N_y2];  // grouping indicator per observation
  // group-level predictor values
  vector[N_y1] Z_2_y1_1;
  vector[N_y2] Z_2_y2_2;
  int<lower=1> NC_2;  // number of group-level correlations
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  int<lower=1> J_3_y2[N_y2];  // grouping indicator per observation
  // group-level predictor values
  vector[N_y2] Z_3_y2_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc_y1 = K_y1 - 1;
  matrix[N_y1, Kc_y1] Xc_y1;  // centered version of X_y1 without an intercept
  vector[Kc_y1] means_X_y1;  // column means of X_y1 before centering
  int Kc_y2 = K_y2 - 1;
  matrix[N_y2, Kc_y2] Xc_y2;  // centered version of X_y2 without an intercept
  vector[Kc_y2] means_X_y2;  // column means of X_y2 before centering
  for (i in 2:K_y1) {
    means_X_y1[i - 1] = mean(X_y1[, i]);
    Xc_y1[, i - 1] = X_y1[, i] - means_X_y1[i - 1];
  }
  for (i in 2:K_y2) {
    means_X_y2[i - 1] = mean(X_y2[, i]);
    Xc_y2[, i - 1] = X_y2[, i] - means_X_y2[i - 1];
  }
}
parameters {
  vector[Kc_y1] b_y1;  // population-level effects
  real Intercept_y1;  // temporary intercept for centered predictors
  real<lower=0> sigma_y1;  // residual SD
  vector[Kc_y2] b_y2;  // population-level effects
  real Intercept_y2;  // temporary intercept for centered predictors
  real<lower=0> sigma_y2;  // residual SD
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  matrix[M_2, N_2] z_2;  // standardized group-level effects
  cholesky_factor_corr[M_2] L_2;  // cholesky factor of correlation matrix
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  vector[N_3] z_3[M_3];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_y1_1;  // actual group-level effects
  matrix[N_2, M_2] r_2;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_2] r_2_y1_1;
  vector[N_2] r_2_y2_2;
  vector[N_3] r_3_y2_1;  // actual group-level effects
  r_1_y1_1 = (sd_1[1] * (z_1[1]));
  // compute actual group-level effects
  r_2 = (diag_pre_multiply(sd_2, L_2) * z_2)';
  r_2_y1_1 = r_2[, 1];
  r_2_y2_2 = r_2[, 2];
  r_3_y2_1 = (sd_3[1] * (z_3[1]));
}
model {
  // initialize linear predictor term
  vector[N_y1] mu_y1 = Intercept_y1 + Xc_y1 * b_y1;
  // initialize linear predictor term
  vector[N_y2] mu_y2 = Intercept_y2 + Xc_y2 * b_y2;
  for (n in 1:N_y1) {
    // add more terms to the linear predictor
    mu_y1[n] += r_1_y1_1[J_1_y1[n]] * Z_1_y1_1[n] + r_2_y1_1[J_2_y1[n]] * Z_2_y1_1[n];
  }
  for (n in 1:N_y2) {
    // add more terms to the linear predictor
    mu_y2[n] += r_2_y2_2[J_2_y2[n]] * Z_2_y2_2[n] + r_3_y2_1[J_3_y2[n]] * Z_3_y2_1[n];
  }
  // priors including all constants
  target += normal_lpdf(b_y1[1] | 0,2.5);
  target += normal_lpdf(Intercept_y1 | 0, 1);
  target += weibull_lpdf(sigma_y1 | 2, 1);
  target += normal_lpdf(b_y2[1] | 0,2.5);
  target += normal_lpdf(Intercept_y2 | 0, 1);
  target += weibull_lpdf(sigma_y2 | 2, 1);
  target += cauchy_lpdf(sd_1 | 0,2.5)
    - 1 * cauchy_lccdf(0 | 0,2.5);
  target += std_normal_lpdf(z_1[1]);
  target += cauchy_lpdf(sd_2 | 0,2.5)
    - 2 * cauchy_lccdf(0 | 0,2.5);
  target += std_normal_lpdf(to_vector(z_2));
  target += lkj_corr_cholesky_lpdf(L_2 | 1);
  target += cauchy_lpdf(sd_3 | 0,2.5)
    - 1 * cauchy_lccdf(0 | 0,2.5);
  target += std_normal_lpdf(z_3[1]);
  // likelihood including all constants
  if (!prior_only) {
    target += normal_lpdf(Y_y1 | mu_y1, sigma_y1);
    target += normal_lpdf(Y_y2 | mu_y2, sigma_y2);
  }
}
generated quantities {
  // actual population-level intercept
  real b_y1_Intercept = Intercept_y1 - dot_product(means_X_y1, b_y1);
  // actual population-level intercept
  real b_y2_Intercept = Intercept_y2 - dot_product(means_X_y2, b_y2);
  // compute group-level correlations
  corr_matrix[M_2] Cor_2 = multiply_lower_tri_self_transpose(L_2);
  vector<lower=-1,upper=1>[NC_2] cor_2;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_2) {
    for (j in 1:(k - 1)) {
      cor_2[choose(k - 1, 2) + j] = Cor_2[j, k];
    }
  }
}
