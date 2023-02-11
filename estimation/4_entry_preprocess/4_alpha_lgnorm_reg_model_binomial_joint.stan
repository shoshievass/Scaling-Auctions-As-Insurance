data {
  int N_I; // number of project-bidder pairs
  int N_n; // number of unique projects (438)
  int N_PT; // number of project types
  int P; // number of project characteristics
  int project_type_seqid[N_I]; // project-type id  by project_bidder pair
  int project_bin_seqid[N_n]; // project-bin id by project id
  int project_seqid[N_I]; // project id
  matrix[N_I, P] X_n; // project-level characteristics matrix
  vector[N_I] alpha_n_i; // (c_o \cdot q_at) * (1 + c_tilde_i) for each bidder-auction pair
  int first_bidder_in_auction_ids[N_n]; // index of first bidder in each auction for stupidest possible indexing hack
  int num_potential_bidders[N_n]; // number of potential bidders in auction (fixed for each bin)
  int num_entered_bidders[N_n]; // number of bidders who entered in each auction
  vector[N_n] min_alpha_in_auction;// min alpha of bidders who entered in each auction
  vector[N_n] max_alpha_in_auction;// max alpha of bidders who entered in each auction
}

parameters {
  vector[P+1] beta; // coefficient on X_n_i;  (including one intercept)
  real<lower=0> alpha_lgsigma[N_n]; // sd of cost distribution by project type
  real alpha_bar_scaler[N_n];
}

transformed parameters {
  vector[N_n] alpha_lgmean;
  real<lower=0, upper=1> entry_prob[N_n];
  real alpha_bar[N_n];
  
  for(n in 1:N_n){
    alpha_lgmean[n] = beta[1] + X_n[ first_bidder_in_auction_ids[n] ] * beta[2:];
    alpha_bar[n] = max_alpha_in_auction[n]*(1 + alpha_bar_scaler[n]);
    entry_prob[n] = lognormal_cdf(alpha_bar[n], alpha_lgmean[n], alpha_lgsigma[n]);
  }
}

model {
  // priors
  to_vector(beta) ~ normal(0,1.0);
  to_vector(alpha_lgsigma) ~ inv_gamma(10,3);
  to_vector(alpha_bar_scaler) ~ normal(0.05, 0.1);

  // likelihood
  for (n_i in 1:N_I) { // N_I
    alpha_n_i[n_i] ~ lognormal(
      alpha_lgmean[project_seqid[n_i]],
      alpha_lgsigma[project_seqid[n_i]]
    );
  }


  for (n in 1:N_n){ // N_n
    num_entered_bidders[n] ~ binomial(num_potential_bidders[n], entry_prob[n]);
    target += (exp( -( alpha_lgmean[n] - log(alpha_bar[n]) )^2 / (2*alpha_lgsigma[n]^2)  ) / ( sqrt2() * pi() * alpha_lgsigma[n] * alpha_bar[n] ) );
  }
}
