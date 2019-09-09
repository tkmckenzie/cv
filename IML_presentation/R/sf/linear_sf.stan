functions{
	real normal_plus_halfnormal_lpdf(vector epsilon,
		real sigma,
		real lambda
	)
	{
		return num_elements(epsilon) * (log(2) - log(sigma)) + normal_lpdf(epsilon ./ sigma | 0, 1) + normal_lcdf(-epsilon * (lambda / sigma) | 0, 1);
	}
}
data{
	int N;
	int k;
	
	vector[N] y;
	matrix[N, k] X;
	
	real<lower=0> beta_const_prior_sd;
	real<lower=0> beta_prior_sd;
	
	real<lower=0> sigma_u_prior_shape;
	real<lower=0> sigma_u_prior_rate;
	
	real<lower=0> sigma_v_prior_shape;
	real<lower=0> sigma_v_prior_rate;
}
parameters{
	real beta_const;
	vector[k] beta;
	
	real<lower=0> sigma_u;
	real<lower=0> sigma_v;
}
model{
	beta_const ~ normal(0, beta_const_prior_sd);
	beta ~ normal(0, beta_prior_sd);
	sigma_u ~ inv_gamma(sigma_u_prior_shape, sigma_u_prior_rate);
	sigma_v ~ inv_gamma(sigma_v_prior_shape, sigma_v_prior_rate);
	
	target += normal_plus_halfnormal_lpdf(y - (beta_const + X * beta) | sqrt(sigma_u^2 + sigma_v^2), sigma_u / sigma_v);
}
