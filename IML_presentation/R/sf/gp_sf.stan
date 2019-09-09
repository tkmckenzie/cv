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
	
	real X[N];
	vector[N] y;
	
	real<lower=0> alpha_prior_sd;
	real<lower=0> rho_prior_shape;
	real<lower=0> rho_prior_rate;
	
	real<lower=0> sigma_u_prior_shape;
	real<lower=0> sigma_u_prior_rate;
	real<lower=0> sigma_v_prior_shape;
	real<lower=0> sigma_v_prior_rate;
}
parameters{
	real<lower=0> alpha;
	real<lower=0> rho;
	
	real<lower=0> sigma_u;
	real<lower=0> sigma_v;
	
	vector[N] eta;
}
transformed parameters{
	vector[N] f;
	
	{
		matrix[N, N] cov;
		matrix[N, N] L;
		
		cov = cov_exp_quad(X, alpha, rho);
		for (i in 1:N){
			cov[i, i] = cov[i, i] + 1e-12;
		}
		L = cholesky_decompose(cov);
		
		f = L * eta;
	}
}
model{
	alpha ~ normal(0, alpha_prior_sd);
	rho ~ inv_gamma(rho_prior_shape, rho_prior_rate);
	
	sigma_u ~ inv_gamma(sigma_u_prior_shape, sigma_u_prior_rate);
	sigma_v ~ inv_gamma(sigma_v_prior_shape, sigma_v_prior_rate);
	
	eta ~ normal(0, 1);
	
	target += normal_plus_halfnormal_lpdf(y - f | sqrt(sigma_u^2 + sigma_v^2), sigma_u / sigma_v);
}
