library(digest)
library(rstan)

setwd("~/git/cv/RAND_presentation/R/sf")

rm(list = ls())

load("data.RData")

#MCMC parameters
burn.iter = 5000
sample.iter = 5000

stan.file = "linear_sf.stan"

#Priors
beta_const_prior_sd = 1
beta_prior_sd = 1
sigma_u_prior_shape = 1
sigma_u_prior_rate = 1
sigma_v_prior_shape = 1
sigma_v_prior_rate = 1

save(beta_const_prior_sd,
     beta_prior_sd,
     sigma_u_prior_shape,
     sigma_u_prior_rate,
     sigma_v_prior_shape,
     sigma_v_prior_rate,
     file = "priors_translog.RData")

#Stan data
stan.data = list(N = N,
                 k = 2,
                 y = c(log.y),
                 X = log.x[,-1,drop = FALSE],
                 beta_const_prior_sd = beta_const_prior_sd,
                 beta_prior_sd = beta_prior_sd,
                 sigma_u_prior_shape = sigma_u_prior_shape,
                 sigma_u_prior_rate = sigma_u_prior_rate,
                 sigma_v_prior_shape = sigma_v_prior_shape,
                 sigma_v_prior_rate = sigma_v_prior_rate)

#Create dso if not present or if changed
stan.dso.file = gsub(".stan$", ".dso", stan.file)
if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.file, data = stan.data,
                  chains = 1, warmup = 1, iter = 1)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
  model.code.onfile = gsub("\\n$", "", gsub("\\r", "", readChar(stan.file, 1e6)))
  
  dso.hash = digest(as.character(stan.dso@stanmodel@model_code))
  onfile.hash = digest(model.code.onfile)
  
  if (dso.hash != onfile.hash){
    stan.dso = stan(stan.file, data = stan.data,
                    chains = 1, warmup = 1, iter = 1)
    save(stan.dso, file = stan.dso.file)
  }
}

#Fit Stan model
stan.fit = stan(stan.file, data = stan.data,
                chains = 1, warmup = burn.iter, iter = burn.iter + sample.iter,
                refresh = floor((burn.iter + sample.iter) / 100))
traceplot(stan.fit)

#Save stan model
save(stan.fit, burn.iter, sample.iter, file = "translog_stanfit.RData")
