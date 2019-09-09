library(abind)
library(ggplot2)
library(rstan)

setwd("~/git/cv/RAND_presentation/R/sf")

rm(list = ls())

load("plot_params.RData")
load("data.RData")


#Plot data
point.df = data.frame(log.X = log.x[,2], y = log.y,
                     X = x[,2], Y = y)
ggplot(point.df, aes(X, Y)) +
  geom_point() +
  theme_bw()
ggsave("../../figures/data.pdf", width = width, height = height, scale = scale)

ggplot(point.df, aes(log.X, y)) +
  geom_point() +
  xlab("log(X)") +
  theme_bw()
ggsave("../../figures/data-log.pdf", width = width, height = height, scale = scale)


#Plot fitting results
x.seq = seq(min(log.x[,2]), max(log.x[,2]), length.out = 100)

#Log-linear
x.fit = x.seq

load("log_lin_stanfit.RData")
stan.extract = extract(stan.fit)

y.fit = apply(sapply(1:sample.iter, function(i) stan.extract$beta_const[i] + x.fit * stan.extract$beta[i,]), 1, mean)

log.lin.df = data.frame(log.X = x.fit, y = y.fit)
ggplot(point.df, aes(log.X, y)) +
  geom_point() +
  geom_line(data = log.lin.df) +
  theme_bw()

#Translog
x.fit = cbind(x.seq, x.seq^2)

load("translog_stanfit.RData")
stan.extract = extract(stan.fit)

y.fit = apply(sapply(1:sample.iter, function(i) stan.extract$beta_const[i] + x.fit %*% stan.extract$beta[i,]), 1, mean)

translog.df = data.frame(log.X = x.fit[,1], y = y.fit)
ggplot(point.df, aes(log.X, y)) +
  geom_point() +
  geom_line(data = translog.df) +
  theme_bw()

#GP
x.fit = x.seq

load("gp_stanfit.RData")
stan.extract = extract(stan.fit)

#Prediction functions
cov.exp.quad = function(x.1, x.2, alpha, rho){
  N.1 = length(x.1)
  N.2 = length(x.2)
  
  alpha.sq = alpha^2
  rho.sq = rho^2
  
  result = matrix(NA, nrow = N.1, ncol = N.2)
  
  for (i in 1:N.1){
    for (j in 1:N.2){
      result[i, j] = alpha.sq * exp(-(x.1[i] - x.2[j])^2 / (2 * rho.sq))
    }
  }
  return(result)
}

gp.pred = function(x.pred, y, x, alpha, rho, sigma){
  #This returns predicted f mean (col 1) and variance (col 2)
  N.pred = length(x.pred)
  N = length(y)
  
  Sigma = cov.exp.quad(x, x, alpha, rho) + sigma^2 * diag(N)
  # Sigma = cov.exp.quad(x, x, alpha, rho)
  
  L.Sigma = t(chol(Sigma))
  K.div.y = solve(t(L.Sigma), solve(L.Sigma, y))
  K.x.x.pred = cov.exp.quad(x, x.pred, alpha, rho)
  
  f.pred.mu = t(K.x.x.pred) %*% K.div.y
  
  v.pred = solve(L.Sigma, K.x.x.pred)
  cov.f.pred = cov.exp.quad(x.pred, x.pred, alpha, rho) - t(v.pred) %*% v.pred
  
  result = cbind(f.pred.mu, diag(cov.f.pred) + sigma^2) #Unconditional variance of y
  # result = cbind(f.pred.mu, sigma^2) #Variance of y|f
  
  return(result)
}

gp.pred.i = function(i){
  alpha = stan.extract$alpha[i]
  rho = stan.extract$rho[i]
  sigma = stan.extract$sigma_v[i]
  
  return(gp.pred(x.fit, log.y, log.x[,2], alpha, rho, sigma))
}

f.pred = lapply(1:sample.iter, gp.pred.i)
f.pred = abind(f.pred, along = 3)

f.pred.mean = apply(f.pred[,1,], 1, mean)

gp.df = data.frame(log.X = x.fit, y = f.pred.mean)
ggplot(point.df, aes(log.X, y)) +
  geom_point() +
  geom_line(data = gp.df) +
  theme_bw()

#Plot all the fits together
log.lin.df$Model = "Log-Linear"
translog.df$Model = "Translog"
gp.df$Model = "Non-Parametric"

fit.df = rbind(log.lin.df, translog.df, gp.df)

ggplot(point.df, aes(log.X, y)) +
  geom_point() +
  geom_line(data = fit.df, aes(group = Model, color = Model)) +
  xlab("log(X)") +
  theme_bw() +
  theme(legend.position = "top")
ggsave("../../figures/func-forms.pdf", width = width, height = height, scale = scale)
