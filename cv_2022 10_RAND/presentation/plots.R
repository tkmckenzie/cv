library(abind)
library(dplyr)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(mlegp)
library(rstan)

rstan_options(auto_write = TRUE)

setwd("~/git/cv/cv_2022 10_RAND/presentation")
rm(list = ls())

width = 4
height = 4
scale = 1

keep.vars = c("width", "height", "scale")

##################################################
# rm(list = setdiff(ls(), keep.vars))
# # PCE
# set.seed(1)
# 
# N.gen = 12
# order = N.gen - 1
# alpha = 4
# beta = 10
# 
# X.gen = seq(0, 1, length.out = N.gen)
# y.gen = runif(N.gen, 0, 1)
# m = lm(y.gen ~ poly(X.gen, order, raw = TRUE))
# coef = m$coefficients
# 
# X.seq = seq(0, 1, length.out = 1000)
# X.seq.matrix = sapply(0:order, function(i) X.seq^i)
# y.seq = X.seq.matrix %*% coef
# plot.df.1 = data.frame(X = X.seq, y = y.seq)
# p1 = ggplot(plot.df.1, aes(X, y)) +
#   geom_line() +
#   theme_bw()
# p1.blank = ggplot(plot.df.1, aes(X, y)) +
#   geom_line(alpha = 0) +
#   theme_bw()
# 
# plot.df.2 = data.frame(X = X.seq, y = dbeta(X.seq, alpha, beta))
# p2 = ggplot(plot.df.2, aes(X, y)) +
#   geom_line() +
#   geom_area(fill = "black", alpha = 0.25) +
#   theme_bw()
# 
# X.sim = rbeta(5000, alpha, beta)
# X.sim.matrix = sapply(0:order, function(i) X.sim^i)
# y.sim = X.sim.matrix %*% coef
# plot.df.3 = data.frame(X = X.sim, y = y.sim)
# p3 = ggplot(plot.df.3, aes(y)) +
#   geom_density(fill = "black", alpha = 0.25) +
#   coord_flip() +
#   theme_bw()
# 
# p.blank = ggplot() + geom_blank() + theme_minimal()
# 
# p.final = arrangeGrob(p1, p3, p2, p.blank, ncol = 2, widths = c(4, 1.5), heights = c(4, 1.5))
# ggsave("images/pce_density.png", p.final, width = 1.5 * width, height = 1.5 * height, scale = scale)
# 
# p.final.blank = arrangeGrob(p1.blank, p3, p2, p.blank, ncol = 2, widths = c(4, 1.5), heights = c(4, 1.5))
# ggsave("images/pce_density_blank.png", p.final.blank, width = 1.5 * width, height = 1.5 * height, scale = scale)

##################################################
# rm(list = setdiff(ls(), keep.vars))
# set.seed(123)
# 
# f = function(x) sin(x) * x
# lower = 0
# upper = 10
# 
# N = 7
# N.seq = 1000
# 
# X = runif(N, lower, upper)
# X.seq = seq(lower, upper, length.out = N.seq)
# y = sapply(X, f)
# y.true = sapply(X.seq, f)
# 
# gp.mle.fit = mlegp(X, y)
# y.fit = predict.gp(gp.mle.fit, matrix(X.seq, ncol = 1), se.fit = TRUE)
# 
# true.df = data.frame(X = X.seq, y = y.true, Variable = "True", y.lower = NA, y.upper = NA)
# fit.df = data.frame(X = X.seq, y = y.fit$fit, Variable = "Estimated")
# fit.df$y.lower = fit.df$y - qnorm(0.975, sd = y.fit$se.fit[,1])
# fit.df$y.upper = fit.df$y + qnorm(0.975, sd = y.fit$se.fit[,1])
# line.df = rbind(true.df, fit.df)
# point.df = data.frame(X = X, y = y)
# 
# ggplot(line.df, aes(X, y)) +
#   geom_line(aes(color = Variable)) +
#   geom_ribbon(data = line.df %>% filter(!is.na(y.lower)), aes(ymin = y.lower, ymax = y.upper), alpha = 0.25) +
#   geom_point(data = point.df) +
#   theme_bw() +
#   theme(legend.position = "top")
# ggsave("images/gp_nonoise.png", width = 1.25 * width, height = 1.5 * height, scale = scale)

##################################################
rm(list = setdiff(ls(), keep.vars))
set.seed(123)

f = function(x) sin(x) * x
lower = 0
upper = 10

N = 100
N.seq = 1000

sd = 2
eps = rnorm(N, sd = sd)

X = runif(N, lower, upper)
X.seq = seq(lower, upper, length.out = N.seq)
y = sapply(X, f) + eps
y.mean = sapply(X.seq, f)

# Fit with Stan
burn.iter = 1000
sample.iter = 1000

model.name = "gp"
stan.model.file = paste0(model.name, ".stan")
stan.dso.file = paste0(model.name, ".dso")

stan.data = list(N = N, X = X, y = y,
                 alpha_prior_scale = 10, rho_prior_scale = 10, sigma_prior_scale = 10)

if (!(stan.dso.file %in% list.files())){
  stan.dso = stan(stan.model.file, data = stan.data,
                  chains = 1, iter = 2, warmup = 1, refresh = 0)
  save(stan.dso, file = stan.dso.file)
} else{
  load(stan.dso.file)
}

stan.fit = stan(stan.model.file, data = stan.data,
                chains = 1, iter = burn.iter + sample.iter, warmup = burn.iter,
                pars = "K", include = FALSE,
                refresh = floor((burn.iter + sample.iter) / 100))
save(stan.fit, burn.iter, sample.iter, file = "gp_fit.RData")

show(traceplot(stan.fit))

stan.extract = extract(stan.fit)

#Prediction
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
  sigma = stan.extract$sigma[i]
  
  return(gp.pred(x.pred, y, X, alpha, rho, sigma))
}

# Sampling posterior
N.pred = 100
x.pred = seq(lower, upper, length.out = N.pred)

f.pred = lapply(1:sample.iter, gp.pred.i)
f.pred = abind(f.pred, along = 3)

f.pred.mean = apply(f.pred[,1,], 1, mean)

p = 0.975
f.pred.low = apply(abind(lapply(1:sample.iter, function(i) f.pred[,1,i] - qnorm(p, sd = sqrt(f.pred[,2,i]))), along = 2), 1, mean)
f.pred.high = apply(abind(lapply(1:sample.iter, function(i) f.pred[,1,i] + qnorm(p, sd = sqrt(f.pred[,2,i]))), along = 2), 1, mean)

# Plotting
point.df = data.frame(X = X, y = y)
true.df = data.frame(X = X.seq, y = y.mean, Variable = "True", y.lower = NA, y.upper = NA)
fit.df = data.frame(X = x.pred, y = f.pred.mean, Variable = "Estimated", y.lower = f.pred.low, y.upper = f.pred.high)
line.df = rbind(true.df, fit.df)

ggplot(line.df, aes(X, y)) +
  geom_line(aes(color = Variable)) +
  geom_ribbon(data = line.df %>% filter(!is.na(y.lower)), aes(ymin = y.lower, ymax = y.upper), alpha = 0.25) +
  geom_point(data = point.df) +
  theme_bw() +
  theme(legend.position = "top")
ggsave("images/gp_withnoise.png", width = width * 1.25, height = height * 0.8, scale = scale)

# Ridgeline plot
N.x = 25
N.y = 25

x.pred = seq(lower, upper, length.out = N.x)

f.pred = lapply(1:sample.iter, gp.pred.i)
f.pred = abind(f.pred, along = 3)

f.pred.mean = apply(f.pred[,1,], 1, mean)
f.pred.sd = apply(sqrt(f.pred[,2,]), 1, mean)

generate.ridge = function(i){
  y.seq = seq(f.pred.mean[i] - 3 * f.pred.sd[i], f.pred.mean[i] + 3 * f.pred.sd[i], length.out = N.y)
  density = sapply(y.seq, dnorm, mean = f.pred.mean[i], sd = f.pred.sd[i])
  
  df = data.frame(X = x.pred[i], y = y.seq, density = density)
  return(df)
}

df.ridge = Reduce(rbind, lapply(1:N.x, generate.ridge))
ggplot(df.ridge, aes(y, X)) +
  geom_ridgeline(aes(height = density, group = X), scale = 2.5) +
  # geom_path(aes(color = Variable), data = true.df) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "top")
ggsave("images/gp_ridgeline.png", width = width * 1.25, height = height * 0.8, scale = scale)
