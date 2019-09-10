library(extraDistr)
library(ggplot2)

setwd("~/git/cv/reproducibility_presentation/R")

rm(list = ls())

# Plotting parameters
width = 6
height = 4
scale = 1

# Generate a "normal-looking" base-case
N.init = 25
alpha = -5
beta = 2
sigma = 0.25
df = 1

# set.seed(98765)
set.seed(321)

# Generate data
X = runif(N.init, 0, 10)
y = alpha + X * beta + rlst(N.init, df = df, sigma = sigma)

# Make some initial plots
plot.df = data.frame(X = X, y = y)
p = ggplot(plot.df, aes(X, y)) +
  geom_point() +
  theme_bw()
p
ggsave("../figures/lm-error-dist-standard-point.pdf", width = width, height = height, scale = scale)

p + geom_smooth(method = "lm", alpha = 0)
ggsave("../figures/lm-error-dist-standard-fit.pdf", width = width, height = height, scale = scale)

# Some diagnostics
m = lm(y ~ X)

residual.df = data.frame(residuals = m$residuals,
                         fitted.values = m$fitted.values)

ggplot(residual.df, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line(color = "blue") +
  theme_bw()
ggsave("../figures/lm-error-dist-standard-qq.pdf", width = width, height = height, scale = scale)

ggplot(residual.df, aes(fitted.values, residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme_bw()
ggsave("../figures/lm-error-dist-standard-fitted-values.pdf", width = width, height = height, scale = scale)

shapiro.test(m$residuals)

# asdf
# Add more observations and keep track of E[y|X = 5] and beta
newdata = data.frame(X = 5)
beta.estimate = m$coefficients[2]
prediction.estimate = predict(m, newdata)
N.seq = floor(seq(N.init, 5e4, length.out = 5e4))

N.seq.diff = diff(N.seq)
for (N in N.seq.diff){
  X.new = runif(N, 0, 10)
  y.new = alpha + X.new * beta + rlst(N, df = df, sigma = sigma)
  
  X = c(X, X.new)
  y = c(y, y.new)
  
  m = lm(y ~ X)
  beta.estimate = c(beta.estimate, m$coefficients[2])
  prediction.estimate = c(prediction.estimate, predict(m, newdata))
}

beta.df = data.frame(beta.estimate = beta.estimate, N = N.seq)
ggplot(beta.df, aes(N, beta.estimate)) + 
  geom_line() +
  ylab("Beta Estimate") +
  theme_bw()
ggsave("../figures/lm-error-dist-standard-beta.pdf", width = width, height = height, scale = scale)

prediction.df = data.frame(prediction.estimate = prediction.estimate, N = N.seq)
ggplot(prediction.df, aes(N, prediction.estimate)) +
  geom_line() +
  ylab("Prediction Estimate") +
  theme_bw()
ggsave("../figures/lm-error-dist-standard-prediction.pdf", width = width, height = height, scale = scale)
