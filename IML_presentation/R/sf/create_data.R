library(ggplot2)
library(truncnorm)

setwd("~/git/cv/RAND_presentation/R/sf")

rm(list = ls())

load("plot_params.RData")

N = 30
k = 3
sd = 0.25
sd.u = 0.1

log.x.range = c(1, 5)
beta.0 = runif(1, 0, 5)
beta.1 = runif(1, 0, 5)
# beta.2 = runif(1, -beta.1 / (2 * log.x.range[2]), 0)
beta.2 = -beta.1 / (2 * log.x.range[2])
beta = c(beta.0, beta.1, beta.2)[1:k]

log.x = runif(N, log.x.range[1], log.x.range[2])
log.x = cbind(1, log.x, log.x^2)[,1:k]

log.y = log.x %*% beta + rnorm(N, sd = sd) - rtruncnorm(N, a = 0, sd = sd.u)

x = exp(log.x)
y = exp(log.y)

plot(y ~ x[,2])
plot(log.y ~ log.x[,2])

plot.df = data.frame(log.X = log.x[,2], log.y,
                     X = x[,2], Y = y)
ggplot(plot.df, aes(X, Y)) +
  geom_point() +
  theme_bw()
# ggsave("../figures/data.pdf")

save(log.x, log.y, x, y, N, k,
     file = "data.RData")
