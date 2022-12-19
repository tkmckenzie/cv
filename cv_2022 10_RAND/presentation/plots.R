library(dplyr)
library(ggplot2)
library(gridExtra)

setwd("~/git/cv/cv_2022 10_RAND/presentation")
rm(list = ls())

width = 4
height = 4
scale = 1

##################################################
rm(list = setdiff(ls(), c("width", "height", "scale")))
# PCE
set.seed(1)

N.gen = 12
order = N.gen - 1
alpha = 4
beta = 10

X.gen = seq(0, 1, length.out = N.gen)
y.gen = runif(N.gen, 0, 1)
m = lm(y.gen ~ poly(X.gen, order, raw = TRUE))
coef = m$coefficients

X.seq = seq(0, 1, length.out = 1000)
X.seq.matrix = sapply(0:order, function(i) X.seq^i)
y.seq = X.seq.matrix %*% coef
plot.df.1 = data.frame(X = X.seq, y = y.seq)
p1 = ggplot(plot.df.1, aes(X, y)) +
  geom_line() +
  theme_bw()
p1.blank = ggplot(plot.df.1, aes(X, y)) +
  geom_line(alpha = 0) +
  theme_bw()

plot.df.2 = data.frame(X = X.seq, y = dbeta(X.seq, alpha, beta))
p2 = ggplot(plot.df.2, aes(X, y)) +
  geom_line() +
  geom_area(fill = "black", alpha = 0.25) +
  theme_bw()

X.sim = rbeta(5000, alpha, beta)
X.sim.matrix = sapply(0:order, function(i) X.sim^i)
y.sim = X.sim.matrix %*% coef
plot.df.3 = data.frame(X = X.sim, y = y.sim)
p3 = ggplot(plot.df.3, aes(y)) +
  geom_density(fill = "black", alpha = 0.25) +
  coord_flip() +
  theme_bw()

p.blank = ggplot() + geom_blank() + theme_minimal()

p.final = arrangeGrob(p1, p3, p2, p.blank, ncol = 2, widths = c(4, 1.5), heights = c(4, 1.5))
ggsave("images/pce_density.png", p.final, width = 1.5 * width, height = 1.5 * height, scale = scale)

p.final.blank = arrangeGrob(p1.blank, p3, p2, p.blank, ncol = 2, widths = c(4, 1.5), heights = c(4, 1.5))
ggsave("images/pce_density_blank.png", p.final.blank, width = 1.5 * width, height = 1.5 * height, scale = scale)

##################################################
# rm(list = ls())