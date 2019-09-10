library(extraDistr)
library(ggplot2)

setwd("~/git/cv/reproducibility_presentation/R")

rm(list = ls())

scale.norm = 1

obj = function(log.scale.cauchy){
  scale.cauchy = exp(log.scale.cauchy)
  
  integrand = function(x){
    return((dnorm(x, sd = scale.norm) - dlst(x, df = 1.5, sigma = scale.cauchy))^2)
  }
  integrate.result = integrate(integrand, -Inf, Inf)
  if (integrate.result$message != "OK") stop(paste0("Integration failed with message ", integrate.result$message))
  
  return(integrate.result$value)
}
optim.result = optim(0, obj)
if (optim.result$convergence != 0) stop(paste0("Optimization failed with code ", optim.result$convergence))

scale.cauchy = exp(optim.result$par)
# scale.cauchy = 0.8

# Plot resulting pdfs
x.seq = seq(-5 * scale.norm, 5 * scale.norm, length.out = 500)
plot.df = data.frame(x = rep(x.seq, times = 2),
                     Density = c(dnorm(x.seq, sd = scale.norm), dcauchy(x.seq, scale = scale.cauchy)),
                     Distribution = rep(c("Normal", "Cauchy"), each = length(x.seq)))
ggplot(plot.df, aes(x, Density)) +
  geom_line(aes(color = Distribution)) +
  theme_bw() +
  theme(legend.position = "top")
ggsave("../figures/normal-cauchy.pdf", width = 6, height = 4, scale = 1)
