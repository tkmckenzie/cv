library(dplyr)

setwd("~/git/cv/cv_2022 10_RAND/presentation/nei")
rm(list = ls())

df = read.csv("data.csv")

total.gen = sum(df$Generation.2021)

df %>%
  group_by(Reactor.Type) %>%
  summarize(proportion = sum(Generation.2021) / total.gen)