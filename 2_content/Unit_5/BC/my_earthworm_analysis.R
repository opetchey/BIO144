rm(list=ls())

library(tidyverse)
library(ggfortify)
library(forcats)

dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/earthworm.csv")

table(dd$Gattung)

ggplot(dd, aes(x=Gewicht)) +
  geom_histogram()
dd <- mutate(dd, log10_Gewicht=log10(Gewicht))
ggplot(dd, aes(x=log10_Gewicht)) +
  geom_histogram(bins=20)

ggplot(dd, aes(x=Magenumf)) +
  geom_histogram(bins=20)

ggplot(dd, aes(x=Magenumf, y=log10_Gewicht)) +
  geom_point()

## slope guess of 0.31

m1 <- lm(log10_Gewicht ~ Magenumf, dd)
autoplot(m1)
summary(m1)

ggplot(dd, aes(x=Magenumf, y=log10_Gewicht)) +
  geom_point() +
  xlab("Gut circumference (mm)") +
  ylab("Worm weight (g)") +
  geom_smooth(method="lm")
  


ggplot(dd, aes(x=Gattung, y=log10_Gewicht)) +
  geom_boxplot() +
  geom_point()

# L = 0.4
# N = -0.2
# Oc = -0.2
 
# N - L = -0.6
# Oc - L = -0.6

m2 <- lm(log10_Gewicht ~ Gattung, dd)
autoplot(m2)
summary(m2)
anova(m2)

## make a new variable with N as the reference
dd <- dd %>%
  mutate(Gattung_refN_ = fct_relevel(Gattung, "N", after = 0))
m2.N <- lm(log10_Gewicht ~ Gattung_refN_, dd)
summary(m2.N)


ggplot(dd, aes(x=Magenumf, y=log10_Gewicht, colour=Gattung)) +
  geom_point()


m3 <- lm(log10_Gewicht ~ Magenumf + Gattung, dd)
autoplot(m3)
summary(m3)

m4 <- lm(log10_Gewicht ~ Magenumf * Gattung, dd)
autoplot(m4)
summary(m4)

ggplot(dd, aes(x=Magenumf, y=log10_Gewicht, colour=Gattung)) +
  geom_point() +
  xlab("Gut circumference (mm)") +
  ylab("Worm weight (g)") +
  geom_smooth(method="lm")


