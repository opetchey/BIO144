rm(list=ls())

library(tidyverse)
library(ggfortify)

dd <- read_csv("~/Desktop/EAGLES.CSV")

m1 <- glm(Count ~ Sex * Prey, data=dd, family=poisson)

anova(m1, test="Chisq")

ggplot(dd, aes(x=Sex, colour=Prey, y=Count)) +
  geom_col(position="dodge")

tt <- xtabs(Count ~ Prey + Sex, data = dd)
chisq.test(tt)
