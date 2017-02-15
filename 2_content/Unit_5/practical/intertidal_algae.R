rm(list=ls())

library(tidyverse)
library(ggfortify)

dd <- read_csv("~/Desktop/IntertidalAlgae.csv")

dd

table(dd$height, dd$herbivores)

ggplot(dd, aes(x=Area_cm2)) + geom_histogram()

dd <- mutate(dd, log10_Area = log10(Area_cm2))
ggplot(dd, aes(x=log10_Area)) + geom_histogram()

dd <- mutate(dd, sqrt_Area = sqrt(Area_cm2))
ggplot(dd, aes(x=sqrt_Area)) + geom_histogram()
## !!!

ggplot(dd, aes(x=sqrt_Area)) + geom_histogram() + facet_grid(height ~ herbivores)
## Still crap!!!

## Hopefully ANOVA is robust enough!!!
## Could use a randomisation test, to avoid assumptions.
## Or non-parameteric.

ggplot(dd, aes(x=height, y=sqrt_Area, col=herbivores)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.2))

## Degrees of freedom
## four means, so 64 -4


## Going back...
meanz <- dd %>% group_by(herbivores, height) %>%
  summarise(mean=mean(sqrt_Area),
            sd=sd(sqrt_Area))
meanz
ggplot(dd, aes(x=herbivores, y=sqrt_Area, col=height)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.2,
                                           dodge.width = 0.5)) +
  geom_point(data=meanz, mapping=aes(y=mean), size=10, shape=1,
             position = position_dodge(width=0.5))

## looks like there's an interaction

m1 <- lm(sqrt_Area ~ herbivores * height, dd)
autoplot(m1)
anova(m1)
confint(m1)
summary(m1)
## at mid heights, there is no effect of herbivores
## herbivores reduce algal cover only at low heights.
ggplot(dd, aes(x=height, y=sqrt_Area, col=herbivores)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.2,
                                           dodge.width = 0.5)) +
  geom_point(data=meanz, mapping=aes(y=mean), size=10, shape=1,
             position = position_dodge(width=0.5)) +
  theme_bw() +
  ylab(expression("Algal cover - square root cm"^"2")) +
  xlab("Height")

    

