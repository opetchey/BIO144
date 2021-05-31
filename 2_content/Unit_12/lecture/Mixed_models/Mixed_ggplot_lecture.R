rm(list=ls())

## install the mixed model package lme4 and pbkrtest
install.packages("lme4")
install.packages("broom")
install.packages("pbkrtest")
## (remember, you only have to do this once)

## load the packages we'll need
library(lme4)    # <- we just installed this
library(pbkrtest)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(broom)

###############################################################
## *** get the data and explore it ***
###############################################################

## load up the data file take a look

offMassData <- read_csv(file="~/Desktop/soay_mass_data.csv")


## Make a very simple plot and think about the questions
## we might like to ask of the data

ggplot(offMassData, aes(x = Ag, y = logM)) + 
	geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
	labs(x="Maternal age (years)", y ="Total mass (kg - log scale)")+
	geom_smooth(method = lm, se=FALSE)+
	theme_bw(base_size = 20)

## Plot the Data - using lattice library and xyplot

offMassData <- mutate(offMassData, Id=factor(Id))

p1 <- ggplot(offMassData, aes(x = Ag, y = logM, colour = factor(Id)))+
	geom_point() + geom_line() +
	geom_smooth(method = 'lm', se=FALSE, linetype = 'dotted') +
	facet_wrap(~Id)
p1

p2 <- ggplot(offMassData, aes(x = Ag, y = logM, colour = factor(Id))) +
	geom_point() +
	geom_smooth(method = 'lm', se=FALSE, size = 0.5)
p2

grid.arrange(p1, p2, ncol = 2)




# fit a separate model for each female using `lmList` from `lme4`
many_models <- lmList(logM ~ Ag | Id, data = offMassData)
all_coefs <- coef(many_models)
names(all_coefs) <- c("a", "b")
# plot the intercepts against the slopes
p3 <- ggplot(all_coefs, aes(x = a, y = b)) + 
  geom_point(colour = "steelblue") + 
  xlab("Intercept") + ylab("Slope")
p3

grid.arrange(p1, p2, p3, ncol = 3)

##
## Other ways to extract infomormation from multiple fits
##

many_models <- 
  offMassData %>% 
  group_by(Id) %>%
  do(fit = lm(logM ~ Ag, data = .))

# even more information using `tidy` function (from `broom`)
many_models %>% tidy(fit)
# ...but not all that useful for plotting

# a trick to grab just the intercept and slope
coef_data <- 
  many_models %>% 
  mutate(a = coef(fit)[[1]], b = coef(fit)[[2]]) %>% 
  select(-fit)

# make that intercept-slope graph again
ggplot(coef_data, aes(x = a, y = b)) + 
  geom_point(colour = "steelblue") + 
  xlab("Intercept") + ylab("Slope")

###############################################################
## *** Make a model ***
###############################################################

offMassMod1 <- lmer(logM ~ Ag + (1 + Ag | Id), data=offMassData)
summary(offMassMod1)

## sequencial ANOVA table - DO NOT USE THIS W/ MIXED MODELS
anova(offMassMod1)

## *** Checking the model ***

## Do it with ggplot

# creates a data frame for you with fitted, resids and standardised resids
diagnostics <- fortify(offMassMod1)  

# residuals vs. fitted
p1 <- ggplot(diagnostics, aes(x = .fitted, y = .scresid))+
  geom_point() +
  geom_smooth(method = "gam", se = FALSE)  +
  theme_grey(base_size = 10)
p1

# qqplot
p2<-ggplot(diagnostics, aes(sample = .scresid))+
  stat_qq()+
  geom_abline() + 
  theme_grey(base_size = 10)
p2

# residuals vs. fitted for each subject
p3<-ggplot(diagnostics, aes(x = Ag, y = .resid))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)  +
  facet_wrap(~Id) + theme_grey(base_size = 10)
p3

# why the third plot?
diag2 <- fortify(lmer(logM ~ Ag + (1 | Id), data = offMassData))
ggplot(diag2, aes(x = Ag, y = .resid))+
  geom_point() + geom_smooth(method = "lm", se = FALSE)  +
  facet_wrap(~Id) + theme_grey(base_size = 10)

## Or do it with the plotting facilities built into lmer

## 1. scaled residuals vs fitted
plot(offMassMod1, resid(., scaled=TRUE) ~ fitted(.), abline = 0)
## 2. box plots by age group
plot(offMassMod1, factor(Ag) ~ resid(., scaled=TRUE))
## 3. residuals against age for each individual
plot(offMassMod1, resid(., scaled=TRUE) ~ Ag | Id, abline = 0)


## We can look at the fixed and random effects using
## 'fixef' and 'ranef'...
fixef(offMassMod1)
ranef(offMassMod1)
## which give us the individual COEFicients
coef(offMassMod1)

###############################################################
## *** Inference (p-values, CIs) ***
###############################################################

## LR Tests

## Is the fixed effect of age significant?
offMassMod1 <- lmer(logM ~ 1 + Ag + (1 + Ag | Id), offMassData)
offMassMod2 <- lmer(logM ~ 1 +      (1 + Ag | Id), offMassData)
anova(offMassMod1, offMassMod2) # yes
## but remember, these LR tests are anticonservative in 
# 'small' datasets

## if you want convincing that anova is comapring likelihoods...
-2*logLik(offMassMod1, REML=FALSE) + 2*logLik(offMassMod2, REML=FALSE)

## Is the among-individual age-slope variation significant?
offMassMod1 <- lmer(logM ~ 1 + Ag + (1 + Ag | Id), offMassData)
offMassMod4 <- lmer(logM ~ 1 + Ag + (1      | Id), offMassData)
anova(offMassMod1, offMassMod4, refit = FALSE) # yes

## Is the correlation terms significant?
offMassMod1 <- lmer(logM ~ 1 + Ag + (1 + Ag |  Id), offMassData)
offMassMod3 <- lmer(logM ~ 1 + Ag + (1 + Ag || Id), offMassData)
anova(offMassMod1, offMassMod3, refit = FALSE) # yes

## N.B. be careful when testing variance terms as '0' is on the boundary
## of parameter space -> test is CONSERVATIVE so we're OK

## Parametric bootstrap via library(pbkrtest)

## Easy to do...
PBmodcomp(offMassMod1, offMassMod2, nsim = 100)

## Confidence intervals

## "Wald" - fast but approximate / only fixed effects
confint(offMassMod1, method="Wald")
## "profile" 
confint(offMassMod1, method="profile")
## "bootstrap"
confint(offMassMod1, method="boot")

# EXTRA STUFF

par(mfrow=c(2,2))
## Take a look at the random effects - Intercept....
hist(ranef(offMassMod1)$Id[,"(Intercept)"], xlim=c(-1,+1), ylim=c(0,6),
     main="", xlab="Individual intercepts")
abline(v=0, lty=2)
## ...or use a kernel density estimate - Slopes... 
plot(density(ranef(offMassMod1)$Id[,"Ag"]), xlim=c(-1,+1)/2,
      main="", xlab="Individual slopes")
rug(ranef(offMassMod1)$Id[,"Ag"])    
abline(v=0, lty=2)
## look at the correlation of random effects
rr.mix <- ranef(offMassMod1)
plot(rr.mix$Id[,"(Intercept)"], rr.mix$Id[,"Ag"],
     xlim=c(-1,1), ylim=c(-0.35,0.35),
     xlab="Individual intercept", ylab="individual age effect (= slope)")

## make a qqplot of intercept blups
all.ranef <- ranef(offMassMod1)
str(all.ranef)
rr.mix <- ranef(offMassMod1)$Id[,1]
qqnorm(scale(rr.mix))
qqline(scale(rr.mix))

