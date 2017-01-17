

path <- "~/Teaching/Bio144/data_examples/bodyFat/"
d.bodyfat <- read.table(paste(path,"bodyfat.clean.txt",sep=""),header=T)

d.bodyfat <- d.bodyfat[,c("bodyfat","age","gewicht","hoehe","bmi","neck","abdomen","hip")]

d.bodyfat$waisthip <- d.bodyfat$abdomen / d.bodyfat$hip

# Simple regression. Question: is BMI a good prognostic factor for bodyfat?
plot(bodyfat ~ bmi,d.bodyfat)

r.simple <- lm(bodyfat ~ bmi, d.bodyfat)
summary(r.simple)

# Tukey-Anscombe plot:
plot(fitted(r.simple),residuals(r.simple))
abline(h=0,lty=2)

# covariate v.s. residuals
plot(d.bodyfat$bmi,residuals(r.simple))
abline(h=0,lty=2)

qqnorm(fitted(r.simple))
qqline(fitted(r.simple))

# or simpler:
plot(r.simple)

# Fazit: Modell schaut ok aus.

# Was ist das Bestimmtheitsmass?
summary(r.simple)$r.squared
summary(r.simple)$adj.r.squared # Unterschied?

# Interpretation der Parameter:
coefs <- summary(r.simple)$coef

beta.int <- coefs[1,1]
beta.bmi <- coefs[2,1]

confint(r.simple)

# Was bedeutet Intercept? (bei BMI=0 haette man -27% Bodyfat...)
# 