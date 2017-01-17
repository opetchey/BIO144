d.cricket <- read.table("~/Teaching/Bio144/data_examples/WBL/cricket.dat",header=T)

pairs(d.cricket)

r.mod1 <- lm(y~x1 + x2 + x4, data=d.cricket)
summary(r.mod1)

plot(r.mod1)
# Residuals v.s. variables
plot(d.cricket$x1,r.mod1$residuals)
abline(h=0,lty=2)

plot(d.cricket$x2,r.mod1$residuals)
abline(h=0,lty=2)

plot(d.cricket$x4,r.mod1$residuals)
abline(h=0,lty=2)

# Transformiere y
r.mod2 <- lm(I(y^2)~x1 + x2 + x4, data=d.cricket)
summary(r.mod2)
plot(r.mod2)

# Nehme quadrierte Eingangsvariablen
r.mod1.q <- lm( y ~ x1 + x2 + x4 + I(x1^2) + I(x2^2) + I(x4^2), data = d.cricket)

r.mod2.q <- lm( I(y^2) ~ x1 + x2 + x4 + I(x1^2) + I(x2^2) + I(x4^2), data = d.cricket)
