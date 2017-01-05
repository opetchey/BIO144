# Teste die Aussage des Papers von Freedman 1983
set.seed(198761)
X <- data.frame(matrix(rnorm(100*50),ncol=50))
y <- data.frame(y=rnorm(100))

data <- cbind(y,X)

summary(lm(y~ ., data=data))

summary(lm(y~ X44 + X40 + X36 + X35 + X31 + X27 + X25 + X23 + X22 + X21 + X16 + X15 + X13 + X11 + X10 , data=data))


