
path <- "~/Teaching/Bio144/data_examples/bodyFat/"
d.bodyfat <- read.table(paste(path,"bodyfat.clean.txt",sep=""),header=T)

d.bodyfat <- d.bodyfat[,c("bodyfat","age","gewicht","hoehe","bmi","neck","abdomen","hip")]

d.bodyfat$waisthip <- d.bodyfat$abdomen / d.bodyfat$hip

plot(bodyfat ~ bmi,d.bodyfat)

r.simple <- lm(bodyfat ~ bmi, d.bodyfat)
summary(r.simple)
plot(r.simple)



pairs(d.bodyfat)

r.lm.all <- lm(bodyfat ~ age + gewicht + hoehe + bmi + neck + abdomen + hip + waisthip, d.bodyfat)
summary(r.lm.all)
AIC(r.lm.all)

AIC(update(r.lm.all,. ~ . -bmi))


d.lm <- lm(bodyfat ~  waisthip + abdomen,data=d.bodyfat)
summary(d.lm)
AIC(d.lm)
