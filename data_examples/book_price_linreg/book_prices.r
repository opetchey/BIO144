d.book <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/buchpreis.dat",header=T)


r.book <- lm(preis~seiten,data=d.book)

summary(r.book)

plot(preis~seiten,d.book)
abline(r.book)

plot(r.book$fitted,r.book$residuals)
abline(h=0,lty=2)

qqnorm(r.book$resid)
identify(qqnorm(r.book$resid))

# confint fuer Koeffizienten von Hand:
coef <- summary(r.book)$coef
coef[2,1]+c(-1,1)*coef[2,2]*qt(0.975,df=nrow(d.book)-2)

# autmoatisch:
confint(r.book)

# Preisprognose fuer 600s Buch
coef[1,1]+600*coef[2,1]
predict(r.book,newdata=data.frame(seiten=600))


# Vertrauensintervall:
t.predict <- predict(r.book,se.fit=T,newdata=data.frame(seiten=340))
t.ywert <- t.predict$fit
t.sd <- t.predict$se.fit
t.degree <-t.predict$df
t.resscale <- t.predict$residual.scale
t.vert.yu <- t.ywert-qt(0.975,df=t.degree)*t.sd
t.vert.yo <- t.ywert+qt(0.975,df=t.degree)*t.sd

c(t.vert.yu,t.vert.yo)

# Vorhersageintervall:
t.sqrt <- sqrt(t.sd**2+t.resscale**2)
t.vorh.vo <- t.ywert-qt(0.975,df=t.degree)*t.sqrt
t.vorh.v1 <- t.ywert+qt(0.975,df=t.degree)*t.sqrt
c(t.vorh.vo,t.vorh.v1)

# automatisch mit R:
t.vert <- predict(r.book,se.fit=T,newdata=data.frame(seiten=340),
                  interval ="confidence")
t.vorh <- predict(r.book,se.fit=T,newdata=data.frame(seiten=340),
                    interval = "prediction")

# Graphik:
t.range <- range(d.book$seiten)
t.xwerte <- seq(t.range[1],t.range[2],by=1)
t.predict <- predict(r.book,se.fit=T,newdata=data.frame(seiten=t.xwerte))
t.ywert <- t.predict$fit
t.sd <- t.predict$se.fit
t.degree <-t.predict$df
t.resscale <- t.predict$residual.scale
t.vert.yu <- t.ywert-qt(0.975,df=t.degree)*t.sd
t.vert.yo <- t.ywert+qt(0.975,df=t.degree)*t.sd
t.sqrt <- sqrt(t.sd**2+t.resscale**2)
t.vorh.vo <- t.ywert-qt(0.975,df=t.degree)*t.sqrt
t.vorh.v1 <- t.ywert+qt(0.975,df=t.degree)*t.sqrt
plot(d.book$seiten,d.book$preis,main="Streudiagramm")
abline(r.book)
lines(x=t.xwerte,y=t.vert.yu,lty=2)
lines(x=t.xwerte,y=t.vert.yo,lty=2)
lines(x=t.xwerte,y=t.vorh.vo,lty=4)
lines(x=t.xwerte,y=t.vorh.v1,lty=4)
legend("bottomright", c("Vertrauensband", "Vorhersageband"),
       lty=c(2,4), cex=1)


# Eleganter mit R:
t.range <- range(d.book$seiten)
t.xwerte <- seq(t.range[1],t.range[2],by=1)
t.vert <- predict(r.book,se.fit=T,newdata=data.frame(seiten=t.xwerte),
                  interval ="confidence")$fit
t.vorh <- predict(r.book,se.fit=T,newdata=data.frame(seiten=t.xwerte),
                  interval ="prediction")$fit
plot(d.book$seiten,d.book$preis,main="Streudiagramm")
abline(r.book)
lines(x=t.xwerte,y=t.vert[,2],lty=2)
lines(x=t.xwerte,y=t.vert[,3],lty=2)
lines(x=t.xwerte,y=t.vorh[,2],lty=4)
lines(x=t.xwerte,y=t.vorh[,3],lty=4)
legend("bottomright", c("Vertrauensband", "Vorhersageband"),
       lty=c(2,4), cex=1)
