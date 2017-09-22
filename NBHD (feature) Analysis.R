# 1 fair (outlier)
data.1 = data[data$NBHD == 1, , drop=FALSE]
model.1 = lm(SALES~LAND+IMP-1, data=data.1)
summary(model.1)

yhat.1 <- predict(model.1) #fitted values
sigma.hat.1 <- summary(model.1)$sigma 
res.1 <- resid(model.1) #original residuals
standard.res.1 <- res.1/sigma.hat.1 #standardized residuals

par(mfrow = c(1,2))
plot(data.1$LAND,data.1$SALES, main="LAND")
plot(data.1$IMP, data.1$SALES, main="IMP")
hist(standard.res.1,breaks=20,freq=FALSE, xlim=c(-3,3), xlab = "Standardized Residuals", main="Residual plot")
curve(dnorm, add = TRUE, col = "red") #PDF of N(0,1)
qqnorm(res.1,pch = 16, cex = 1) #QQ plot
abline(qqline(res.1),col = "red", lty = 2) 
dev.off()
plot(yhat.1, res.1)

# 2 good (outlier)
data.2 = data[data$NBHD == 2, , drop=FALSE]
model.2 = lm(SALES~LAND+IMP-1, data=data.2)
summary(model.2)

yhat.2 <- predict(model.2) #fitted values
sigma.hat.2 <- summary(model.2)$sigma 
res.2 <- resid(model.2) #original residuals
standard.res.2 <- res.2/sigma.hat.2 #standardized residuals

par(mfrow = c(1,2))
plot(data.2$LAND,data.2$SALES, main="LAND")
plot(data.2$IMP, data.2$SALES, main="IMP")
hist(standard.res.2,breaks=20,freq=FALSE, xlim=c(-3,3), xlab = "Standardized Residuals", main="Residual plot")
curve(dnorm, add = TRUE, col = "red") #PDF of N(0,1)
qqnorm(res.2,pch = 16, cex = 1) #QQ plot
abline(qqline(res.2),col = "red", lty = 2) 
dev.off()
plot(yhat.2, res.2)

# 3 poor (many influencial cases)
data.3 = data[data$NBHD == 3, , drop=FALSE]
model.3 = lm(SALES~LAND+IMP-1, data=data.3)
summary(model.3)

yhat.3 <- predict(model.3) #fitted values
sigma.hat.3 <- summary(model.3)$sigma 
res.3 <- resid(model.3) #original residuals
standard.res.3 <- res.3/sigma.hat.3 #standardized residuals

par(mfrow = c(1,2))
plot(data.3$LAND,data.3$SALES, main="LAND")
plot(data.3$IMP, data.3$SALES, main="IMP")
hist(standard.res.3,breaks=20,freq=FALSE, xlim=c(-3,3), xlab = "Standardized Residuals", main="Residual plot")
curve(dnorm, add = TRUE, col = "red") #PDF of N(0,1)
qqnorm(res.3,pch = 16, cex = 1) #QQ plot
abline(qqline(res.3),col = "red", lty = 2) 
dev.off()
plot(yhat.3, res.3)

# 4 fair (outlier)
data.4 = data[data$NBHD == 4, , drop=FALSE]
model.4 = lm(SALES~LAND+IMP-1, data=data.4)
summary(model.4)

yhat.4 <- predict(model.4) #fitted values
sigma.hat.4 <- summary(model.4)$sigma 
res.4 <- resid(model.4) #original residuals
standard.res.4 <- res.4/sigma.hat.4 #standardized residuals

par(mfrow = c(1,2))
plot(data.4$LAND,data.4$SALES, main="LAND")
plot(data.4$IMP, data.4$SALES, main="IMP")
hist(standard.res.4,breaks=20,freq=FALSE, xlim=c(-3,3), xlab = "Standardized Residuals", main="Residual plot")
curve(dnorm, add = TRUE, col = "red") #PDF of N(0,1)
qqnorm(res.4,pch = 16, cex = 1) #QQ plot
abline(qqline(res.4),col = "red", lty = 2) 
dev.off()
plot(yhat.4, res.4)

# 5 poor (res not random)
data.5 = data[data$NBHD == 5, , drop=FALSE]
model.5 = lm(SALES~LAND+IMP-1, data=data.5)
summary(model.5)

yhat.5 <- predict(model.5) #fitted values
sigma.hat.5 <- summary(model.5)$sigma 
res.5 <- resid(model.5) #original residuals
standard.res.5 <- res.5/sigma.hat.5 #standardized residuals

par(mfrow = c(1,2))
plot(data.5$LAND,data.5$SALES, main="LAND")
plot(data.5$IMP, data.5$SALES, main="IMP")
ahist(standard.res.5,breaks=20,freq=FALSE, xlim=c(-3,3), xlab = "Standardized Residuals", main="Residual plot")
curve(dnorm, add = TRUE, col = "red") #PDF of N(0,1)
qqnorm(res.5,pch = 16, cex = 1) #QQ plot
abline(qqline(res.5),col = "red", lty = 2) 
dev.off()
plot(yhat.5, res.5)

# 6 good (outlier)
data.6 = data[data$NBHD == 6, , drop=FALSE]
model.6 = lm(SALES~LAND+IMP-1, data=data.6)
summary(model.6)

yhat.6 <- predict(model.6) #fitted values
sigma.hat.6 <- summary(model.6)$sigma 
res.6 <- resid(model.6) #original residuals
standard.res.6 <- res.6/sigma.hat.6 #standardized residuals

par(mfrow = c(1,2))
plot(data.6$LAND,data.6$SALES, main="LAND")
plot(data.6$IMP, data.6$SALES, main="IMP")
hist(standard.res.6,breaks=20,freq=FALSE, xlim=c(-3,3), xlab = "Standardized Residuals", main="Residual plot")
curve(dnorm, add = TRUE, col = "red") #PDF of N(0,1)
qqnorm(res.6,pch = 16, cex = 1) #QQ plot
abline(qqline(res.6),col = "red", lty = 2) 
dev.off()
plot(yhat.6, res.6)

# 7 poor (Heteroscedasticity)
data.7 = data[data$NBHD == 7, , drop=FALSE]
model.7 = lm(SALES~LAND+IMP-1, data=data.7)
summary(model.7)

yhat.7 <- predict(model.7) #fitted values
sigma.hat.7 <- summary(model.7)$sigma 
res.7 <- resid(model.7) #original residuals
standard.res.7 <- res.7/sigma.hat.7 #standardized residuals

par(mfrow = c(1,2))
plot(data.7$LAND,data.7$SALES, main="LAND")
plot(data.7$IMP, data.7$SALES, main="IMP")
hist(standard.res.7,breaks=20,freq=FALSE, xlim=c(-3,3), xlab = "Standardized Residuals", main="Residual plot")
curve(dnorm, add = TRUE, col = "red") #PDF of N(0,1)
qqnorm(res.7,pch = 16, cex = 1) #QQ plot
abline(qqline(res.7),col = "red", lty = 2) 
dev.off()
plot(yhat.7, res.7)

# 8 poor (non-linear)
data.8 = data[data$NBHD == 8, , drop=FALSE]
model.8 = lm(SALES~LAND+IMP-1, data=data.8)
summary(model.8)

yhat.8 <- predict(model.8) #fitted values
sigma.hat.8 <- summary(model.8)$sigma 
res.8 <- resid(model.8) #original residuals
standard.res.8 <- res.8/sigma.hat.8 #standardized residuals

par(mfrow = c(1,2))
plot(data.8$LAND,data.8$SALES, main="LAND")
plot(data.8$IMP, data.8$SALES, main="IMP")
hist(standard.res.8,breaks=20,freq=FALSE, xlim=c(-3,3), xlab = "Standardized Residuals", main="Residual plot")
curve(dnorm, add = TRUE, col = "red") #PDF of N(0,1)
qqnorm(res.8,pch = 16, cex = 1) #QQ plot
abline(qqline(res.8),col = "red", lty = 2) 
dev.off()
plot(yhat.8, res.8)
c(
mean(data.1$SALES),
mean(data.2$SALES),
mean(data.3$SALES),
mean(data.4$SALES),
mean(data.5$SALES),
mean(data.6$SALES),
mean(data.7$SALES),
mean(data.8$SALES))

plot(yhat.1, data.1$SALES)
plot(yhat.2, data.2$SALES)
plot(yhat.3, data.3$SALES)
plot(yhat.4, data.4$SALES)
plot(yhat.5, data.5$SALES)
plot(yhat.6, data.6$SALES)
plot(yhat.7, data.7$SALES)
plot(yhat.8, data.8$SALES)

predict(model.4, data.frame(IMP=1129.65, LAND=1004.59),interval="confidence",level=0.98)
predict(model.1, data.frame(IMP=961.74, LAND=297),interval="confidence",level=0.98)
predict(model.6, data.frame(IMP=1221.2, LAND=533.28),interval="confidence",level=0.98)
