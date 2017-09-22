model.final = lm(SALES~IMP*NBHD, data = data.1)
summary(model.final)

yhat.final <- predict(model.final) #fitted values
sigma.hat.final <- summary(model.final)$sigma 
res.final <- resid(model.final)
standard.res.final <- res.final/sigma.hat.final

hist(standard.res.final,breaks=20,freq=FALSE, xlim=c(-3,3), main = "Standardized Residuals Histogram", 
     xlab = "Standardized Residuals")
curve(dnorm, add = TRUE, col = "red") #PDF of N(0,1)

qqnorm(res.final ,pch = 16, cex = 1, main = "Residual Q-Q Plot") #QQ plot
abline(qqline(res.final ,col = "red"), lty = 2) 

plot(yhat.final, standard.res.final, main = " Standardized Residuals vs Fitted Value", 
     ylab = "Standardized Residuals", xlab = "Fitted Values")
plot(data.1$IMP, standard.res.final, main = " Standardized Residuals vs Appraised Improvement Value", 
     ylab = "Standardized Residuals", xlab = "Appraised Improvement Value")
plot(data.1$NBHD, standard.res.final, main = " Standard Residuals vs Appraised Land Value", 
     ylab = "Standard Residuals", xlab = "Appraised Land Value")


# leverage
leverage<-hatvalues(model.final)
D <- cooks.distance(model.final)
p <- length(coef(model.final))
n <- nobs(model.final)
hbar <- p/n
infl.ind <- which.max(D) # top influence point
lev.ind <- leverage > 2*hbar # leverage more than 2x the average
clrs <- rep("black", len = n)
clrs[lev.ind] <- "blue"
clrs[infl.ind] <- "red"
par(mfrow = c(1,1), mar = c(4,4,1,1))
cex <- .8
plot(leverage, D, main = "Cook's Influence Measure vs Leverage", xlab = "Leverage",
     ylab = "Cook's Influence Measure",pch = 21, bg = clrs, cex = cex, cex.axis = cex)
abline(v = 2*hbar, col = "grey60", lty = 2) # 2x average leverage
legend("topleft", legend = c("High Leverage", "High Influence"), pch = 21,
       pt.bg = c("blue", "red"), cex = cex, pt.cex = cex)



# not used
which.max(D)

# removed high influencial point
data.1 = data.1[-146,]

summary(model.final)



