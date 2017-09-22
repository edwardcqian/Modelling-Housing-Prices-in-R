
model.final = lm(SALES~IMP*NBHD-IMP-1, data = data.1)
model.simp = lm(SALES~IMP+NBHD, data = data.1)

AIC(model.final)
anova(model.final)
summary(model.final)

AIC(model.simp)
Sanova(model.simp)
summary(model.simp)

