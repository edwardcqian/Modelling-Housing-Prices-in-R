# Model.simp is the model with appraised land values removed and no interaction effects. 
# Simple model
model.simp = lm(SALES~IMP+NBHD, data = data.1)
summary(model.simp)

# Interaction model
model.full = lm(SALES~IMP*NBHD, data = data.1)
summary(model.full)

# Model selection setup
M0 = model.simp
Mfull = model.full

# Forward
Mfwd <- step(object = M0, scope = list(lower = M0, upper = Mfull),direction = "forward",trace = FALSE)
summary(Mfwd)
# Backward
Mback <- step(object = Mfull, scope = list(lower = M0, upper = Mfull),direction = "backward",trace = FALSE)
summary(Mback)
# Stepwise
Mstep <- step(object = M0, scope = list(lower = M0, upper = Mfull),direction = "both",trace = FALSE)
summary(Mstep)
