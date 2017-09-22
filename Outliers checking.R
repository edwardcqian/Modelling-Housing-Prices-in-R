library(car)
data = read.csv("TamSales8.csv", header = T)
names(data)

# Initial test (basic model)
boxplot(data$SALES) # boxplot looks bad (right skewed data)

model = lm(SALES~LAND+IMP+NBHD, data=data)
outlierTest(model)
# Potential outliers 5, 25, 4
# Potential outliers were not suspicious

# New ln Data
data.1 = data
data.1$SALES = log(data.1$SALES)
data.1$LAND = log(data.1$LAND)
data.1$IMP = log(data.1$IMP)

# Matrix Scatterplot for LAND, IMP and NBHD
pairs(~LAND + IMP + as.numeric(NBHD), data = data.1
      main = "Scatterplot Matrix of Explanatory Variables")

# Outlier test for ln model
boxplot(data.1$SALES, main = "Sales Boxplot", ylab = "ln(Sales)") # 3 outliers (top 3 sales)

model.1 = lm(SALES~LAND+IMP+NBHD, data=data.1)

outlierTest(model.1)
# Potential outliers 252, 107, 234, 115
# Potential outliers were not suspicious

summary(model)
summary(model.1)

pairs(~LAND*IMP + as.factor(NBHD), data=data)
