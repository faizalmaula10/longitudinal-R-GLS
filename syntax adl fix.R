library(gee)
data=Data_ADL_fix
data
library(lme4)       
library(lmerTest) 
numeric=data[,3:6]
numeric=as.matrix(numeric)
a=scale(numeric)
kategorik=data[,c(1,2,7)]
a=cbind(a,kategorik)
a
cor(numeric)

m2<-glm(a$Life_expectancy~a$Tax_Revenue+a$GDPPC+a$Health_Expenditure_per_Capita,family=gaussian,data=a)
summary(m2)
m2$weights
AIC(m2)
library(nlme)

mgls<-gls(Life_expectancy~Tax_Revenue+GDPPC+Health_Expenditure_per_Capita,data=a,correlation=corCompSymm(form=~1|Negara))
summary(mgls)
AIC(mgls)
residuals_mgls <- residuals(mgls)  # Extract residuals
mse_mgls <- mean(residuals_mgls^2)  # Calculate MSE
print(mse_mgls)
sst <- sum((data$Life_expectancy - mean(data$Life_expectancy))^2)
rss <- sum(residuals(mgls)^2)
r_squared <- 1 - (rss / sst)
print(r_squared)

mgls1<-gls(Life_expectancy~Tax_Revenue+GDPPC+Health_Expenditure_per_Capita,data=a,correlation=corAR1(form=~1|Negara))
summary(mgls1)
AIC(mgls1)
residuals_mgls1 <- residuals(mgls1)  # Extract residuals
mse_mgls1 <- mean(residuals_mgls1^2)  # Calculate MSE
print(mse_mgls1)
rss1 <- sum(residuals(mgls1)^2)
r_squared1 <- 1 - (rss1 / sst)
print(r_squared1)

mgls2<-gls(Life_expectancy~Tax_Revenue+GDPPC+Health_Expenditure_per_Capita,data=a,correlation=corGaus(form=~1|Negara))
summary(mgls2)
AIC(mgls2)
residuals_mgls2 <- residuals(mgls2)  # Extract residuals
mse_mgls2 <- mean(residuals_mgls2^2) # Calculate MSE
print(mse_mgls2)
rss2<- sum(residuals(mgls2)^2)
r_squared2 <- 1 - (rss2/ sst)
print(r_squared2)
cov_matrix <- getVarCov(mgls2)

# Round the covariance matrix to 2 decimal places
rounded_cov_matrix <- round(cov_matrix, 2)

# Print the rounded covariance matrix
print(rounded_cov_matrix)
summary_mgls2 <- summary(mgls2)

# Extract the coefficients and p-values
coef_pvalues <- summary_mgls2$tTable

# Print the coefficients and p-values
print(coef_pvalues)
