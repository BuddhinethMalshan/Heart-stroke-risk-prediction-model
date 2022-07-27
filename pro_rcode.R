attach(X4_Stroke)
raw_data = X4_Stroke
raw_data
fit1 = lm(Risk~Age+Pressure+factor(Smoker,c("Yes","No")))
#fit1
#anova(fit1)

library(MASS)
forw = stepAIC(fit1,direction=c("forward"))
back = stepAIC(fit1,direction=c("backward"))
both = stepAIC(fit1,direction=c("both"))
fit = lm(Risk~Age+Pressure+factor(Smoker,c("Yes","No")))
fit

summary(fit)
anova(fit)

plot(fit)

std_residuals = scale(fit$residuals)
std_residuals = round(std_residuals,4)
std_residuals
fitted_values = fit$fitted.values
plot(fitted_values,std_residuals,main="std residuals Vs fitted values")
identify(fitted_values,std_residuals, plot=TRUE)

residuals = fit$residuals
qqnorm(residuals)
qqline(residuals, col="red")

library(car)
vif(fit)

cooks_dist_value = 4/(20-3)
cooks_dist_value
cooks_dis = cooks.distance(fit)
cooks_dis = round(cooks_dis,4)
cooks_dis

