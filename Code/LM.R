# Load Packages
library(readr)
library(dplyr)
library(MASS)
library(leaps)

# Load Data and subset with variables we do not want in the model.
BodyFat2 <- read_csv("BodyFat2.csv")
names(BodyFat2)
BodyFat<- subset(BodyFat2, select = -c(IDNO, DENSITY, WEIGHT, HEIGHT)) #take out ID, Density, and non-metric height and weight.
names(BodyFat)


# Using regsubsets
slr<- regsubsets(BODYFAT~., data=BodyFat, nbest=1, nvmax=10) #full model
regsum<-summary(slr) #summary of regsubsets
regsum$rsq # not much change in R^2 between 4 variables and up.
regsum # with 4: BODYFAT ~ ABDOMEN + FOREARM + WRIST + WEIGHT_KG (R^2 = 0.7303030)


#Comparing model with 3 variables and 4 variables
model4<- lm(BODYFAT ~ WEIGHT_KG + ABDOMEN + FOREARM + WRIST, data=BodyFat)
summary(model4) #Adjusted R^2 = 0.7259
model3<- lm(BODYFAT ~ WEIGHT_KG + ABDOMEN + WRIST, data=BodyFat)
summary(model3) #Adjusted R^2 = 0.7198

#Residuals vs Fitted
plot(predict(model3),resid(model3), ylim =c(-12,12), xlab="Predicted Body Fat Percentage", ylab="Standardized Residuals", main="Standardized Residual Plot")
abline(0,0, col = "red") # Don't like the 39th observation
plot(predict(model4),resid(model4), ylim =c(-10,10), xlab="Predicted Body Fat Percentage", ylab="Standardized Residuals", main="Standardized Residual Plot")
abline(0,0, col = "red") # Model 4 seems to fit assumptions better

# QQ
qqnorm(rstandard(model3), main="Q-Q Plot of Residuals")
abline(0,1, col="red")
qqnorm(rstandard(model4), main="Q-Q Plot of Residuals")
abline(0,1, col="red")

#Cook's Distance
plot(cooks.distance(model3), main="Cook's Distance Plot", ylab="Cook's Distance", xlab="ID Number", col = ifelse(cooks.distance(model3) < 0.4,'black','red'))
plot(cooks.distance(model4), main="Cook's Distance Plot", ylab="Cook's Distance", xlab="ID Number", col = ifelse(cooks.distance(model4) < 0.4,'black','red'))


#########
# Look for changes in R^2 when taking out extreme BMI individual
BodyFatsub<- BodyFat2 %>% filter(IDNO != 39)
BodyFatsub2<- subset(BodyFatsub, select = -c(IDNO, DENSITY, WEIGHT, HEIGHT))


# Using regsubsets()
slr2<- regsubsets(BODYFAT~., data=BodyFatsub2, nbest=1, nvmax=10) #full model
regsum2<-summary(slr2) #summary of regsubsets()
regsum2$rsq # not much change in R^2 between 3 variables and up.
regsum2 # with 3: BODYFAT ~ ABDOMEN + WRIST + WEIGHT_KG (R^2 = 0.7303582)

#### FINAL MODEL
modelsub<- lm(BODYFAT~ ABDOMEN + WRIST + WEIGHT_KG, data=BodyFatsub2)
summary(modelsub)#Adjusted R^2 = 0.7271, better than model with outlier and 4 variables
# -22.89403 - (0.19315*WEIGHT) - (1.33607*WRIST) + (0.88570*AB)
# Rule of thumb: 0.9Ab - 0.19WeightKG - 1.4Wrist - 23

# Plots too see assumption difference with/without outlier
#Residuals vs Fitted
plot(predict(modelsub),resid(modelsub), ylim = c(-10,10),xlab="Predicted Body Fat Percentage", ylab="Standardized Residuals", main="Standardized Residual Plot")
abline(0,0, col = "red")
plot(predict(model4),resid(model4), ylim =c(-10,10), xlab="Predicted Body Fat Percentage", ylab="Standardized Residuals", main="Standardized Residual Plot")
abline(0,0, col = "red")

# QQ
qqnorm(rstandard(modelsub), main="Q-Q Plot of Residuals")
abline(0,1, col="red")
qqnorm(rstandard(model4), main="Q-Q Plot of Residuals")
abline(0,1, col="red")

#Cook's Distance
plot(cooks.distance(modelsub), main="Cook's Distance Plot", ylab="Cook's Distance", xlab="ID Number", col = ifelse(cooks.distance(modelsub) < 0.4,'black','red'))
plot(cooks.distance(model4), main="Cook's Distance Plot", ylab="Cook's Distance", xlab="ID Number", col = ifelse(cooks.distance(model4) < 0.4,'black','red'))
