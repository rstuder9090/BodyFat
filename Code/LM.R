# Load Packages
library(readr)
library(dplyr)
library(MASS)
library(leaps)
library(ggplot2)
library(gridExtra)

# Load Data and subset with variables we do not want in the model.
BodyFat2 <- read.csv("BodyFat2.csv")
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
tmp4 <- dplyr::select(BodyFat,BODYFAT,WEIGHT_KG,ABDOMEN,FOREARM,WRIST) %>%
        dplyr::mutate(fits=fitted(model4),
                resids=resid(model4),
                sresids=rstudent(model4))
compare.4 <- ggplot(data=tmp4,mapping=aes(x=fits,y=resids)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed",col="red",size=1)+
  labs(x = "Fitted value")+
  labs(y = "Residuals")+
  labs(title = 'Fitted vs. Residual',
       subtitle = 'R-square = 0.7259',
       caption = 'BODYFAT=-29.68-0.28*WEIGHT_KG+0.92*ABDOMEN+0.43*FOREARM-1.41*WRIST')+
  theme(plot.title = element_text(family="serif",
                                  face="bold",
                                  color=4,
                                  hjust = 0.5),
        plot.subtitle = element_text(family="serif",
                                     hjust = 0.5),
        plot.caption = element_text(family="serif",
                                    hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot")+
  ylim(-15, 10)
compare.4
tmp3 <- dplyr::select(BodyFat,BODYFAT,WEIGHT_KG,ABDOMEN,WRIST) %>%
        dplyr::mutate(fits=fitted(model3),
                resids=resid(model3),
                sresids=rstudent(model3))
compare.3 <- ggplot(data=tmp3,mapping=aes(x=fits,y=resids)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed",col="red",size=1)+
  labs(x = "Fitted value")+
  labs(y = "Residuals")+
  labs(title = 'Fitted vs. Residual',
       subtitle = 'R-square = 0.7198',
       caption = 'BODYFAT=-23.36-0.24*WEIGHT_KG+0.9*ABDOMEN-1.18*WRIST')+
  theme(plot.title = element_text(family="serif",
                                  face="bold",
                                  color=4,
                                  hjust = 0.5),
        plot.subtitle = element_text(family="serif",
                                     hjust = 0.5),
        plot.caption = element_text(family="serif",
                                    hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot")+
  ylim(-15, 10)
compare.3
grid.arrange(compare.4,compare.3,ncol=2)
# plot(predict(model3),resid(model3), ylim =c(-12,12), xlab="Predicted Body Fat Percentage", ylab="Standardized Residuals", main="Standardized Residual Plot")
# abline(0,0, col = "red") # Don't like the 39th observation
# plot(predict(model4),resid(model4), ylim =c(-10,10), xlab="Predicted Body Fat Percentage", ylab="Standardized Residuals", main="Standardized Residual Plot")
# abline(0,0, col = "red") # Model 4 seems to fit assumptions better

# QQ
tmp4 <- dplyr::select(BodyFat,BODYFAT,WEIGHT_KG,ABDOMEN,FOREARM,WRIST) %>%
  dplyr::mutate(y = rstandard(model4))
qq.4 <- ggplot(tmp4, aes(sample = y))+
  stat_qq(col = 1) +
  stat_qq_line(col = 2,linetype="dashed",size=1)+
  labs(x = "Theoretical Quantiles")+
  labs(y = "Standard Residuals")+
  labs(title = 'Q-Q Plot of Residuals',
       subtitle = 'R-square = 0.7259',
       caption = 'BODYFAT=-29.68-0.28*WEIGHT_KG+0.92*ABDOMEN+0.43*FOREARM-1.41*WRIST')+
  theme(plot.title = element_text(family="serif",
                                  face="bold",
                                  color=4,
                                  hjust = 0.5),
        plot.subtitle = element_text(family="serif",
                                     hjust = 0.5),
        plot.caption = element_text(family="serif",
                                    hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot")
qq.4
tmp3 <- dplyr::select(BodyFat,BODYFAT,WEIGHT_KG,ABDOMEN,WRIST) %>%
        dplyr::mutate(y = rstandard(model3))
qq.3 <- ggplot(tmp3, aes(sample = y))+
     stat_qq(col = 1) +
     stat_qq_line(col = 2,linetype="dashed",size=1)+
     labs(x = "Theoretical Quantiles")+
     labs(y = "Standard Residuals")+
     labs(title = 'Q-Q Plot of Residuals',
         subtitle = 'R-square = 0.7198',
         caption = 'BODYFAT=-23.36-0.24*WEIGHT_KG+0.9*ABDOMEN-1.18*WRIST')+
     theme(plot.title = element_text(family="serif",
                                    face="bold",
                                    color=4,
                                    hjust = 0.5),
          plot.subtitle = element_text(family="serif",
                                       hjust = 0.5),
          plot.caption = element_text(family="serif",
                                      hjust = 0.5),
          plot.title.position = "plot",
          plot.caption.position = "plot")
qq.3
grid.arrange(qq.4,qq.3,ncol=2)
# qqnorm(rstandard(model3), main="Q-Q Plot of Residuals")
# abline(0,1, col="red")
# qqnorm(rstandard(model4), main="Q-Q Plot of Residuals")
# abline(0,1, col="red")

#Cook's Distance
tmp4 <- dplyr::select(BodyFat,BODYFAT,WEIGHT_KG,ABDOMEN,FOREARM,WRIST,X) %>%
        dplyr::mutate(x=X,Cooks = cooks.distance(model4))
cook.4 <- ggplot(tmp4, aes(x = X, y = Cooks))+
  geom_point(aes(size = Cooks)) +
  labs(x = "ID number")+
  labs(y = "Cook's Distance")+
  labs(title = "Cook's Distance Plot",
       subtitle = 'R-square = 0.7198',
       caption = 'BODYFAT=-23.36-0.24*WEIGHT_KG+0.9*ABDOMEN-1.18*WRIST')+
  theme(plot.title = element_text(family="serif",
                                  face="bold",
                                  color=4,
                                  hjust = 0.5),
        plot.subtitle = element_text(family="serif",
                                     hjust = 0.5),
        plot.caption = element_text(family="serif",
                                    hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot")
cook.4
tmp3 <- dplyr::select(BodyFat,BODYFAT,WEIGHT_KG,ABDOMEN,WRIST) %>%
  dplyr::mutate(y = cooks.distance(model3))
cook.3 <- ggplot(tmp4, aes(x = X, y = Cooks))+
  geom_point(aes(size = Cooks)) +
  labs(x = "ID number")+
  labs(y = "Cook's Distance")+
  labs(title = "Cook's Distance Plot",
       subtitle = 'R-square = 0.7259',
       caption = 'BODYFAT=-29.68-0.28*WEIGHT_KG+0.92*ABDOMEN+0.43*FOREARM-1.41*WRIST')+
  theme(plot.title = element_text(family="serif",
                                  face="bold",
                                  color=4,
                                  hjust = 0.5),
        plot.subtitle = element_text(family="serif",
                                     hjust = 0.5),
        plot.caption = element_text(family="serif",
                                    hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot")
cook.3
grid.arrange(cook.4,cook.3,ncol=2)
# plot(cooks.distance(model3), main="Cook's Distance Plot", ylab="Cook's Distance", xlab="ID Number", col = ifelse(cooks.distance(model3) < 0.4,'black','red'))
# plot(cooks.distance(model4), main="Cook's Distance Plot", ylab="Cook's Distance", xlab="ID Number", col = ifelse(cooks.distance(model4) < 0.4,'black','red'))


#########
# Look for changes in R^2 when taking out extreme BMI individual
BodyFatsub<- BodyFat2 %>% filter(IDNO != 39)
BodyFatsub2<- subset(BodyFatsub, select = -c(IDNO, DENSITY, WEIGHT, HEIGHT))


# Using regsubsets()
slr2<- regsubsets(BODYFAT~., data=BodyFatsub2, nbest=1, nvmax=10) #full model
regsum2<-summary(slr2) #summary of regsubsets()
regsum2$rsq # not much change in R^2 between 3 variables and up.
regsum2 # with 3: BODYFAT ~ ABDOMEN + WRIST + WEIGHT_KG (R^2 = 0.7303582)

modelsub<- lm(BODYFAT~ ABDOMEN + WRIST + WEIGHT_KG, data=BodyFatsub2)
summary(modelsub)#Adjusted R^2 = 0.7271, better than model with outlier and 4 variables
# -22.89403 - (0.19315*WEIGHT) - (1.33607*WRIST) + (0.88570*AB)
# Rule of thumb: 0.9Ab - 0.19WeightKG - 1.4Wrist - 23

# Plots too see assumption difference with/without outlier
#Residuals vs Fitted
tmpsub <- dplyr::select(BodyFatsub2,BODYFAT,WEIGHT_KG,ABDOMEN,FOREARM,WRIST) %>%
          dplyr::mutate(fits=fitted(modelsub),
                resids=resid(modelsub),
                sresids=rstudent(modelsub))
compare.sub <- ggplot(data=tmpsub,mapping=aes(x=fits,y=resids)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed",col="red",size=1)+
  labs(x = "Fitted value")+
  labs(y = "Residuals")+
  labs(title = 'Fitted vs. Residual',
       subtitle = 'R-square = 0.7304',
       caption = 'BODYFAT=-22.89-0.19*WEIGHT_KG+0.89*ABDOMEN-1.34*FOREARM-1.41*WRIST')+
  theme(plot.title = element_text(family="serif",
                                  face="bold",
                                  color=4,
                                  hjust = 0.5),
        plot.subtitle = element_text(family="serif",
                                     hjust = 0.5),
        plot.caption = element_text(family="serif",
                                    hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot")+
  ylim(-15, 10)
compare.sub
grid.arrange(compare.4,compare.3,compare.sub,ncol=3)
# plot(predict(modelsub),resid(modelsub), ylim = c(-10,10),xlab="Predicted Body Fat Percentage", ylab="Standardized Residuals", main="Standardized Residual Plot")
# abline(0,0, col = "red")
# plot(predict(model4),resid(model4), ylim =c(-10,10), xlab="Predicted Body Fat Percentage", ylab="Standardized Residuals", main="Standardized Residual Plot")
# abline(0,0, col = "red")

# QQ
tmpsub <- dplyr::select(BodyFatsub2,BODYFAT,WEIGHT_KG,ABDOMEN,FOREARM,WRIST) %>%
  dplyr::mutate(y = rstandard(modelsub))
qq.sub <- ggplot(tmpsub, aes(sample = y))+
  stat_qq(col = 1) +
  stat_qq_line(col = 2,linetype="dashed",size=1)+
  labs(x = "Theoretical Quantiles")+
  labs(y = "Standard Residuals")+
  labs(title = 'Q-Q Plot of Residuals',
       subtitle = 'R-square = 0.7304',
       caption = 'BODYFAT=-22.89-0.19*WEIGHT_KG+0.89*ABDOMEN-1.34*FOREARM-1.41*WRIST')+
  theme(plot.title = element_text(family="serif",
                                  face="bold",
                                  color=4,
                                  hjust = 0.5),
        plot.subtitle = element_text(family="serif",
                                     hjust = 0.5),
        plot.caption = element_text(family="serif",
                                    hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot")
qq.sub
grid.arrange(qq.4,qq.3,qq.sub,ncol=3)
# qqnorm(rstandard(modelsub), main="Q-Q Plot of Residuals")
# abline(0,1, col="red")
# qqnorm(rstandard(model4), main="Q-Q Plot of Residuals")
# abline(0,1, col="red")

#Cook's Distance
tmpsub <- dplyr::select(BodyFatsub2,BODYFAT,WEIGHT_KG,ABDOMEN,FOREARM,WRIST,X) %>%
          dplyr::mutate(x=X, Cooks = cooks.distance(modelsub))
cook.sub <- ggplot(tmpsub, aes(x = X, y = Cooks))+
  geom_point(aes(size = Cooks)) +
  labs(x = "ID number")+
  labs(y = "Cook's Distance")+
  labs(title = "Cook's Distance Plot",
       subtitle = 'R-square = 0.7304',
       caption = 'BODYFAT=-22.89-0.19*WEIGHT_KG+0.89*ABDOMEN-1.34*FOREARM-1.41*WRIST')+
  theme(plot.title = element_text(family="serif",
                                  face="bold",
                                  color=4,
                                  hjust = 0.5),
        plot.subtitle = element_text(family="serif",
                                     hjust = 0.5),
        plot.caption = element_text(family="serif",
                                    hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot")
cook.sub
grid.arrange(cook.4,cook.3,cook.sub,ncol=3)
# plot(cooks.distance(modelsub), main="Cook's Distance Plot", ylab="Cook's Distance", xlab="ID Number", col = ifelse(cooks.distance(modelsub) < 0.4,'black','red'))
# plot(cooks.distance(model4), main="Cook's Distance Plot", ylab="Cook's Distance", xlab="ID Number", col = ifelse(cooks.distance(model4) < 0.4,'black','red'))
