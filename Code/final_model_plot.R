library(readr)
library(dplyr)
library(MASS)
library(leaps)
library(ggplot2)
library(gridExtra)

BodyFat2<- read.csv("BodyFat2.csv")
BodyFatsub<- BodyFat2 %>% filter(IDNO != 39)
BodyFatsub2<- subset(BodyFatsub, select = -c(DENSITY, WEIGHT, HEIGHT))
modelsub<- lm(BODYFAT~ ABDOMEN + WRIST + WEIGHT_KG, data=BodyFatsub2)
p4<-ggplot(BodyFatsub2,aes(x=IDNO,y=fitted(modelsub)))+
  geom_point(shape=19,col=4)+
  ylab("Predicted bodyfat")+
  xlab("Real Bodyfat")+
  stat_smooth(method = "glm",formula=y ~ poly(x, 1),
              method.args=list(family="gaussian"),col="gold")
p4