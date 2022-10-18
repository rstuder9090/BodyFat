rm(list=ls())
# set the working directory as "c/bodyfa-main"
# library(corrplot)
library(dplyr)
library(ggfortify)

###############################################################################
# read the data
data <- read.csv(file = "BodyFat2.csv",header=T)
head(data)
# corrplot(cor(data), type = "upper", order = "hclust",
#          tl.col = "black", tl.srt = 45)
# exclude: BODYFAT(resonse variable), IDNO,X(useless factor), DENSITY,WEIGHT,HEIGHT(verified collinearity)
sample <- data[,-which(colnames(data) %in% c("BODYFAT","IDNO","DENSITY","WEIGHT","HEIGHT","X"))]

# calculate the pca, using all the sample
# with all the samples normalized
pca <- prcomp(sample,
              center = TRUE,
              scale. = TRUE)
pca.plot <- autoplot(pca,
                     data = data,
                     colour = 'BODYFAT')
pca.plot
pca.cor <-princomp(sample,cor = TRUE)
# biplot(pca.cor)
# qqplot(data$BODYFAT, pca.cor$scores[, 1])
# abline(data$BODYFAT~ pca.cor$scores[, 1])

explained.prob <- pca$sdev^2/sum(pca$sdev^2)
for( i in c(1:length(explained.prob))){
  explained.prop[i] <- sum(explained.prob[1:i])
}
# ith value means the proportion of things explained with i principle components concluded
explained.prop
# With 3 main components, more than 80% of data could be explained
jpeg(file="./Images/plotofPCA.jpeg")
plot(1:14, pca$sdev^2, type="b", xlab="Principle Components", 
     ylab="Variance of Principle Components",main="PCA of Covariance Matrix")
dev.off()

PCA.plot <- data.frame(x=c(1:14),
                       Variance = pca$sdev^2,
                       Proportion = explained.prop)
df1 <- data.frame(a=c(1,1,3,3), b=c(0.3,0.2,0.2,0.3))
p1 <- ggplot(PCA.plot, aes(x)) +  
  geom_line(aes(y = Variance), color = "blue",linetype = 2,size = 1) +
  # geom_line(aes(y=Proportion), color = "red",linetype = 2,size = 1)+
  geom_point(aes(y=Variance),colour = "red", size = 3)+
  geom_text(data = PCA.plot, aes(x,Variance, label = round(Proportion,2)),
            position = "identity",hjust = -0.5, vjust = 2,angle = 45,size = 5)+
  ggtitle('Variance and Explained Proportion: Different Numbers of 
          Princple Component\nFrom 1-14')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Number of Principle Components")+
  labs(y = "Variance for Each Principle Components")+
  geom_line(data=df1,aes(a,b),cex=1)+
  annotate('text',x=2,y=0,label="81% Explained",size=4,color='red')+
  theme_classic()
################################################################################

# verification for choosing 3 components: using AIC
df <- data.frame(matrix(ncol = 2, nrow = 4))
colnames(df) <-c("R-square","AIC")
for (i in c(1:14)){
  pca.score <- pca.cor$scores[,1:i]
  pca.score.m <- lm(data$BODYFAT~., data = data.frame(pca.score))
  r2 <- summary(pca.score.m)$r.squared
  aic <- AIC(pca.score.m)
  df[i,]<-c(r2,aic)
}
df
df$x <- c(1:14)
p2 <- ggplot(df, aes(x))+
  geom_line(aes(y=AIC), color = "blue",linetype = 2,size = 1)+
  geom_point(aes(y=AIC),color = "red", size = 3)+
  # geom_text(aes(x,AIC, label = round(AIC,0)),position = "identity",hjust = 0.7, vjust = -0.5,angle = 315,size = 5)+
  ggtitle('AIC: Different Numbers of Princple Component\nFrom 1-14')+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Number of Principle Components")+
  labs(y = "AIC for Linear Model")+
  theme_classic()+
  geom_vline(xintercept=3,color = "purple")+
  annotate('text',x=7,y=1530,label="No prominent optimization",size=6,color="purple")
p2

###############################################################################

# Final Linear Model
pca.score <- pca.cor$scores[,1:3]
pca.score <- data.frame(pca.score)
colnames(pca.score) <- c("PC1","PC2","PC3")
pca.score.m <- lm(data$BODYFAT~., data = pca.score)
summary(pca.score.m)

###############################################################################
# new data transformation
tfm.matrix <- pca.cor$loadings[,1:3]
# put the formar 5 sample
newdata <- data[1:5,-which(colnames(data) %in% c("BODYFAT","IDNO","DENSITY","WEIGHT","HEIGHT","X"))]
scaled <- scale(newdata,center = T,scale = T)
data.tfm <- scaled %*% tfm.matrix
newdata <- data.frame(PC1=scaled[,1],PC2=scaled[,2],PC3=scaled[,3])
# predict the 95% confidence interval for each sample
predict(pca.score.m,type="response",newdata, interval="predict")

###############################################################################
# overview of model performance
p4<-ggplot(data,aes(x=IDNO,y=fitted(pca.score.m)))+
  geom_point(shape=19)+xlab("")+
  ylab("Predicted bodyfat")+
  xlab("ID number")+
  stat_smooth(method = "glm",formula=y ~ poly(x, 1),
              method.args=list(family="gaussian"),col="gold")
p4
  # geom_vline(aes(xintercept=intercept2),colour="red",linetype="dashed")
