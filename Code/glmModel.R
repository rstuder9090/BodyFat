data <- read.csv(file = "./Data/BodyFat2.csv",header=T)
head(data)

# Gaussian link, same as linear regression

glm_start <- glm(BODYFAT ~ AGE+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP
                 +THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+WIGHT_KG+HEIGHT_CM,
                 family= gaussian(link = "identity"), data = data)
backward <- step(glm_start)
summary(backward)

glm_no <- glm(BODYFAT ~ 1, family= gaussian(link = "identity"), data = data)
bothway <- step(glm_no, list(lower=formula(glm_no), upper = formula(glm_all)
                             ),  direction = "both", trace = 0)

summary(bothway)

jpeg(file="./Images/plotwihoutinteraction.jpeg")
par(mfrow=c(2,2))
plot(bothway)
dev.off()

glm_all <- glm(BODYFAT ~ AGE*WEIGHT*HEIGHT*ADIPOSITY*NECK+CHEST*ABDOMEN*HIP
                 *THIGH*KNEE*ANKLE*BICEPS*FOREARM*WRIST,
               family= gaussian(link = "identity"), data = data)
bothway_all <- step(glm_no, list(lower=formula(glm_no), upper = formula(glm_all)
                             ),  direction = "both", trace = 0)
summary(bothway_all)

jpeg(file="./Images/plotwihinteraction.jpeg")
par(mfrow=c(2,2))
plot(bothway_all)
dev.off()

# possion regression

glm_start <- glm(BODYFAT ~ AGE+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP
                 +THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+WIGHT_KG+HEIGHT_CM,
                 family= gaussian(link = "identity"), data = data)
backward <- step(glm_start)
summary(backward)

glm_no <- glm(BODYFAT ~ 1, family= gaussian(link = "identity"), data = data)
bothway <- step(glm_no, list(lower=formula(glm_no), upper = formula(glm_start)
                             ),  direction = "both", trace = 0)
summary(bothway)

jpeg(file="./Images/plotwihoutinteraction.jpeg")
par(mfrow=c(2,2))
plot(bothway)
dev.off()

glm_all <- glm(BODYFAT ~ AGE*WEIGHT*HEIGHT*ADIPOSITY*NECK+CHEST*ABDOMEN*HIP
                 *THIGH*KNEE*ANKLE*BICEPS*FOREARM*WRIST,
               family= gaussian(link = "identity"), data = data)

backward_all <- step(glm_no, list(lower=formula(glm_no), upper = formula(glm_all)
                             ),  direction = "backward", trace = 0)
summary(backward_all)


bothway_all <- step(glm_no, list(lower=formula(glm_no), upper = formula(glm_all)
                             ),  direction = "both", trace = 0)
summary(bothway_all)

jpeg(file="./Images/plotwihinteraction.jpeg")
par(mfrow=c(2,2))
plot(bothway_all)
dev.off()
