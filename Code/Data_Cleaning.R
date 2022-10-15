# Data Cleaning R

# Load Packages
library(readr)
library(dplyr)
library(ggplot2)

# Load Data
BodyFat <- read_csv("BodyFat.csv")
names(BodyFat)

# Create metric versions of height and weight
BodyFat<- BodyFat %>% mutate(WEIGHT_KG = round(WEIGHT*0.45359237,2),
                             HEIGHT_CM = round(HEIGHT*2.54,2))

# Check for outliers using boxplots on all variables
# Body Fat
boxplot(BodyFat$BODYFAT, ylab="Body Fat") #outliers found
BodyFat$IDNO[BodyFat$BODYFAT<4]
BodyFat<- BodyFat %>% filter(!IDNO %in% c(172, 182))

# Density
boxplot(BodyFat$DENSITY, ylab="Density")

#Weight
boxplot(BodyFat$WEIGHT, ylab="Weight")

#Height
boxplot(BodyFat$HEIGHT, ylab="Height")
BodyFat$IDNO[BodyFat$HEIGHT<40]

# Fix height outlier of ID 42
round(sqrt((BodyFat$WEIGHT[BodyFat$IDNO==42]*0.45359237)/BodyFat$ADIPOSITY[BodyFat$IDNO==42])*39.37004,digits=2)
BodyFat<- BodyFat %>% mutate(HEIGHT = case_when(
  HEIGHT == 29.50 ~ 69.43,
  TRUE ~ HEIGHT
))

# BMI
boxplot(BodyFat$ADIPOSITY, ylab="BMI")
BodyFat$IDNO[BodyFat$ADIPOSITY>35]
# Check accuracy of BMI
# ID 39
BodyFat$ADIPOSITY[BodyFat$IDNO==39]==round(BodyFat$WEIGHT_KG[BodyFat$IDNO==39]/((BodyFat$HEIGHT_CM[BodyFat$IDNO==39]/100)^2),1)
# ID 41
BodyFat$ADIPOSITY[BodyFat$IDNO==41]==round(BodyFat$WEIGHT_KG[BodyFat$IDNO==41]/((BodyFat$HEIGHT_CM[BodyFat$IDNO==41]/100)^2),1)
# ID 216
BodyFat$ADIPOSITY[BodyFat$IDNO==216]==round(BodyFat$WEIGHT_KG[BodyFat$IDNO==216]/((BodyFat$HEIGHT_CM[BodyFat$IDNO==216]/100)^2),1)
# All 3 outliers are true values.

# Neck
boxplot(BodyFat$NECK, ylab="Neck") #same outlier as others, accurate
BodyFat$IDNO[BodyFat$NECK > 45]

# Chest
boxplot(BodyFat$CHEST, ylab="Chest")
BodyFat$IDNO[BodyFat$CHEST>125]

# Abdomen
boxplot(BodyFat$ABDOMEN, ylab="Abdomen")
BodyFat$IDNO[BodyFat$ABDOMEN>125]

# Hip
boxplot(BodyFat$HIP, ylab="Hip")
BodyFat$IDNO[BodyFat$HIP>120]

# Thigh
boxplot(BodyFat$THIGH, ylab="Thigh")
BodyFat$IDNO[BodyFat$THIGH>75]

# Knee
boxplot(BodyFat$KNEE, ylab="Knee")
BodyFat$IDNO[BodyFat$KNEE>46]

# Ankle
boxplot(BodyFat$ANKLE, ylab="Ankle")
BodyFat$IDNO[BodyFat$ANKLE>27]

# Biceps
boxplot(BodyFat$BICEPS, ylab="Biceps")
BodyFat$IDNO[BodyFat$BICEPS>40]

# Forearm
boxplot(BodyFat$FOREARM, ylab="Forearm")
BodyFat$IDNO[BodyFat$FOREARM<24]
BodyFat$IDNO[BodyFat$FOREARM>34.5]

# Wrist
boxplot(BodyFat$WRIST, ylab="Wrist")
BodyFat$IDNO[BodyFat$WRIST>20.50]

# Any outliers seem to be the same people who also have higher BMI's.
# These BMI's were re-calculated based on height and weight. Shown to be accurate across measures.
# Only took out Body Fat measures of < 4, due to inability to find online "calculator" of body fat accurate enough to the data - for replacement.

# Write new csv file with clean data -> BodyFat2.csv
# write.csv(BodyFat, "~/Desktop/628 Practicum/Mod 2//BodyFat2.csv")
