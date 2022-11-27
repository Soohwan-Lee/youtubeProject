install.packages("lme4")
install.packages("nlme")
install.packages("lsmeans")

library(ggplot2)
library(plyr)
library(dplyr)
library(lme4)
library(nlme)
library(lsmeans)
library(emmeans)

# --------------- #
# RED: "#F8766D"  #
# BLUE: "#01BFC4" #
# --------------- #

### Set File Path for Window Environment
setwd('C:/Users/LeeSooHwan/OneDrive - unist.ac.kr (1)/EXPC/EXPC Project/2021/taeyoon-visualization')

### Load Data
youtubePercentage <- read.csv(file = "./revisedData/youtubePercentage.csv", header=T, fileEncoding="UTF-8-BOM")

### declare factor
youtubePercentage$participant <- as.factor(youtubePercentage$participant)
youtubePercentage$day <- as.factor(youtubePercentage$day)
youtubePercentage$type <- as.factor(youtubePercentage$type)
youtubePercentage$phase <- as.factor(youtubePercentage$phase)
youtubePercentage$percentage <- as.numeric(youtubePercentage$percentage)

### Mixed Effect Model as fit
fit <- lmer(percentage ~ type * phase + (1|participant), data = youtubePercentage)
taeyoon <- lme(percentage ~ type * phase, random = ~ 1|participant, data = youtubePercentage, na.action=na.omit)

### Get ANOVA Table
anova(fit)
summary(fit)
anova(taeyoon)
summary(taeyoon)

## Original 2-way ANOVA
anova <- aov(percentage ~ (type*phase) + Error(participant/(type*phase)), data = youtubePercentage)
anova <- lmer(percentage ~ 1 + type*phase + (participant/(type*phase)), data=youtubePercentage)
summary(anova)


