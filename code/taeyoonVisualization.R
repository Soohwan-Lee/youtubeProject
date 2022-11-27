library(ggplot2)
library(plyr)
library(dplyr)

# --------------- #
# RED: "#F8766D"  #
# BLUE: "#01BFC4" #
# --------------- #

### Set File Path for Window Environment
setwd('C:/Users/LeeSooHwan/OneDrive - unist.ac.kr (1)/EXPC/EXPC Project/2021/taeyoon-visualization')

### Load Data
# Behavior
behavior <- read.csv(file = "./revisedData/behavior.csv", header=T, fileEncoding="UTF-8-BOM")
# Experience
experience <- read.csv(file = "./revisedData/experience.csv", header=T, fileEncoding="UTF-8-BOM") 

### Start Y axis from n
require(scales)
my_trans <- function(from=0) 
{
  trans <- function(x) x-from
  inv <- function(x) x+from
  trans_new("myscale", trans, inv, 
            domain = c(from, Inf))
}

### Boxplot of Bahavior
behavior$value <- as.numeric(behavior$value)
behavior$experience <- as.factor(behavior$experience)
behavior$type <- factor(behavior$type, level = c("metaphorical", "simile"))

behaviorPlot <- ggplot(behavior, aes(x=experience, y=value, fill=type)) + 
  geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  #scale_x_discrete(labels=c("")) +
  scale_fill_discrete(labels=c("Metaphorical", "Simile")) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(0,1,2,3)) + 
  coord_cartesian(ylim = c(0, 3)) +
  geom_point(aes(colour = type), position=position_jitterdodge(), show.legend = F) +
  labs(title="Behavior Level Change Comparison(*)", x="", y = "Difference Score", fill = "Experiment Type") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "left")
behaviorPlot

### Boxplot of Experience
experience$value <- as.numeric(experience$value)
experience$experience <- factor(experience$experience, level = c("stress", "timeIntuition", "appInfluence"))
experience$type <- factor(experience$type, level = c("metaphorical", "simile"))

behaviorPlot <- ggplot(experience, aes(x=experience, y=value, fill=type)) + 
  geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  scale_x_discrete(labels=c("Stress", "Time Intuition Change", "App Influence")) +
  scale_fill_discrete(labels=c("Metaphorical", "Simile")) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5,6,7)) + 
  coord_cartesian(ylim = c(1, 7)) +
  geom_point(aes(colour = type), position=position_jitterdodge(), show.legend = F) +
  labs(title="Experience Comparison(*)", x="", y = "Score", fill = "Experiment Type") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "left")
behaviorPlot
