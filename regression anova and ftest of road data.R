library(ggvis)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(AICcmodavg)
options(scipen = 100)
#Read the data
# Load the casualty data
casualties <- read.csv("http://tfl.gov.uk/cdn/static/cms/documents/2013-gla-data-extract-casualty.csv", header=T)
head(casualties, 2)
casualties$X <- NULL # remove the redundant 'X' variable
# Load the attendant data
attendant <- read.csv("http://tfl.gov.uk/cdn/static/cms/documents/2013-gla-data-extract-attendant.csv", header=T)
#Merge the two datasets
# Merge the attendant data with the casualty data using the AREFNO
casualties <- merge(casualties, attendant, by="AREFNO")
names(casualties)

rm(attendant) # remove the attendant dataset to save memory
#Cleaning the data
#Part 1: create date and time variables
# Convert 'Accident.Date' from a factor to a date
casualties$Accident.Date <- as.Date(casualties$Accident.Date, "%d-%b-%y")
class(casualties$Accident.Date)
summary(casualties)
# ONE WAY ANOVA TEST ,i.e.  F TEST TO SEE IF SPEED OF VEHICLE HAS ANY IMPACT ON NO. OF CASUALTIES
one.way <- aov(No..of.Casualties.in.Acc. ~ Speed.Limit, data = casualties)
summary(one.way)
casualties$Light.Conditions..Banded. <- ifelse(casualties$Light.Conditions..Banded. == "1 Daylight", 1, 2)
casualties$Casualty.Severity <- ifelse(casualties$Casualty.Severity == "3 Slight", 3, 2)
two.way <- aov(No..of.Casualties.in.Acc. ~ Light.Conditions..Banded.+ Speed.Limit , data = casualties)
summary(two.way)
interaction <- aov(No..of.Casualties.in.Acc. ~ Light.Conditions..Banded.*Speed.Limit, data = casualties) 
summary(interaction)
blocking <- aov(No..of.Casualties.in.Acc. ~ Speed.Limit+Light.Conditions..Banded.+ Casualty.Severity, data = casualties)
summary(blocking)
library(AICcmodavg)

model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)
par(mfrow=c(2,2))
plot(blocking)
par(mfrow=c(1,1))
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way
tukey.plot.aov<-aov(No..of.Casualties.in.Acc. ~ Speed.Limit, data = casualties)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)
mean.No..of.Casualties.in.Acc..data <- casualties %>%
  group_by(Speed.Limit, Casualty.Severity) %>%
  summarise(
    No..of.Casualties.in.Acc. = mean(No..of.Casualties.in.Acc.)
  )
mean.No..of.Casualties.in.Acc..data

mean.No..of.Casualties.in.Acc..data$group <- c("a","a","b","b","c","c","d","d","e","e","f","f")

mean.No..of.Casualties.in.Acc..data

one.way.plot <- ggplot(casualties, aes(x = Casualty.Severity, y = No..of.Casualties.in.Acc., group=Speed.Limit)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

one.way.plot

one.way.plot <- one.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.No..of.Casualties.in.Acc..data, aes(x=Casualty.Severity, y=No..of.Casualties.in.Acc.))

one.way.plot

one.way.plot <- one.way.plot +
  geom_text(data=mean.No..of.Casualties.in.Acc..data, label=mean.No..of.Casualties.in.Acc..data$group, vjust = -8, size = 5) +
  facet_wrap(~ Speed.Limit)

one.way.plot

t.test(No..of.Casualties.in.Acc. ~ Casualty.Severity, data = casualties)
library(rstatix)
stat.test <- casualties %>% 
  t_test(No..of.Casualties.in.Acc. ~ Casualty.Severity ,detailed = TRUE) %>%
  add_significance()
stat.test
# Create a box-plot
bxp <- ggboxplot(
  casualties, y = "No..of.Casualties.in.Acc.", x = "Casualty.Severity", 
  xlab = "Casualty.Severity", ylab = "No..of.Casualties.in.Acc.", add = "jitter"
)

# Add p-value and significance levels

stat.test <- stat.test %>% add_xy_position(x = "group")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))
stat.test <- stat.test %>% add_xy_position(x = "group")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))