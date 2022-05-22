library(ggvis)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(AICcmodavg)
#Read the data
# Load the casualty data
casualties <- read.csv("http://tfl.gov.uk/cdn/static/cms/documents/2013-gla-data-extract-casualty.csv", header=T)
head(casualties, 2)
summary(casual)
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

# Extract the month
casualties$month <- format(casualties$Accident.Date, format="%B")
casualties$month <- as.factor(casualties$month)
casualties$month <- factor(casualties$month,levels=month.name)
summary(casualties$month)

# Extract the day of the week
casualties$day <- format(casualties$Accident.Date, format="%A")
casualties$day <- factor(casualties$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                         labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
summary(casualties$day)

# Add an hour band variable
casualties$Time <- gsub("[ [:punct:]]", "" , casualties$Time) # remove punctuation from 'Time' variable
casualties$Time <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", casualties$Time) # add a colon after 2nd value
casualties$hour<- as.POSIXlt(casualties$Time, format="%H:%M")$hour # add a new column called 'hour' with hours band
casualties[1:3,c(1,24,26, 44)] # check the results

#Cleaning the data
#Part 2: relabel some of the categories
# Relabel the 'Casualty.Severity' categories
casualties$Casualty.Severity <- factor(casualties$Casualty.Severity,
                                       levels= c("1 Fatal", "2 Serious", "3 Slight"),
                                       labels= c("Fatal", "Serious", "Slight"))
levels(casualties$Casualty.Severity)

# Relabel the 'Mode.of.Travel' categories
casualties$Mode.of.Travel <- factor(casualties$Mode.of.Travel,
                                    levels= c("1 Pedestrian", "2 Pedal Cycle", "3 Powered 2 Wheeler", "4 Car",
                                              "5 Taxi", "6 Bus Or Coach", "7 Goods Vehicle", "8 Other Vehicle"),
                                    labels= c("Pedestrian", "Pedal Cycle", "Powered 2 Wheeler", "Car",
                                              "Taxi", "Bus or Coach", "Goods Vehicle", "Other Vehicle"))
levels(casualties$Mode.of.Travel)

# Relabel the 'Casualty.Sex' categories
levels(casualties$Casualty.Sex)
casualties$Casualty.Sex <- factor(casualties$Casualty.Sex,
                                  levels= c("1 Male", "2 Female"),
                                  labels= c("Male", "Female"))
levels(casualties$Casualty.Sex)
# Relabel the 'Light.Conditions..Banded.' categories
levels(casualties$Light.Conditions..Banded.)
casualties$Light.Conditions..Banded. <- factor(casualties$Light.Conditions..Banded.,
                                               levels= c("1 Daylight", "2 Dark"),
                                               labels= c("Daylight", "Dark"))
levels(casualties$Light.Conditions..Banded.)
#Cleaning the data
#Part 3: create casualty age bands
# Create age bands
bands <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
           "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84")
casualties$ageband <- cut(casualties$Casualty.Age, 
                          breaks=c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84), 
                          labels = bands)
casualties[1:3,c(10, 44)]

#Calculate some summary statistics
# Number of casualties by mode of travel
table(casualties$Mode.of.Travel)

# Number of casualties by mode and casualty severity
with(casualties, table(Mode.of.Travel, Casualty.Severity))

# Calculate the mean age of KSI casualties by mode of travel
casualties[casualties$Casualty.Severity == "Fatal" | casualties$Casualty.Severity == "Serious", ] %>%
  group_by(Mode.of.Travel) %>% 
  summarise(mean = round(mean(Casualty.Age), 0))

#Create some bar charts
# Casualties by mode of travel
casualties %>% 
  ggvis(x = ~Mode.of.Travel, fill = ~factor(Mode.of.Travel) ) %>% 
  layer_bars() %>%
  add_axis("x", title = "Mode of Travel") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Mode of Travel")
# Casualties by month
casualties %>% 
  ggvis(~month, fill :="#e5f5f9") %>% 
  layer_bars() %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties")
# Casualties by month and mode of travel
casualties %>%
  ggvis(x = ~month, fill = ~as.factor(Mode.of.Travel)) %>%
  layer_bars() %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Mode of Travel")
# Casualties by mode and severity
casualties %>%
  group_by(Casualty.Severity) %>%
  ggvis(x = ~Mode.of.Travel, fill = ~Casualty.Severity, fillOpacity := 0.7) %>%
  layer_bars() %>%
  scale_nominal("fill", range = c("#1b9e77", "#d95f02", "#7570b3")) %>%
  add_axis("x", title = "Mode of travel") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty severity")
# KSIs by mode of travel
casualties[casualties$Casualty.Severity == "Fatal" | casualties$Casualty.Severity == "Serious",] %>%
  group_by(Mode.of.Travel) %>%
  ggvis(~Mode.of.Travel, fill = ~Casualty.Severity) %>%
  add_axis("x", title = "Mode of travel") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty severity")
# Casualty severity by month amongst pedal cyclists
casualties[casualties$Mode.of.Travel == "Pedal Cycle", ] %>%
  group_by(Casualty.Severity) %>%
  ggvis(~month, fill = ~Casualty.Severity) %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty severity")
# KSIs by month amongst pedal cyclists
casualties[casualties$Mode.of.Travel == "Pedal Cycle" & 
             casualties$Casualty.Severity == "Fatal" | casualties$Casualty.Severity == "Serious", ] %>%
  group_by(Casualty.Severity) %>%
  ggvis(~month, fill = ~Casualty.Severity) %>%
  layer_bars() %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty severity")
# Powered 2 Wheeler casualties by age band and gender
casualties[casualties$Mode.of.Travel == "Powered 2 Wheeler", ] %>% 
  group_by(Casualty.Sex) %>%
  ggvis(x = ~ageband, fill = ~Casualty.Sex, fillOpacity := 0.8) %>%
  layer_bars() %>%
  add_axis("x", title = "Casualty age band") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Casualty sex")
# Powered 2 Wheeler casualties by month and light conditions
casualties[casualties$Mode.of.Travel == "Powered 2 Wheeler", ] %>% 
  group_by(Light.Conditions..Banded.) %>%
  ggvis(x = ~month, fill = ~Light.Conditions..Banded.) %>%
  layer_bars() %>%
  scale_nominal("fill", range = c("Yellow", "Black")) %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Number of casualties") %>%
  add_legend("fill", title="Light conditions")
#Create a box plot
# Mean age of casualties by mode of travel
casualties %>% 
  ggvis(~factor(Mode.of.Travel), ~Casualty.Age) %>% 
  layer_boxplots(width = 0.5)
## Warning in rbind_all(out[[1]]): Unequal factor levels: coercing to
## character
## Warning in rbind_all(out[[1]]): Unequal factor levels: coercing to
## character
#Create a line chart
# Casualties by day
casualties %>%
  group_by(Accident.Date) %>%
  summarize(count = n()) %>%
  ggvis(~Accident.Date, ~count) %>%
  layer_lines()
#Create some interactive charts
# Number of casualties by mode of travel
casualty_freq <- casualties %>%
  group_by(Mode.of.Travel, Casualty.Severity) %>%
  summarize(count = n()) 
casualty_freq <- as.data.frame(casualty_freq)
head(casualty_freq)

freq <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(x[4], collapse = "<br />")
}

casualty_freq %>% ggvis(~Mode.of.Travel, ~count, fill = ~factor(Casualty.Severity)) %>% 
  layer_bars() %>%
  scale_nominal("fill", range = c("#e41a1c", "#377eb8", "#4daf4a")) %>%
  add_axis("x", title = "Mode of travel", properties = axis_props(labels=list(angle=90, align="left")) ) %>% 
  add_axis("y", title = "") %>%
  add_legend("fill", title="Casualty severity") %>%
  add_tooltip(freq, "hover")

casualty_ages <- casualties %>%
  group_by(Mode.of.Travel, ageband) %>%
  summarize(count = n()) 

casualty_ages %>% 
  ggvis(~ageband, ~count) %>% 
  filter(Mode.of.Travel %in% eval(input_select(c("Pedestrian", "Pedal Cycle", "Powered 2 Wheeler", 
                                                 "Car", "Taxi", "Bus or Coach", "Goods Vehicle", 
                                                 "Other Vehicle"), selected = "Pedestrian"))) %>% 
  add_axis("x", title = "Casualty age band") %>%
  add_axis("y", title = "Number of casualties") %>%
  layer_bars(fill = ~Mode.of.Travel) %>%
  hide_legend("fill")

