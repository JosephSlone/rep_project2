# Command History

# Libraries

library(R.utils)  # for bunzip2
library(data.table)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)

# Some colors and fonts

bold.16.text <- element_text(face = "bold", size = 12)
mypal <- colorRampPalette(brewer.pal(8, "Dark2"))

# Download and un-bzip the data file

data.file.URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
target.file <- "StormData.csv.bz2"
download.file(data.file.URL, target.file)
bunzip2(target.file)

# Read data into a data.table and clean up the Column Names

StormData <- data.table(read.csv("StormData.csv"))




# From the Storm Event Table on pg 6 NWSI 10-1605 AUGUST 17, 2007

weather_event_labels <- toupper(c("Astronomical Low Tide",  "Avalanche",
                          "Blizzard", "Coastal Flood",
                          "Cold/Wind Chill", "Debris Flow",
                          "Dense Fog", "Dense Smoke",
                          "Drought", "Dust Devil",
                          "Dust Storm", "Excessive Heat",
                          "Extreme Cold/Wind Chill", "Flash Flood",
                          "Flood", "Frost/Freeze",
                          "Funnel Cloud", "Freezing Fog",
                          "Hail", "Heat",
                          "Heavy Rain", "Heavy Snow",
                          "High Surf", "High Wind",
                          "Hurricane (Typhoon)", "Ice Storm",
                          "Lake-Effect Snow", "Lakeshore Flood",
                          "Lightning", "Marine Hail",
                          "Marine High Wind", "Marine Strong Wind",
                          "Marine Thunderstorm Wind", "Rip Current",
                          "Seiche", "Sleet",
                          "Storm Surge/Tide", "Strong Wind",
                          "Thunderstorm Wind", "Tornado",
                          "Tropical Depression", "Tropical Storm",
                          "Tsunami", "Volcanic Ash",
                          "Waterspout", "Wildfire",
                          "Winter Storm", "Winter Weather"))


# Collapse all of the different Hurricanes names into a list so that
# we can have a generic "Hurricane Category

StormData %>% filter(grepl("Hurricane|HURRICANE",EVTYPE)) -> HurricaneData
Hurricanes <- unique(as.character(HurricaneData$EVTYPE))

# Clean the data.  Converting the older event types into the modern
# event types where it is reasonable to do so.

StormData %>%
    mutate(EVTYPE = as.character(EVTYPE)) %>%
    mutate(Event.Type = ifelse(EVTYPE %in% weather_event_labels, EVTYPE, "OTHER")) %>%
    mutate(Event.Type = ifelse(EVTYPE %in% Hurricanes, "HURRICANE", Event.Type )) %>%
    mutate(Event.Type = ifelse(EVTYPE == "TSTM WIND", "THUNDERSTORM WIND", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "THUNDERSTORM WINDS", "THUNDERSTORM WIND", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "HIGH WINDS", "HIGH WIND", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "WILD/FOREST FIRE", "WILDFIRE", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "FLASH FLOODING", "FLASH FLOOD", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "URBAN/SML STREAM FLD", "FLOOD", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "EXTREME COLD", "EXTREME COLD/WIND CHILL", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "FLOODING", "FLOOD", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "RIP CURRENTS", "RIP CURRENT", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "HEAT WAVE", "EXCESSIVE HEAT", Event.Type)) %>%
    mutate(Event.Type = ifelse(EVTYPE == "EXTREME HEAT", "EXCESSIVE HEAT", Event.Type)) %>%
    mutate(Event.Type = ifelse(Event.Type == "OTHER", paste("OTHER-", EVTYPE), Event.Type)) %>%
    select(Event.Type, Property.Damage = PROPDMG , Crop.Damage = CROPDMG, Fatalities = FATALITIES , Injuries = INJURIES) -> StormData.Cleaned

# Summarise the 4 different damage types.

StormData.Cleaned %>%
    select(Event.Type, Property.Damage) %>%
    group_by(Event.Type) %>%
    summarise_each(funs(sum)) %>%
    arrange(desc(Property.Damage)) -> Property.Summary

StormData.Cleaned %>%
    select(Event.Type, Crop.Damage) %>%
    group_by(Event.Type) %>%
    summarise_each(funs(sum)) %>%
    arrange(desc(Crop.Damage)) -> Crops.Summary

StormData.Cleaned %>%
    select(Event.Type, Fatalities) %>%
    group_by(Event.Type) %>%
    summarise_each(funs(sum)) %>%
    arrange(desc(Fatalities)) -> Fatalities.Summary

StormData.Cleaned %>%
    select(Event.Type, Injuries) %>%
    group_by(Event.Type) %>%
    summarise_each(funs(sum)) %>%
    arrange(desc(Injuries)) -> Injuries.Summary

head(Injuries.Summary, 15)
head(Fatalities.Summary, 15)
head(Crops.Summary, 15)
head(Property.Summary, 15)

bold.16.text <- element_text(face = "bold", size = 12)

ggplot(Fatalities.Summary, aes(x = Event.Type, y = Fatalities, fill=Event.Type)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    theme(axis.text.x = bold.16.text) +
    xlab("Event Type") + ylab("Fatalities") +
    ggtitle("Top 10 Fatalities")

ggplot(head(Fatalities.Summary, 15), aes(x=Event.Type, y = Fatalities, fill=Event.Type)) +
    geom_bar(stat="identity") +
    xlab("Event Type") + ylab("Fatalities") +
    ggtitle("Top 15 Fatalities") +
    theme(axis.text.x = element_blank())

knit2html("report.Rmd", stylesheet="custom.css")

# https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf

