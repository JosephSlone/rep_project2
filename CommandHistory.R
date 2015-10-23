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
#download.file(data.file.URL, target.file)
#bunzip2(target.file)

# Read data into a data.table and clean up the Column Names

StormData <- data.table(read.csv("StormData.csv"))
old.names = colnames(StormData, do.NULL=TRUE)

fixCaps <- function(x) {
    s <- tolower(x)
    s <- strsplit(s, "_")[[1]]
    s <- paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
    s <- trimws(s)
    s <- sub(" ",".", s)
    return(s)
}

new.names <- sapply(old.names, fixCaps)
setnames(StormData, old.names, new.names)



# From the Storm Event Table on pg 6 NWSI 10-1605 AUGUST 17, 2007

weather_event_labels <- toupper(c("Astronomical Low Tide",  "Avalanche",
                          "Blizzard", "Coastal Flood",
                          "Cold/Windhill", "Debris Flow",
                          "Dense Fog", "Dense Smoke",
                          "Drought", "Dust Devil",
                          "Dust Storm", "Excessive Heat",
                          "Extremeold/Windhill", "Flash Flood",
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


# All of the different Hurricanes

StormData %>% filter(grepl("Hurricane|HURRICANE",Evtype)) -> HurricaneData
Hurricanes <- unique(as.character(HurricaneData$Evtype))

StormData %>%
    mutate(Evtype = as.character(Evtype)) %>%
    mutate(EvClass = ifelse(Evtype %in% weather_event_labels, Evtype, "OTHER")) %>%
    mutate(EvClass = ifelse(Evtype %in% Hurricanes, "HURRICANE", EvClass )) %>%
    mutate(EvClass = ifelse(Evtype == "TSTM WIND", "THUNDERSTORM WIND", EvClass)) %>%
    mutate(EvClass = ifelse(Evtype == "THUNDERSTORM WINDS", "THUNDERSTORM WIND", EvClass)) %>%
    mutate(EvClass = ifelse(Evtype == "HIGH WINDS", "HIGH WIND", EvClass)) %>%
    mutate(EvClass = ifelse(Evtype == "WILD/FOREST FIRE", "WILDFIRE", EvClass)) %>%
    mutate(EvClass = ifelse(Evtype == "FLASH FLOODING", "FLASH FLOOD", EvClass)) %>%
    mutate(EvClass = ifelse(Evtype == "URBAN/SML STREAM FLD", "FLOOD", EvClass)) %>%
    mutate(EvClass = ifelse(EvClass == "OTHER", paste("OTHER-", Evtype), EvClass)) %>%
    select(EvClass, Propdmg, Cropdmg, Fatalities, Injuries) -> StormData.Cleaned

StormData.Cleaned %>%
    select(EvClass, Propdmg) %>%
    group_by(EvClass) %>%
    summarise_each(funs(sum)) %>%
    arrange(desc(Propdmg)) -> Property.Summary

StormData.Cleaned %>%
    select(EvClass, Cropdmg) %>%
    group_by(EvClass) %>%
    summarise_each(funs(sum)) %>%
    arrange(desc(Cropdmg)) -> Crops.Summary

StormData.Cleaned %>%
    select(EvClass, Fatalities) %>%
    group_by(EvClass) %>%
    summarise_each(funs(sum)) %>%
    arrange(desc(Fatalities)) -> Fatalities.Summary

StormData.Cleaned %>%
    select(EvClass, Injuries) %>%
    group_by(EvClass) %>%
    summarise_each(funs(sum)) %>%
    arrange(desc(Injuries)) -> Injuries.Summary

