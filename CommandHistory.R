# Command History

# Libraries

library(R.utils)  # for bunzip2
library(data.table)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Some colors and fonts

bold.16.text <- element_text(face = "bold", size = 12)
mypal <- colorRampPalette(brewer.pal(10, "Dark2"))


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

# Probably Not Necessary

# StormData %>%
#     mutate(Bgn.Date = as.Date(Bgn.Date, "%m/%d/%Y")) %>%
#     mutate(Bgn.Time = as.character(Bgn.Time)) -> StormData


# Fatality and Injuries by Event Type

StormData %>% select(Evtype, Fatalities, Injuries) -> Mortality

StormData %>%
    select(Evtype, Fatalities, Injuries) %>%
    group_by(Evtype) %>%
    summarise_each(funs(sum)) -> Mortality.smry


# Sorted by Fatilites
Mortality.smry[order(-Fatalities)] %>%
    select(Evtype, Fatalities) %>%
    head(11) -> fatalities

# Sorted by Injuries
Mortality.smry[order(-Injuries)] %>%
    select(Evtype, Injuries) %>%
    head(11) -> injuries


# Fatalities Plot
ggplot(fatalities, aes(x = Evtype, y = Fatalities, fill=Evtype)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    theme(axis.text.x = bold.16.text) +
    xlab("Event Type") + ylab("Fatalities") +
    ggtitle("Top 10 Fatalities") +
    scale_fill_manual(values = mypal(11))

# Injuries Plot
ggplot(injuries, aes(x = Evtype, y = Injuries, fill=Evtype)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    theme(axis.text.x = bold.16.text) +
    xlab("Event Type") + ylab("Injuries") +
    ggtitle("Top 10 Injuries") +
    scale_fill_manual(values = mypal(11))

# Economic Impact

StormData %>%
    select(Evtype, Propdmg, Cropdmg) %>%
    group_by(Evtype) %>%
    summarise_each(funs(sum)) -> Economics.smry

property_dmg <- sum(Economics.smry$Propdmg)
crop_dmg <- sum(Economics.smry$Cropdmg)

property_top10 <- sum(head(sort(Economics.smry$Propdmg, decreasing = TRUE), 10))
crop_top10 <- sum(head(sort(Economics.smry$Cropdmg, decreasing = TRUE), 10))

# The top 10 event types over the years account for over 90% of the
# property and crop damage reported.  Therefore, I think that simply reporting the
# top 10 events of all classes will be enough.
#
# Except that Thunderstorm Wind and TSTM Wind should be lumped together.
#
# Arggh

property_top10/property_dmg
# 91%
crop_top10/crop_dmg
# 93%

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


StormData %>% filter(grepl("Hurricane|HURRICANE",Evtype)) -> HurricaneData
Hurricanes <- as.character(HurricaneData$Evtype)

StormData %>%
    mutate(Evtype = as.character(Evtype)) %>%
    mutate(EvClass = ifelse(Evtype %in% weather_event_labels, Evtype, "OTHER")) %>%
    mutate(EvClass = ifelse(Evtype %in% Hurricanes, "HURRICANE", EvClass )) %>%
    mutate(EvClass = ifelse(EvClass == "OTHER", paste("OTHER-", Evtype), EvClass)) %>%
    select(EvClass, Propdmg, Cropdmg) %>%
    group_by(EvClass) %>%
    summarise_each(funs(sum)) -> Economics.smry

# This showed a typo "THUNDERSTORM WINDS" should be "THUNDERSTORM WIND"

StormData %>%
    mutate(Evtype = as.character(Evtype)) %>%
    mutate(EvClass = ifelse(Evtype %in% weather_event_labels, Evtype, "OTHER")) %>%
    mutate(EvClass = ifelse(Evtype %in% Hurricanes, "HURRICANE", EvClass )) %>%
    mutate(EvClass = ifelse(Evtype == "THUNDERSTORM WINDS", "THUNDERSTORM WIND", EvClass)) %>%
    select(EvClass, Propdmg, Cropdmg) %>%
    group_by(EvClass) %>%
    summarise_each(funs(sum)) -> Economics.smry

# OTHER- TSTM WIND appears to be a problem.
