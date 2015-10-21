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
    head(10) -> fatalities

# Sorted by Injuries
Mortality.smry[order(-Injuries)] %>%
    select(Evtype, Injuries) %>%
    head(10) -> injuries


# Fatalities Plot
ggplot(fatalities, aes(x = Evtype, y = Fatalities, fill=Evtype)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    theme(axis.text.x = bold.16.text) +
    xlab("Event Type") + ylab("Fatalities") +
    ggtitle("Top 10 Fatalities") +
    scale_fill_manual(values = mypal(10))

# Injuries Plot
ggplot(injuries, aes(x = Evtype, y = Injuries, fill=Evtype)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    theme(axis.text.x = bold.16.text) +
    xlab("Event Type") + ylab("Injuries") +
    ggtitle("Top 10 Injuries") +
    scale_fill_manual(values = mypal(10))


