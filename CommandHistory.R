# Command History

# Libraries

library(R.utils)  # for bunzip2
library(data.table)
library(dplyr)
library(tidyr)

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

StormData %>%
    mutate(Bgn.Date = as.Date(Bgn.Date, "%m/%d/%Y")) %>%
    mutate(Bgn.Time = as.character(Bgn.Time)) -> StormData


