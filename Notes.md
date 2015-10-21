# Programming Notes and Information

Data Source: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2

Data Documentation: https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf

data.file.URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"



Looks like all Hurricane Data needs to be folded together.

We need to look at just the top ? events for Fatalities and Injuries.

ggplot(summary.mtc, aes(x = factor(gear), y = meanwt)) + geom_bar(stat = "identity")