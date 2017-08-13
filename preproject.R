# Packages needed for this project
library(R.utils)
library(dplyr)

# Download the data from the course website and unzip the file onto the working directory:

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = "StormData.bz2", mode = "wb")
bunzip2(filename = "StormData.bz2", destname= "StormData.csv", remove = F, overwrite = T)

# Read the data into R:

stormData <- read.csv("StormData.csv")

# To address the above objectives, we need the following columns at most and we will reduce the working data to that.

- EVTYPE (the event type)
- FATALITIES (the number of deaths caused by the event)
- INJURIES (the number of injuries caused by the event)
- PROPDMG (the amount of property damage caused by the event)
- PROPDMGEXP (a multiplier code for PROPDMG)
- CROPDMG (the amount of crop damage caused by the event)
- CROPDMGEXP (a multiplier code for CROPDMG)

stormData <- stormData[, c(8,23:28)]

# Explore the data and determine which needed columns are not having the workable type and content of data.
str(stormData)

# Looks like all columns are in the correct data type. However, based on the Storm Data Documentation, we expect there should only be 5 levels for the mulitplier code:

- blank/no value
- H: for hundreds
- K: for thousands
- M: for millions
- B: for billions

# Check the frequency table of `PROPDMGEXP` and determine the significance of uncategorized multiplier codes.
PROP_Count <- table(stormData$PROPDMGEXP)
Percentage <- round(prop.table(PROP_Count)*100,2)
cbind(PROP_Count,Percentage)

# Check the frequency table of `PROPDMG` where `PROPDMGEXP` is blank and determine the significance of `PROPDMG=0`
PROP.blank_Count <- table(as.factor(stormData$PROPDMG[stormData$PROPDMGEXP==""]))
Percentage <- round(prop.table(PROP.blank_Count)*100,2)
cbind(PROP.blank_Count,Percentage)

# Check the frequency table of `CROPDMGEXP` and determine the significance of uncategorized multiplier codes.
CROP_Count <- table(stormData$CROPDMGEXP)
Percentage <- round(prop.table(CROP_Count)*100,2)
cbind(CROP_Count,Percentage)

# Check the frequency table of `CROPDMG` where `CROPDMGEXP` is blank and determine the significance of `CROPDMG=0`
CROP.blank_Count <- table(as.factor(stormData$CROPDMG[stormData$CROPDMGEXP==""]))
Percentage <- round(prop.table(CROP.blank_Count)*100,2)
cbind(CROP.blank_Count,Percentage)


# It shows from the summary tables that uncategorized multiplier codes constitute to `< 1%` of the number of total observations and that `> 99%` of the blank categories have estimated damage `=0`). Therefore it is safe to assign the value `1` as the multiplier value.

# Create new variables to assign the multiplier value based on the multiplier codes.

stormData$PROPDMGMULT <- 1
stormData$PROPDMGMULT[stormData$PROPDMGEXP=="H"|stormData$PROPDMGEXP=="h"] <- 100
stormData$PROPDMGMULT[stormData$PROPDMGEXP=="K"|stormData$PROPDMGEXP=="k"] <- 1000
stormData$PROPDMGMULT[stormData$PROPDMGEXP=="M"|stormData$PROPDMGEXP=="m"] <- 1000000
stormData$PROPDMGMULT[stormData$PROPDMGEXP=="B"|stormData$PROPDMGEXP=="b"] <- 1000000000

stormData$CROPDMGMULT <- 1
stormData$CROPDMGMULT[stormData$CROPDMGEXP=="H"|stormData$CROPDMGEXP=="h"] <- 100
stormData$CROPDMGMULT[stormData$CROPDMGEXP=="K"|stormData$CROPDMGEXP=="k"] <- 1000
stormData$CROPDMGMULT[stormData$CROPDMGEXP=="M"|stormData$CROPDMGEXP=="m"] <- 1000000
stormData$CROPDMGMULT[stormData$CROPDMGEXP=="B"|stormData$CROPDMGEXP=="b"] <- 1000000000

# Create new variables pertaining to the actual property or crop damage in US dollars

stormData$PROPDMGVAL <- stormData$PROPDMG*stormData$PROPDMGMULT
stormData$CROPDMGVAL <- stormData$CROPDMG*stormData$CROPDMGMULT

# The dataset is now workable and ready for analysis.

# RESULTS

# Question 1: Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health?

# POPULATION HEALTH INDICATORS as AFFECTED by STORM EVENTS ACROSS U.S.

# As described in the Storm Data Documentation, fatalities and injuries directly or indirectly caused by the weather event are entered in the Storm Data software "fatality" and "injury" entry fields, respectively. We utilized the `INJURIES` and `FATALITIES` columns as main indicators of population health.
# The chart below shows the total cases of INJURIES and FATALITIES by EVENT TYPE.

total_inj <- aggregate(INJURIES ~ EVTYPE, stormData, sum)
top_inj <- head(arrange(total_inj, desc(INJURIES)),5)

total_ftl <- aggregate(FATALITIES ~ EVTYPE, stormData, sum)
top_ftl <- head(arrange(total_ftl, desc(FATALITIES)),5)

# The predominant storm event that impacts to population health is **TORNADO** for both injuries and fatalities.

head(top_inj,1)
head(top_ftl,1)

par(mfrow=c(1,2), mar=c(7,4,4,2), las = 2)
barplot(top_inj$INJURIES,
        names=top_inj$EVTYPE,
        main = "Injuries by Storm Event",
        cex.names = 0.7)
barplot(top_ftl$FATALITIES,
        names=top_ftl$EVTYPE,
        main = "Fatalities by Storm Event",
        cex.names = 0.7)




# Question 2: Across the United States, which types of events have the greatest economic consequences?

# Property damage estimates are entered as actual dollar amounts, if a reasonably accurate estimate from an insurance company or other qualified individual is available. If this estimate is not available, then the preparer has two choices: either check the "no information available" box, or make an estimate. The damage has been estimated in good faith by the preparer of the dataset.  Property damage estimates are very important for many users and should be obtained if at all possible.
# Crop damage information were obtained only from reliable sources, such as the U.S. Department of Agriculture (USDA), the county/parish agricultural extension agent, the state department of agriculture, crop insurance agencies, or any other reliable authority.  Crop damage amounts were obtained from the USDA or other similar agencies.
# For this data analysis project, we utilized `PROPERTY` and `CROP` damage information (expressed in USD) to infer which storm events have greatest economic impacts.

total_propdmg <- aggregate(PROPDMGVAL ~ EVTYPE, stormData, sum)
top_propdmg <- head(arrange(total_propdmg, desc(PROPDMGVAL)),10)

total_cropdmg <- aggregate(CROPDMGVAL ~ EVTYPE, stormData, sum)
top_cropdmg <- head(arrange(total_cropdmg, desc(CROPDMGVAL)),10)

# The predominant storm event that affects the economy in terms of Property Damage is **FLOOD**.

head(top_propdmg,1)

par(mfrow=c(1,1), mar=c(7,4,4,2), las = 2)
barplot(top_propdmg$PROPDMGVAL,
        names=top_propdmg$EVTYPE,
        main = "Property Damage (in USD) by Storm Event",
        cex.names = 0.7)

# The predominant storm event that affects the economy in terms of Crop Damage is **DROUGHT**.

head(top_cropdmg,1)

par(mfrow=c(1,1), mar=c(7,4,4,2), las = 2)
barplot(top_cropdmg$CROPDMGVAL,
        names=top_cropdmg$EVTYPE,
        main = "Crop Damage (in USD) by Storm Event",
        cex.names = 0.7)
