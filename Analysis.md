Severe Weather in the United States: Threats to Life & Property
========================================================
The goal of this analysis is to determine the 10 forms of severe weather which pose greatest risk to life & property, in order to support decision making on mitigation.
The analysis is based on the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database.
We conclude that Tornadoes are by far the greatest threat to public safety, as in the US they cause vastly more deaths and injuries than all other forms of severe weather.
Furthermore, we conclude that Hurricanes (Typhoons) are the most economically costly form of severe weather as, like tornadoes, they far outstrip other types of severe weather in the damage they incurr.

Data Processing
--------------------------------------------------------

```r
require(stringr)
```

```
## Loading required package: stringr
```

```r
require(data.table)
```

```
## Loading required package: data.table
```

```r
require(reshape2)
```

```
## Loading required package: reshape2
```

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```


For the purposes of our analysis, we use the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database.

We begin by reading the data in from file, making the assumption that the .bz2 archive containing the data is located in the R working directory.

```r
srcData <- read.csv(bzfile(file.path(getwd(), "repdata-data-StormData.csv.bz2")), 
    stringsAsFactors = FALSE)
```


We note that there are inconsistencies and incongruous values present in the ```EVTYPE``` variable, as there are `length(unique(srcData$EVTYPE))` unique values recorded, compared to the 48 unique values listed in the NOAA data.

In order to improve the quality of any results, we therefore conduct some cleaning of the data, beginning by imposing a consistent character-set and case.

```r
srcData$EVTYPE <- tolower(srcData$EVTYPE)
srcData$EVTYPE <- str_replace_all(srcData$EVTYPE, "[^[:alnum:/]]", " ")
```


Following this standardization, we group the data into the categories listed by NOAA (given below).

```r
noaaEVTYPE <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", 
    "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", 
    "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", 
    "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog", 
    "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane (Typhoon)", 
    "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood", "Lightning", "Marine Hail", 
    "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", 
    "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", 
    "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", 
    "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")
```


We begin by subsetting the data based on whether any threat to safety or property was present, as for the purposes of our analysis, events which did not result in danger to the public, or economic damage, are irrelevant.

```r
cleanData <- data.table(subset(srcData, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 
    0 | CROPDMG > 0, select = c(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, 
    CROPDMG, CROPDMGEXP)))

cleanData$EVTYPE <- str_trim(cleanData$EVTYPE)
```


As there are typographic errors present in the data, we employ regular expressions in order to robustly match the entries in the EVTYPE variable to the corresponding categories. We attempt to correct for spelling errors, and equivalent terminology, but do not attempt to map ambiguous categories to one of the NOAA event types.

```r
cleanData$EVTYPE[grepl("*astronomical low tide*", cleanData$EVTYPE)] <- "Astronomical Low Tide"
cleanData$EVTYPE[grepl("*avalanc(h)?e*", cleanData$EVTYPE)] <- "Avalanche"
cleanData$EVTYPE[grepl("*blizzard*", cleanData$EVTYPE)] <- "Blizzard"
cleanData$EVTYPE[grepl("*coastal flood*", cleanData$EVTYPE)] <- "Coastal Flood"
cleanData$EVTYPE[grepl("*cold/wind chill*", cleanData$EVTYPE)] <- "Cold/Wind Chill"
cleanData$EVTYPE[grepl("*debris flow*|mud( )?slide*|*landslide*|*rock slide*", 
    cleanData$EVTYPE)] <- "Debris Flow"
cleanData$EVTYPE[grepl("*(dense )?fog*", cleanData$EVTYPE)] <- "Dense Fog"
cleanData$EVTYPE[grepl("*dense smoke*", cleanData$EVTYPE)] <- "Dense Smoke"
cleanData$EVTYPE[grepl("*drought*", cleanData$EVTYPE)] <- "Drought"
cleanData$EVTYPE[grepl("*dust devil*", cleanData$EVTYPE)] <- "Dust Devil"
cleanData$EVTYPE[grepl("*dust storm*", cleanData$EVTYPE)] <- "Dust Storm"
cleanData$EVTYPE[grepl("*excessive heat*|*extreme heat*", cleanData$EVTYPE)] <- "Excessive Heat"
cleanData$EVTYPE[grepl("*extreme cold/wind chill*", cleanData$EVTYPE)] <- "Extreme Cold/Wind Chill"
cleanData$EVTYPE[grepl("*flash flood*|*flash/flood*|*flood/flash*|*flashflood*|*flood flash*", 
    cleanData$EVTYPE)] <- "Flash Flood"
cleanData$EVTYPE[grepl("*lakeshore flood*", cleanData$EVTYPE)] <- "Lakeshore Flood"
cleanData$EVTYPE[grepl("*flood*", cleanData$EVTYPE)] <- "Flood"
cleanData$EVTYPE[grepl("*frost*|*frost/freeze*|*freeze*", cleanData$EVTYPE)] <- "Frost/Freeze"
cleanData$EVTYPE[grepl("*funnel cloud*", cleanData$EVTYPE)] <- "Funnel Cloud"
cleanData$EVTYPE[grepl("*freezing fog*", cleanData$EVTYPE)] <- "Freezing Fog"
cleanData$EVTYPE[grepl("*marine hail*", cleanData$EVTYPE)] <- "Marine Hail"
cleanData$EVTYPE[grepl("*hail*", cleanData$EVTYPE)] <- "Hail"
cleanData$EVTYPE[grepl("*heat*", cleanData$EVTYPE)] <- "Heat"
cleanData$EVTYPE[grepl("*heavy rain*", cleanData$EVTYPE)] <- "Heavy Rain"
cleanData$EVTYPE[grepl("*heavy snow*", cleanData$EVTYPE)] <- "Heavy Snow"
cleanData$EVTYPE[grepl("*high surf*|*high seas*|*high waves*|*high swells*|*rough seas*|*rough surf*", 
    cleanData$EVTYPE)] <- "High Surf"
cleanData$EVTYPE[grepl("*marine high wind*", cleanData$EVTYPE)] <- "Marine High Wind"
cleanData$EVTYPE[grepl("*high wind*", cleanData$EVTYPE)] <- "High Wind"
cleanData$EVTYPE[grepl("*hurricane*|*typhoon*", cleanData$EVTYPE)] <- "Hurricane (Typhoon)"
cleanData$EVTYPE[grepl("*ice storm*", cleanData$EVTYPE)] <- "Ice Storm"
cleanData$EVTYPE[grepl("*lake effect snow*", cleanData$EVTYPE)] <- "Lake-Effect Snow"
cleanData$EVTYPE[grepl("*lig(h)?t(n)?ing*", cleanData$EVTYPE)] <- "Lightning"
cleanData$EVTYPE[grepl("*marine strong wind*", cleanData$EVTYPE)] <- "Marine Strong Wind"
cleanData$EVTYPE[grepl("*marine thunderstorm wind*", cleanData$EVTYPE)] <- "Marine Thunderstorm Wind"
cleanData$EVTYPE[grepl("*rip current*", cleanData$EVTYPE)] <- "Rip Current"
cleanData$EVTYPE[grepl("*seiche*", cleanData$EVTYPE)] <- "Seiche"
cleanData$EVTYPE[grepl("*sleet*", cleanData$EVTYPE)] <- "Sleet"
cleanData$EVTYPE[grepl("*storm surge/tide*|*storm surge*", cleanData$EVTYPE)] <- "Storm Surge/Tide"
cleanData$EVTYPE[grepl("*strong wind*", cleanData$EVTYPE)] <- "Strong Wind"
cleanData$EVTYPE[grepl("*t(h)?u(n)?(d)?e(e)?r(e)?(s)?torm*|*tstm*|*thunderstrom*", 
    cleanData$EVTYPE)] <- "Thunderstorm Wind"
cleanData$EVTYPE[grepl("*torn(a)?do*", cleanData$EVTYPE)] <- "Tornado"
cleanData$EVTYPE[grepl("*tropical depression*", cleanData$EVTYPE)] <- "Tropical Depression"
cleanData$EVTYPE[grepl("*tropical storm*", cleanData$EVTYPE)] <- "Tropical Storm"
cleanData$EVTYPE[grepl("*tsunami*", cleanData$EVTYPE)] <- "Tsunami"
cleanData$EVTYPE[grepl("*volcanic ash*", cleanData$EVTYPE)] <- "Volcanic Ash"
cleanData$EVTYPE[grepl("*waterspout*", cleanData$EVTYPE)] <- "Waterspout"
cleanData$EVTYPE[grepl("*wild( )?fire*|*wild/forest fire*|*brush fire*|*forest fire*", 
    cleanData$EVTYPE)] <- "Wildfire"
cleanData$EVTYPE[grepl("*winter storm*", cleanData$EVTYPE)] <- "Winter Storm"
cleanData$EVTYPE[grepl("*winter weather*", cleanData$EVTYPE)] <- "Winter Weather"

types <- unique(cleanData$EVTYPE)
```


We begin the analysis proper of the cleaned data by attempting to answer the question "Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?"

We total the fatalities and injuries attributable to each type of weather, rank them by the total number of fatalities and injuries, and take the top 10.

```r
# Getting the total number of fatalities & injuries for each event-type.
Fatalities <- cleanData[, sum(FATALITIES), by = EVTYPE]
Injuries <- cleanData[, sum(INJURIES), by = EVTYPE]

x <- data.table(types, Fatalities$V1, Injuries$V1)
setnames(x, old = c("types", "V2", "V3"), new = c("EVTYPE", "Fatalities", "Injuries"))
x <- x[with(x, order(-Fatalities, -Injuries)), ]

rankedLife <- head(x, 10)
moltenLife <- melt(rankedLife, id.vars = "EVTYPE")
```



We now consider the question "Across the United States, which types of events have the greatest economic consequences?"

We make the assumption that events which have PROP/CROPDMGEXP values corresponding to billions of dollars will be the most damaging, and as such take the corresponding subset of the the data.
We then total the property and crop damage attributable to each type of weather, rank the types of weather by the total monetary damage caused, and take the top 10.

```r
# Subset the data on the assumption that 'B' refers to billions of dollars,
# and that events with this exponent will be most expensive.
subData <- subset(cleanData, PROPDMGEXP == "B" | CROPDMG == "B", select = c("EVTYPE", 
    "PROPDMG", "CROPDMG"))

Property <- subData[, sum(PROPDMG), by = EVTYPE]
Crops <- subData[, sum(CROPDMG), by = EVTYPE]

y <- data.table(Property$EVTYPE, Property$V1, Crops$V1)
setnames(y, old = c("V1", "V2", "V3"), new = c("EVTYPE", "Property", "Crops"))
y <- y[with(y, order(-Property, -Crops)), ]

rankedDmg <- head(y, 10)
moltenDmg <- melt(rankedDmg, id.vars = "EVTYPE")
```


Results
--------------------------------------------------------
We plot below the number of fatalities/injuries which are attributable to the 25 most harmful types of severe weather. The data are ordered based on the total number of fatalities and injuries attributed to a given type of weather, with ties broken by the number of fatalities.

```r
life <- ggplot(moltenLife, aes(x = reorder(EVTYPE, value), y = value, group = EVTYPE, 
    fill = variable))
life <- life + geom_bar(stat = "identity") + coord_flip()
life <- life + ggtitle("Severe Weather Posing The Greatest Risk To Safety") + 
    ylab("Number of Fatalities/Injuries") + xlab("Type of Severe Weather")
print(life)
```

![plot of chunk plotLife](figure/plotLife.png) 

Fig 1: 10 most dangerous forms of severe weather.

It is evident that Tornadoes are by far the most dangerous form of severe weather, followed by 'Thunderstorm Wind', and 'Excessive Heat'.

We plot below the economic cost associated with the 25 most damaging types of severe weather. The data are ordered based on the total cost, with ties being broken by the estimated damage to property.

```r
prop <- ggplot(moltenDmg, aes(x = reorder(EVTYPE, value), y = value, group = EVTYPE, 
    fill = variable))
prop <- prop + geom_bar(stat = "identity") + coord_flip()
prop <- prop + ggtitle("Severe Weather Incurring The Greatest Economic Cost") + 
    ylab("Economic Cost - Billions of Dollars") + xlab("Type of Severe Weather")
print(prop)
```

![plot of chunk plotProp](figure/plotProp.png) 

Fig 2: 10 most economically costly forms of severe weather.

We see that Hurricanes (Typhoons), have the greatest economic impact, followed by Flooding, and 'Storm Surge/Tide'.

We conclude that the greatest threats to public safety are Tornadoes, and that the most economically damaging events are Hurricanes (Typhoons).
