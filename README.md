# Weather data analytics

General Information
-------------------------

"50% chance of rain" says the Weather Channel. How did they get this prediction?

Modeling the weather is one of the most complex and difficult problems in data analysis, due in part to the vast amount of data collected and the incredibly complicated system it comes from.

The file `weather_proj.txt` contains meteorogical observations for 1000 weather stations in the US for the year 2012 provided by NOAA (National Oceanic and Atmospheric Administration). Items measured include precipitation, snowfall, minimum / maximum temperature, etc. The data are already in a tidy format.

A note on units from the `readme.txt` provided by NOAA at <ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt>:

- `PRCP = Precipitation (tenths of mm)`
- `SNOW = Snowfall (mm)`
- `SNWD = Snow depth (mm)`
- `TMAX = Maximum temperature (tenths of degrees C)`
- `TMIN = Minimum temperature (tenths of degrees C)`

Many stations do not have all measurements for all days; some record only temperature, or only precipitation. Additionally, some stations do not have any measurements on some days.

The file `stations_proj.txt` contains latitude, longitude, and elevation for the 1000 weather stations found in the previous file. Note that the longitude field is not the actual longitude, but instead a transform of it, so that the western-most point in the data is at longitude 0. This ensures that plotting with longitude does not create a mirror image of the US.

The file `stations_with_loc.txt` contains a field `location` that has the name of the station's location.

General skills used in this project are the following:

- Reading in `.txt` files
- Manipulating data with `dplyr`
    * `separate` from `tidyr` 
    * `inner_join`
    * Standard commands: `filter`, `mutate`, `summarize`, etc
- Plotting with `ggplot2`
- Critical thinking about data and transformations
- `dcast` to prepare data for model-fitting
- Fitting a linear model and using ANOVA to compare models
- t-tests
- $\chi^2$ tests

1. Read In Data and Merge
------------------------------

```{r settings, message=FALSE}
options(stringsAsFactors = FALSE)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(broom)
library(magrittr)
library(reshape2)
```

**First, read in `weather_proj.txt`  and `stations_proj.txt` and store then as data frames.**

```{r readProj}
weatherProj <- read.table("weather_proj.txt", header=TRUE)
head(weatherProj)
tail(weatherProj)

stationsProj <- read.table("stations_proj.txt", header=TRUE)
head(stationsProj)
tail(stationsProj)
```

**Then, I merge the two datasets**

```{r mergeData}
proj <- inner_join(weatherProj, stationsProj, by="station")
head(proj)
tail(proj)
```

**I Extract the day and month from the `date` field**

```{r extractDayMonth}
dateToDay <- function(date) {
  date%%100
}
dateToMonth <- function(date) {
  as.integer(date/100)%%100
}

projDayMonth <- proj %>%
  mutate(day = dateToDay(date), month = dateToMonth(date)) %>%
  select(-date)
head(projDayMonth)
tail(projDayMonth)
```

2. Some Summaries
-------------------

**What was the hottest temperature measured in the US in 2012? Where was this temperature measured? Repeat this for the coldest temperature.**

```{r extremeTemps}
stationsWithLoc <- read.table("stations_with_loc.txt", sep="\t",
                              header=TRUE, quote="")
head(stationsWithLoc)
tail(stationsWithLoc)

tempProj <- proj %>%
  filter(obs_type == "TMIN" | obs_type == "TMAX")

getStationLoc <- function(stationRaw) {
  tempLoc <- stationsWithLoc %>%
    filter(station == stationRaw)
  tempLoc[1,]$location
}

tempMax <- tempProj[which.max(tempProj$obs_value),]
sprintf("Hottest temp (Celsius): %f; recorded in %s", tempMax$obs_value/10,
        getStationLoc(tempMax$station))

tempMin <- tempProj[which.min(tempProj$obs_value),]
sprintf("Hottest temp (Celsius): %f; recorded in %s", tempMin$obs_value/10,
        getStationLoc(tempMin$station))
```

**Which station got the most rain over the course of 2012?**

```{r mostRain}
rainProj <- proj %>%
  filter(obs_type == "PRCP") %>%
  group_by(station) %>%
  summarize(rainTot = sum(obs_value)) %>%
  select(station, rainTot)

station <- rainProj[which.max(rainProj$rainTot),]$station
sprintf("Station that got the most rain: %s in %s", station,
        getStationLoc(station))
```

**Which date has the most recorded observations?**

```{r mostObs}
obsProj <- proj %>%
  group_by(date) %>%
  summarize(obsTot = n()) %>%
  select(date, obsTot)

obsProj[which.max(obsProj$obsTot),]$date
```

3. Visualization
------------------------

**This plot shows the elevations of weather stations in the US in geographical space.**

```{r stationElevation}
stationElevation <- stationsProj %>%
  filter(elevation >= 0) # Excluding station with bad value

ggplot(data=stationElevation, mapping=aes(x=long,y=lat,color=elevation)) +
  geom_point(alpha=0.75) +
  scale_color_gradientn(colors=terrain.colors(3), name="Elevation") +
  theme_bw() +
  ggtitle("Weather station elevations") +
  labs(x="Longitude", y="Latitude")
```

The map is sightly compressed on the x axis and that is because the longitude scale is larger than the latitude scale and is therefore slightly skewed. In order to solve this we can give more room in the y axis and normalize the bed so that the plot has the right aspect ratio.


### Part 2

**I plot a temperature measure over the course of the year for two weather stations that recorded temperature data.**

```{r temp2Stations}
# Calculates the number of days since the beginning of the year
calcDays <- function(month, day) {
  as.integer(difftime(ISOdate(2012, month, day), ISOdate(2012,1,1), units="days"))
}

chosenStations <- c("USC00011084", "USW00094224")

stationTemps <- projDayMonth %>%
  filter(obs_type == "TMAX",
         station == chosenStations[1] | station == chosenStations[2]) %>%
  mutate(days = calcDays(month, day), temp = obs_value/10) %>%
  select(days, temp, station)

ggplot(data=stationTemps, mapping=aes(x=days, y=temp, color=station)) +
  geom_point(alpha=0.75) +
  theme_bw() +
  scale_color_manual(values=c("indianred", "skyblue"), name="Station") +
  labs(x="Days since January 1", y="Temperature (Celsius)") +
  ggtitle("2012 max temperatures")
```

### Part 3

**Here I illustrate the two stations that I chose in the previous step.**

```{r spatial2Stations}
chosenStationsProj <- stationsProj %>%
  filter(station == chosenStations[1] | station == chosenStations[2])

ggplot(data=stationsProj, mapping=aes(x=long,y=lat)) +
  geom_point(alpha=0.75, color="lightgray") +
  theme_bw() +
  ggtitle("Max temperature station elevations") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=chosenStationsProj, mapping=aes(x=long, y=lat, color=station),
             size=3) +
  scale_color_manual(values=c("indianred", "skyblue"), name="Station")
```
    
### Part 4

**To examine rainfall data, I make a spatial plot of the US using the total rainfall over a particular month.**

```{r spatialRain}
aprilShowers <- projDayMonth %>%
  filter(month == 4, obs_type == "PRCP") %>%
  group_by(lat, long) %>%
  summarize(totRain = sum(obs_value)/10)

ggplot(data=aprilShowers, mapping=aes(x=long, y=lat, color=totRain)) +
  geom_point(alpha=0.75) +
  scale_color_gradient(low="skyblue", high="blue", name="Rainfall (mm)") +
  theme_bw() +
  ggtitle("Total April rainfall") +
  labs(x="Longitude", y="Latitude")
```


4. Inference
---------------------------------

### Part 1

**After choosing a random weather station, I construct a 90% CI for the probability there is precipitation on any given day.**

```{r prcpStation}
statPrcp <- weatherProj %>%
  filter(station == "USC00401790", obs_type == "PRCP") %>%
  mutate(prcp = obs_value > 0) %>%
  select(date, prcp)

# Built-in test (random p, as we only want to find the CI)
n <- nrow(statPrcp)
x <- sum(statPrcp$prcp)
binom.test(x=x, n=n, p=0.5, conf.level=0.90)$conf.int # Choose one?
prop.test(x=x, n=n, p=0.5, conf.level=0.90)$conf.int

# Calculating CI by hand
p.hat <- x/n
z <- qnorm(0.95)
stderr <- sqrt(p.hat*(1-p.hat)/n)
lowerBound <- p.hat - z*stderr
upperBound <- p.hat + z*stderr
sprintf("(%f, %f)", lowerBound, upperBound)
```

**Here, I compute the p-value via simulation for the following hypothesis test involving stations USW00004725 (Binghampton, NY), USW00014765 (Providence, RI), and USW00014860 (Erie, PA):**

- $H_0$: Precipitation occurring at all three stations on any given day is probabilistically independent.  Mathematically, this means:
$$\operatorname{Pr}(\text{precip. at all three stations})=\operatorname{Pr}(\text{precip. at station 1})\cdot \operatorname{Pr}(\text{precip. at station 2})\cdot \operatorname{Pr}(\text{precip. at station 3})$$
- $H_1$: Precipitation occurring at all three stations is probabilistically dependent.

```{r pValRainClose}
rainStat <- c("USW00004725", "USW00014765", "USW00014860")

prcpAllStat <- weatherProj %>%
  filter(obs_type == "PRCP", station %in% rainStat) %>%
  mutate(rain = obs_value > 0) %>%
  select(date, station, rain) %>%
  dcast(date ~ station, value.var = "rain") %>%
  mutate(ALL_STATIONS = USW00004725 & USW00014765 & USW00014860)

days <- nrow(prcpAllStat)
probPrcpStat1 <- sum(prcpAllStat$USW00004725)/days
probPrcpStat2 <- sum(prcpAllStat$USW00014765)/days
probPrcpStat3 <- sum(prcpAllStat$USW00014860)/days
probPrcpAllStat <- sum(prcpAllStat$ALL_STATIONS)/days

# Create some fake data with the real probabilities. I have to generate one triad of data for every simulation. I am running many simulations in order to get an accurate p-value.

multProbRainSim <- function(probPrcpStat1, probPrcpStat2, probPrcpStat3) {
  # Simulate rain over a year using sample probabilities
  prcpSimStat1 <- rbinom(days,1,probPrcpStat1)
  prcpSimStat2 <- rbinom(days,1,probPrcpStat2)
  prcpSimStat3 <- rbinom(days,1,probPrcpStat3)
  
  # Calculate the probabilities of the data above
  probPrcpSimStat1 <- sum(prcpSimStat1)/days
  probPrcpSimStat2 <- sum(prcpSimStat2)/days
  probPrcpSimStat3 <- sum(prcpSimStat3)/days
  
  # Calculate the probability of rain in all three stations
  probPrcpSimStat1 * probPrcpSimStat2 * probPrcpSimStat3
}

expNum = 1e4
simPHat <- replicate(expNum, multProbRainSim(probPrcpStat1,
                                             probPrcpStat2,
                                             probPrcpStat3))
mu0 <- mean(simPHat)
pVal <- 2*sum(abs(simPHat-mu0) >= abs(probPrcpAllStat-mu0))/expNum
pVal
```

 **Here, I compute the p-value using simulation of the same hypothesis test for the following stations: USC00195984 (Norton, MA), USS0008T01S (Signal Peak Trail, NM), and USC00228374 (Michigan State University), which lie at larger distances between one another.**

```{r pValRainFar}
rainStat <- c("USC00195984", "USS0008T01S", "USC00228374")

prcpAllStat <- weatherProj %>%
  filter(obs_type == "PRCP", station %in% rainStat) %>%
  mutate(rain = obs_value > 0) %>%
  select(date, station, rain) %>%
  dcast(date ~ station, value.var = "rain") %>%
  mutate(ALL_STATIONS = USC00195984 & USS0008T01S & USC00228374)

probPrcpStat1 <- sum(prcpAllStat$USC00195984)/days
probPrcpStat2 <- sum(prcpAllStat$USS0008T01S)/days
probPrcpStat3 <- sum(prcpAllStat$USC00228374)/days
probPrcpAllStat <- sum(prcpAllStat$ALL_STATIONS)/days
probPrcpAllStatVect <- replicate(expNum, probPrcpAllStat)

simPHat <- replicate(expNum, multProbRainSim(probPrcpStat1,
                                             probPrcpStat2,
                                             probPrcpStat3))
mu0 <- mean(simPHat)
pVal <- 2*sum(abs(simPHat-mu0) >= abs(probPrcpAllStatVect-mu0))/expNum
pVal
```

The p-value is very small when the stations are geographically close to each other, so we can reject $H_0$; this suggests that the precipitation occurring at all three stations is probabilistically dependent. On the other hand, the p-value is very large when the stations are farther from each other, so we fail to reject $H_0$; this suggests that the precipitation occurring at all three stations is probabilistically independent. This is not surprising, as one storm could lead to rain at three stations close to each other (leading to dependence), but weather patterns vary more across the country.

### Part 2


**One longitude to delineate the east coast, one longitude to delineate the west coast, and one latitude to delineate the northern half of the country from the southern half is chosen to separate the country into sectors. Alaska and Hawaii have been assigned a sector "none", so as to be excluded from further analysis.**

```{r addSector}
nsLine <- 38
wLine <- 50
eLine <- 80
noneLine <- 40

getSector <- function(long, lat) {
  ns <- ifelse(lat > nsLine, "north", "south")
  wme <- ifelse(long > wLine & long < eLine, "mid-", 
                ifelse(long < wLine, "west", "east"))
  defSector <- ifelse(wme == "mid-", paste(wme, ns, sep=""),
                      paste(ns, wme, sep=""))
  sector <- ifelse(long < noneLine, "none", defSector)
}

allProjSectors <- stationsProj %>%
  group_by(lat, long) %>%
  summarize(sector = getSector(long, lat))
```

```{r plotSector}
projSectors <- allProjSectors %>%
  filter(sector != "none")

ggplot(data=projSectors, mapping=aes(x=long, y=lat, color=sector)) +
  geom_point(alpha=0.75) +
  geom_hline(yintercept=nsLine, color="darkgray", linetype="dashed") +
  geom_vline(xintercept=wLine, color="darkgray", linetype="dashed") +
  geom_vline(xintercept=eLine, color="darkgray", linetype="dashed") +
  scale_color_manual(values=brewer.pal(6, "Set2"), name="Sector") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) + 
  labs(x="Longitude", y="Latitude") +
  ggtitle("Weather stations by sector") +
  theme_bw()
```

**This boxplot shows the distributions of temperatures in 6 sectors: northwest, northeast, mid-north, southwest, southeast, and mid-south, given the differences in weather patterns across sectors.**

```{r sectTempDist}
tempProjJan1 <- projDayMonth %>%
  filter(day == 1, month == 1, obs_type == "TMAX") %>%
  mutate(sector = getSector(long, lat), obs_value = obs_value/10) %>%
  filter(sector != "none")

sectTempGraph <- ggplot(data=tempProjJan1,
                        mapping=aes(x=sector, y=obs_value, fill=sector)) +
  geom_boxplot() +
  scale_color_manual(values=brewer.pal(6, "Set2")) +
  theme_bw() +
  theme(legend.position="none") +
  labs(x="Sector", y="Temperature (Celsius)") +
  ggtitle("Temperature on January 1, 2012")
sectTempGraph
```

They appear approximately Normal as the whiskers are of similar length on each side, and the median is near the center of the box; there is no dramatic skew.

**For all pairs of sectors, I test the null hypothesis that the mean temperatures are equal vs. the alternative that they are unequal.**

```{r meanTempTest}
sectors <- c("northwest", "northeast", "mid-north", "southwest", "southeast",
             "mid-south")
getTemps <- function(sect) {
  tempProjJan1 %>%
    filter(sector == sect) %>%
    select(obs_value)
}
sectorsPValue <- function(sector1, sector2) {
  tidy(t.test(x=getTemps(sector1), y=getTemps(sector2)))
}
sectorTempTests <- as.data.frame(expand.grid(sectors, sectors)) %>%
  rename(sector1 = Var1, sector2 = Var2) %>%
  filter(sector1 != sector2) %>%
  group_by(sector1, sector2) %>%
  do(sectorsPValue(.$sector1, .$sector2))
sectorTempTests %>%
  select(sector1, sector2, p.value)
```

The p-value for the northwest/northeast is very high (0.798007), so we cannot reject $H_0$ under any reasonable cut-off. It is also quite high for the southwest/southeast (0.16539), and somewhat high for the southeast-mid-south (0.0564719). All other p-values are far below a 5% cut-off. Therefore, p-value cut-offs should be catered to the data.

**To further confirm the statistical inference, I built a linear regression model that helps test the null hypothesis that all six sectors have equal mean temperature.**

```{r linMeanTempTest}
tempProjMean <- tempProjJan1 %>%
  group_by(sector) %>%
  summarize(mean_temp = mean(obs_value), n = n())
tempProjMean

# If all sectors have the same mean
fitConst <- lm(obs_value ~ 1, data=tempProjJan1)
# If sectors have different means
fitSect <- lm(obs_value ~ 1 + sector, data=tempProjJan1)
fitDf <- data.frame(sector=tempProjJan1$sector,
                    f1=fitConst$fitted.values,
                    f2=fitSect$fitted.values)
sectTempGraph +
  geom_point(aes(x=sector, y=f1), data=fitDf, color="red") +
  geom_point(aes(x=sector, y=f2), data=fitDf, color="lightgray")

anova(fitConst, fitSect)
```

As the F-statistic is very big and its p-value is very small, we reject the null hypothesis; the six sectors do not have an equal mean temperature (visualized by the red and gray dots being significantly far apart from each other).

Assumptions for a linear hypothesis may be partially confirmed with the following two graphs, which show that the fitted values and residuals show no trends with respect to each other and that the residuals are distributed approximately normally. However, we cannot assume that there are no lurking variables.

```{r checkAssumps}
plot(fitSect, which=1)
plot(fitSect, which=2)
```

### Part 3

Twelve stations are provided below in the `rain_stations` vector, four from each geographical sector northwest, mid-north, and northeast. 

```{r}
rain_stations = c("US1NYMR0018", "US1VAGN0001", "USC00202691", 
                  "US1INWL0002", "US1CODG0062", "US10chey021", 
                  "US1ILKD0024", "US1COAD0135", "USC00263964", 
                  "US1ORCC0003", "US1ORWS0037", "USC00351448")
```

```{r spatialStat12}
rainStatProj <- stationsProj %>%
  filter(station %in% rain_stations)
ggplot(data=stationsProj, mapping=aes(x=long, y=lat)) +
  geom_point(color="gray", alpha=0.75) +
  geom_point(data=rainStatProj, color="indianred", alpha=0.75) +
  theme_bw() +
  labs(x="Longitude", y="Latitude") +
  ggtitle("12 special stations")
```

**To utilize snow data, I compute the total number of snows days and non-snow days for each of these stations and create a table of these values.**

```{r stat12Snow}
snowDays <- proj %>%
  filter(station %in% rain_stations, obs_type == "SNOW") %>%
  group_by(station, long, lat) %>%
  summarize(snow_days = n(), non_snow_days = 366 - snow_days)
snowDays
```

**Then, I perform a hypothesis test to determine if snow days are independently distributed across the three sectors.**

```{r snowDaysTest}
snowDaysSector <- snowDays %>%
  mutate(sector = getSector(long, lat)) %>%
  group_by(sector) %>%
  summarize(snow_days = sum(snow_days), non_snow_days = sum(non_snow_days))
snowDaysSector
chisq.test(select(snowDaysSector, -sector))
```

In this case, our $H_0$ is that snow days are independently distributed across the three sectors; our $H_0$ is that they are dependent. The p-value from our chi-squared test is very small (below 0.05), so we can reject $H_0$ with a confidence level of 99.5%. This suggests that snow days are probabilistically dependent across the sectors.

5. Fitting A Model
-----------------------------

### Transforming Data

Now we want to build a linear model to model variation in the `TMAX` temperature variable in terms of other variables.  

The date in the original data set cannot be used as the number of days per month is not from 0-99; as a result, jumps occur as there are no dates between the end of one month and the start of the next.

The days from January 1 transformation that I applied is not appropriate for linear modeling either, as the relationship is parabolic.

**In order to do this, I identify and apply a transformation to the date variable that is appropriate for fitting a linear model of temperature on date.** 

```{r transformDate}
chosenStations <- c("USC00011084", "USW00094224")

stationTempsByDays <- projDayMonth %>%
  filter(obs_type == "TMAX") %>%
  mutate(days = calcDays(month, day), temp = obs_value/10) %>%
  select(-c(day, month, obs_type, obs_value))

transformedStationTempsByDays <- stationTempsByDays %>%
  mutate(transformed_days = abs(days-200))

head(transformedStationTempsByDays)
```

**To confirm the viability of the transofmration, I plot temperature on the y-axis vs. the transformed date variable on the x-axis for two random stations.**

```{r plotTempDate}
chosenStationTempsByDays <- transformedStationTempsByDays %>%
  filter(station == chosenStations[1] | station == chosenStations[2])

ggplot(data=chosenStationTempsByDays,
       mapping=aes(x=transformed_days, y=temp, color=station)) +
  geom_point(alpha=0.75) +
  theme_bw() +
  scale_color_manual(values=c("indianred", "skyblue"), name="Station") +
  labs(x="Transformed days since Jan 1", y="Temperature (Celsius)") +
  ggtitle("2012 max temperatures")
```

The transformation translates the date into the number of days since January 1, and then finds its distance from day 200 (the peak around which the data seemed to be symmetrically linear).
This is appropriate as the resulting data appears linear, which is better for a linear fit.

### Building a Model

```{r fitTemp}
tempfitAll <- lm(temp ~ transformed_days + lat + long + elevation,
                 data=transformedStationTempsByDays)
summary(tempfitAll)
tidy(tempfitAll)
anova(tempfitAll)
```

The model manages to linearly fit the temperatures by nature of the transformed date variable. Each term is extremely significant, as the F-Statistic results show us: The transformed date is the one that contributes to the fit the most, followed by the latitude, elevation and longitude. However, I test the importance of longtitude next, despite its relative significance to the other variables.

```{r modelComparison}
# Remove longittude (smallest F value/largest p-value)
tempfitNoLong <- lm(temp ~ transformed_days + lat + elevation,
                 data=transformedStationTempsByDays)
summary(tempfitNoLong)
tidy(tempfitNoLong)
anova(tempfitNoLong)

anova(tempfitNoLong, tempfitAll)
```

I would use the first model, as the ANOVA models showed that the elevation model term was significant; it is also worth noting that the R-squared value decreases (the fit is worse) when the term is removed. Removing the longitude variable in the second model leads to a biased outcome because of its importance in the calculation - in terms of selection the data that was picked up was stripped of a significant variable. More importantly however, the response-like bias in the data is strong and should be attributed to longitude, or the lack thereof.

# Session Information  
  
Session information always included for reproducibility!

```{r sessionInformation, cache=FALSE}
sessionInfo()
```
