# Load librairies
#library("FactoMineR")
library("ggplot2")
library("dplyr")

# Import file
dataAll <- read.csv("~/Documents/DataGlauss/Climate/Climate_Data_Analysis/data/GlobalTemperatures.csv")

# Create dataframes
tempAfter1850 <- dataAll[1201:nrow(dataAll),]
tempBefore1850 <- dataAll[1:1200, 1:3]

tempAfter1850$dt <- as.Date(tempAfter1850$dt)
#tempBefore1850$dt <- as.Date(tempBefore1850$dt)

# First Analysis - per month
# Creation of the color gradient
colIntensity <- function(x, myrange, n=100) round( 1+(x-myrange[1])/diff(myrange) * (n-1))
colFunc <- colorRampPalette(c("yellow", "orange", "red", "black"))
tempRange <- range(tempAfter1850$LandAverageTemperature)

# Every month
p <- ggplot(tempAfter1850, aes(dt, LandAverageTemperature))
p + geom_pointrange(colour=colFunc(100)[colIntensity(tempAfter1850$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))


# January
janMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "01")
pjan <- ggplot(janMonth, aes(dt, LandAverageTemperature))
pjan + geom_pointrange(colour=colFunc(100)[colIntensity(janMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))

# February
febMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "02")
pfeb <- ggplot(febMonth, aes(dt, LandAverageTemperature))
pfeb + geom_pointrange(colour=colFunc(100)[colIntensity(febMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))

# March
marMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "03")
pmar <- ggplot(marMonth, aes(dt, LandAverageTemperature))
pmar + geom_pointrange(colour=colFunc(100)[colIntensity(marMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))

# April
aprMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "04")
papr <- ggplot(aprMonth, aes(dt, LandAverageTemperature))
papr + geom_pointrange(colour=colFunc(100)[colIntensity(aprMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))

# May
mayMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "05")
pmay <- ggplot(mayMonth, aes(dt, LandAverageTemperature))
pmay + geom_pointrange(colour=colFunc(100)[colIntensity(mayMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))

# June
junMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "06")
pjun <- ggplot(junMonth, aes(dt, LandAverageTemperature))
pjun + geom_pointrange(colour=colFunc(100)[colIntensity(junMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))

# July
julMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "07")
pjul <- ggplot(julMonth, aes(dt, LandAverageTemperature))+labs(x="Years", y="Global Average Land Temperature")
pjul + geom_pointrange(colour=colFunc(100)[colIntensity(julMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))


# Testing gradient
fun_color_range <- colorRampPalette(c("yellow", "orange", "red", "black"))
my_colors <- fun_color_range(100)
plot(1:100, pch = 16, col = my_colors)