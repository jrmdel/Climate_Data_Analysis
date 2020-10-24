library("FactoMineR")
# library("readr")
library("ggplot2")
library("dplyr")

dataAll <- read.csv("~/Documents/DataGlauss/Climate/Climate_Data_Analysis/data/GlobalTemperatures.csv")
tempAfter1850 <- dataAll[1201:nrow(dataAll),]
tempBefore1850 <- dataAll[1:1200, 1:3]

#test <- tempAfter1850[(nrow(tempAfter1850)-95):nrow(tempAfter1850),]
#test$dt <- as.Date(test$dt)

#p <- ggplot(test, aes(dt, LandAverageTemperature))
#p + geom_pointrange(aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2)) + geom_line()

# Check if a date was in January
#t <- format(test[1,]$dt, "%m") == "01"

tempAfter1850$dt <- as.Date(tempAfter1850$dt)
colIntensity <- function(x, myrange, n=100) round( 1+(x-myrange[1])/diff(myrange) * (n-1))
colFunc <- colorRampPalette(c("yellow", "orange", "red", "black"))
tempRange <- range(tempAfter1850$LandAverageTemperature)


janMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "01")
pjan <- ggplot(janMonth, aes(dt, LandAverageTemperature))
pjan + geom_pointrange(colour=colFunc(100)[colIntensity(janMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))

marMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "03")
pmar <- ggplot(marMonth, aes(dt, LandAverageTemperature))
pmar + geom_pointrange(colour=colFunc(100)[colIntensity(marMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))


julMonth <- tempAfter1850 %>% mutate(month=format(dt,"%m")) %>% filter(month == "07")
pjul <- ggplot(julMonth, aes(dt, LandAverageTemperature))
pjul + geom_pointrange(colour=colFunc(100)[colIntensity(julMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))


# Testing gradient
fun_color_range <- colorRampPalette(c("yellow", "orange", "red", "black"))
my_colors <- fun_color_range(100)
plot(1:100, pch = 16, col = my_colors)