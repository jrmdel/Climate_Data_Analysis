# Preliminary study

# Load librairies
library(dplyr)
library(tidyr)
library(ggplot2)

# Import file
dataAll <- read.csv("~/Documents/DataGlauss/Climate/Climate_Data_Analysis/data/GlobalTemperatures.csv")

# Create our computed dataframe
tempAfter1850 <- dataAll[1201:nrow(dataAll),]
tempAfter1850$dt <- as.Date(tempAfter1850$dt)
tempAfter1850 <- tempAfter1850 %>% 
  mutate(Month=format(dt,"%m"), Year=format(dt,"%Y")) %>%
  mutate(Season= if_else(`Month`=="12"|`Month`=="01"|`Month`=="02","Winter",if_else(`Month`=="03"|`Month`=="04"|`Month`=="05","Spring",if_else(`Month`=="06"|`Month`=="07"|`Month`=="08","Summer","Autumn"))))
since1980 <- tempAfter1850 %>% filter(dt >= as.Date("1980-01-01"))
since2000 <- tempAfter1850 %>% filter(dt >= as.Date("2000-01-01"))

# Create the color gradient used for temperatures
colIntensity <- function(x, myrange, n=100) round( 1+(x-myrange[1])/diff(myrange) * (n-1))
colFunc <- colorRampPalette(c("yellow", "orange", "red", "black"))
tempRange <- range(tempAfter1850$LandAverageTemperature)

# Create some high-order functions that will be used later
# From a given dataframe, retrieve the linear regression's coefficients applied to temperatures
getRawCoef <- function(df) {
  linear <- lm(df$LandAverageTemperature ~ df$dt)
  l_range <- range(linear$fitted.values)
  coef <- (l_range[2]-l_range[1])/length(linear$fitted.values)
  return(c(coef,l_range[1]))
}
getYearCoef <- function(df){
  t <- getRawCoef(df)
  return(c(round(12*t[1],digits=4), round(t[2],digits=4)))
}
# Scripting coefficient retrieval for every month of a given dataframe
getCoefPerMonth <- function(df){
  vect = c()
  for (i in c(1:12)) {
    month <- df %>% filter(as.numeric(Month) == i)
    vect[i] <- getYearCoef(month)[1]
  }
  return(vect)
}


# First simple data visualisations
# Everything from 1850 to 2015
ggplot(tempAfter1850, aes(dt, LandAverageTemperature)) +
  geom_pointrange(
    colour=colFunc(100)[colIntensity(tempAfter1850$LandAverageTemperature, tempRange, 100)],
    aes(
      ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2,
      ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2
      )
    ) +
  labs(x="Date time", y="Average Land Temperature (°C)", title = "Overall temperature evolution for each month of every year from 1850 to 2015")

# One segmented view displaying temperatures for each month
ggplot(tempAfter1850, aes(x=dt, y=LandAverageTemperature, group=Month)) +
  geom_point(colour=colFunc(100)[colIntensity(tempAfter1850$LandAverageTemperature, tempRange, 100)]) +
  geom_smooth() + facet_wrap(~`Month`) +
  labs(x="Date time", y="Average Land Temperature (°C)", title="Temperature evolution over time for each month")

# Here, we compute the pace at which temperatures increase every year
t <- tempAfter1850[1:12,] %>% mutate(`1850`=getCoefPerMonth(tempAfter1850)[as.numeric(Month)], `1980`=getCoefPerMonth(since1980)[as.numeric(Month)], `2000`=getCoefPerMonth(since2000)[as.numeric(Month)])
rates <- t[,c(10,12:15)]
gathered <- rates %>% gather(key="Since", value = "Temperature augmentation rate", `1850`, `1980`, `2000`)
ggplot(data=gathered, aes(x=Month, y=`Temperature augmentation rate`, fill=Since)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Mean temperature elevation rate over three periods of time") +
  geom_text(
    aes(label=round(`Temperature augmentation rate`, digits = 2)),
    vjust=1.6, color="white", position = position_dodge(0.9), size=3
    ) +
  scale_fill_manual(values=c("#fec44f","#9ecae1","#02818a"))
