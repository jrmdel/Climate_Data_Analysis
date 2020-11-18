# Import librairies
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Create some high order functions
# For cardinalValue(), if we were to use a simple if(x){...}else{...}, the computation of df columns would not work 
cardinalValue <- function(val, neg){
  valHasNeg <- grepl(neg, val,fixed = TRUE)
  cropped <- as.numeric(gsub("[A-Z]","",val))
  return(if_else(valHasNeg, -1*cropped, cropped))
}
computeLat <- function(lat){
  return(cardinalValue(lat,"S"))
}
computeLon <- function(lon){
  return(cardinalValue(lon,"W"))
}

# Import data
df <- read.csv("~/Documents/DataGlauss/Climate/Climate_Data_Analysis/data/GlobalLandTemperaturesByMajorCity.csv")
df$dt <- as.Date(df$dt)
df <- df %>% mutate(Lat=computeLat(Latitude), Lon=computeLon(Longitude), Year=format(dt,"%Y"))

# Extract unique cities
df_cities <- df[order(df$Country),] %>% distinct(City, Country, Lat, Lon)
countCountries <- df_cities %>% 
  group_by(`Country`) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


dfYear <- df %>% group_by(Year) %>% mutate(AvgYear=mean(AverageTemperature)) %>% distinct(City,Year,AvgYear, .keep_all = TRUE)
dfYearNoNA <- dfYear[ !(is.na(dfYear$AvgYear)) ,c(4:5,8:11)]
dfYearNoNA$Year <- as.numeric(dfYearNoNA$Year)


# PCA(dfYearNoNA, quali.sup=c(1,2))

# res.pca = PCA(df_cities[,-2], quali.sup = c(1))
# plot(res.pca, cex = 0.7)

dftest <- data.frame(Name=c("Abidjan"),Lon=c(5.63),Lat=c(-3.23))


world <- ne_countries(scale = "medium", returnclass = "sf")
# ggplot(data = wo) + geom_sf(fill="antiquewhite") + theme(panel.background = element_rect(fill = "aliceblue"))
theme_set(theme_bw())
ggplot(data = world) + geom_sf(fill="whitesmoke") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = "100 world cities with climate data") +
  theme(panel.background = element_rect(fill = "steelblue4")) +
  geom_point(data=df_cities, aes(x=Lon, y=Lat), colour="red", alpha=0.7) +
  geom_point(data=df_cities, aes(x=Lon, y=Lat), colour="yellow", alpha=0.03, size=20) +
  coord_sf(expand = FALSE)


