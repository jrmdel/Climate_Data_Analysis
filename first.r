# Load librairies
library("FactoMineR")
library("ggplot2")
library("dplyr")
library("tidyr")

# Import file
dataAll <- read.csv("~/Documents/DataGlauss/Climate/Climate_Data_Analysis/data/GlobalTemperatures.csv")

# Create dataframes
tempAfter1850 <- dataAll[1201:nrow(dataAll),]
tempBefore1850 <- dataAll[1:1200, 1:3]

tempAfter1850$dt <- as.Date(tempAfter1850$dt)
tempAfter1850 <- tempAfter1850 %>% 
  mutate(Month=format(dt,"%m"), Year=format(dt,"%Y")) %>%
  mutate(Season= if_else(`Month`=="12"|`Month`=="01"|`Month`=="02","Winter",if_else(`Month`=="03"|`Month`=="04"|`Month`=="05","Spring",if_else(`Month`=="06"|`Month`=="07"|`Month`=="08","Summer","Autumn"))))
#tempBefore1850$dt <- as.Date(tempBefore1850$dt)

# First Analysis - per month
# Creation of the color gradient
colIntensity <- function(x, myrange, n=100) round( 1+(x-myrange[1])/diff(myrange) * (n-1))
colFunc <- colorRampPalette(c("yellow", "orange", "red", "black"))
tempRange <- range(tempAfter1850$LandAverageTemperature)
# Create high-order function
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
# Scripting coefficient retrieval
getCoefPerMonth <- function(df){
  vect = c()
  for (i in c(1:12)) {
    month <- df %>% filter(as.numeric(Month) == i)
    vect[i] <- getYearCoef(month)[1]
  }
  return(vect)
}

# Every month of every year
p <- ggplot(tempAfter1850, aes(dt, LandAverageTemperature))
p + geom_pointrange(colour=colFunc(100)[colIntensity(tempAfter1850$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))

# Rates
cat("Since 1850, we can estimate that the average global temperature rises at a rate of ",round(getYearCoef(tempAfter1850)[1],digits = 4),"°C per year", sep='')
since1980 <- tempAfter1850 %>% filter(dt >= as.Date("1980-01-01"))
cat("Over the last 40 years, temperatures changed substantially. From 1980 to 2015, temperatures rose by approximately ",round( getYearCoef(since1980)[1] ,digits = 4),"°C every year. This rate is ",round( getYearCoef(since1980)[1]/getYearCoef(tempAfter1850)[1], digits = 4)," times greater than the one witnessed for the period 1850-2015, which means that the last 35 years of that period are greatly responsible for that elevation of temperature.", sep='')

# Every month in one segmented view
pm <- ggplot(tempAfter1850, aes(x=dt, y=LandAverageTemperature, group=Month)) + geom_point(colour=colFunc(100)[colIntensity(tempAfter1850$LandAverageTemperature, tempRange, 100)]) + geom_smooth() + facet_wrap(~`Month`)
pm + labs(x="Years", y="Average Land Temperature (°C)", title="Temperature evolution over time for each month")

# January
janMonth <- tempAfter1850 %>% filter(Month == "01")
pjan <- ggplot(janMonth, aes(dt, LandAverageTemperature)) + geom_pointrange(colour=colFunc(100)[colIntensity(janMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))
pjan + geom_smooth(method = "lm", fill="skyblue") + geom_text(x=as.Date("1875-01-01"), y=4, size=6, label=as.character(as.expression(substitute(y == a %.% x + b,list(a=getYearCoef(janMonth)[1], b=getYearCoef(janMonth)[2])))), parse=TRUE)

# February
febMonth <- tempAfter1850 %>% filter(Month == "02")
pfeb <- ggplot(febMonth, aes(dt, LandAverageTemperature)) + geom_pointrange(colour=colFunc(100)[colIntensity(febMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))
pfeb + geom_smooth(method = "lm", fill="skyblue") + geom_text(x=as.Date("1875-01-01"), y=4.5, size=6, label=as.character(as.expression(substitute(y == a %.% x + b,list(a=getYearCoef(febMonth)[1], b=getYearCoef(febMonth)[2])))), parse=TRUE)

# March
marMonth <- tempAfter1850 %>% filter(Month == "03")
pmar <- ggplot(marMonth, aes(dt, LandAverageTemperature)) + geom_pointrange(colour=colFunc(100)[colIntensity(marMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))
pmar + geom_smooth(method = "lm", fill="skyblue") + geom_text(x=as.Date("1875-01-01"), y=6, size=6, label=as.character(as.expression(substitute(y == a %.% x + b,list(a=getYearCoef(marMonth)[1], b=getYearCoef(marMonth)[2])))), parse=TRUE)

# April
aprMonth <- tempAfter1850 %>% filter(Month == "04")
papr <- ggplot(aprMonth, aes(dt, LandAverageTemperature)) + geom_pointrange(colour=colFunc(100)[colIntensity(aprMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))
papr + geom_smooth(method = "lm", fill="skyblue") + geom_text(x=as.Date("1875-01-01"), y=9.5, size=6, label=as.character(as.expression(substitute(y == a %.% x + b,list(a=getYearCoef(aprMonth)[1], b=getYearCoef(aprMonth)[2])))), parse=TRUE)

# May
mayMonth <- tempAfter1850 %>% filter(Month == "05")
pmay <- ggplot(mayMonth, aes(dt, LandAverageTemperature)) + geom_pointrange(colour=colFunc(100)[colIntensity(mayMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))
pmay + geom_smooth(method = "lm", fill="skyblue") + geom_text(x=as.Date("1875-01-01"), y=12.2, size=6, label=as.character(as.expression(substitute(y == a %.% x + b,list(a=getYearCoef(mayMonth)[1], b=getYearCoef(mayMonth)[2])))), parse=TRUE)

# June
junMonth <- tempAfter1850 %>% filter(strtoi(Month) == 6)
pjun <- ggplot(junMonth, aes(dt, LandAverageTemperature)) + geom_pointrange(colour=colFunc(100)[colIntensity(junMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))
pjun + geom_smooth(method = "lm", fill="skyblue") + geom_text(x=as.Date("1875-01-01"), y=14.2, size=6, label=as.character(as.expression(substitute(y == a %.% x + b,list(a=getYearCoef(junMonth)[1], b=getYearCoef(junMonth)[2])))), parse=TRUE)

# July
julMonth <- tempAfter1850 %>% filter(Month == "07")
pjul <- ggplot(julMonth, aes(dt, LandAverageTemperature))+labs(x="Years", y="Global Average Land Temperature") + geom_pointrange(colour=colFunc(100)[colIntensity(julMonth$LandAverageTemperature, tempRange, 100)],aes(ymin=LandAverageTemperature-LandAverageTemperatureUncertainty/2, ymax=LandAverageTemperature+LandAverageTemperatureUncertainty/2))
pjul + geom_smooth(method = "lm", fill="skyblue") + geom_text(x=as.Date("1875-01-01"), y=15.3, size=6, label=as.character(as.expression(substitute(y == a %.% x + b,list(a=getYearCoef(julMonth)[1], b=getYearCoef(julMonth)[2])))), parse=TRUE)

# Here, we see the pace at which temperatures increase every year
t <- tempAfter1850[1:12,] %>% mutate(`1850`=getCoefPerMonth(tempAfter1850)[as.numeric(Month)], `1980`=getCoefPerMonth(since1980)[as.numeric(Month)])
rates <- t[,c(10,12:14)]



gathered <- rates %>% gather(key="Since", value = "Temperature augmentation rate", `1850`, `1980`)
pr <- ggplot(data=gathered, aes(x=Month, y=`Temperature augmentation rate`, fill=Since)) + geom_bar(stat="identity", position=position_dodge()) + labs(title = "Temperature mean elevation rate over two periods of time")
pr + geom_text(aes(label=round(`Temperature augmentation rate`, digits = 2)), vjust=1.6, color="white", position = position_dodge(0.9), size=3) + scale_fill_manual(values=c("#a6bddb","#2b8cbe"))


# Fisrt MCA
dfPCA <- tempAfter1850[,c(2:3,10:12)] %>% mutate(Year=as.numeric(Year))
res <- PCA(dfPCA[,-3] %>% filter(Season=="Autumn"), quali.sup = c(4))

dfPCA2 <- since1980[,c(2:3,10:12)] %>% mutate(Year=as.numeric(Year))
res <- PCA(dfPCA2 %>% filter(Season=="Winter"), quali.sup = c(3,5), quanti.sup = 2)

# Testing gradient
fun_color_range <- colorRampPalette(c("yellow", "orange", "red", "black"))
my_colors <- fun_color_range(100)
plot(1:100, pch = 16, col = my_colors)