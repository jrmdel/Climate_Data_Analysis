# Climate Data Analysis

The aim of this study is to explore some analysis tools provided by R. We found that dataset on kaggle (https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data) and felt that we could extract interesting information from it, but we have no precise direction to follow. We hope to find something peculiar that makes us investigate a bit more.

## Prerequisites

So far, we intend to work with the language R so we'll need :

- R
- RStudio as an IDE

Plus some useful packages such as :

- FactoMineR
- ggplot2
- sf
...

## Errors that could occur

### Package 'sf'

When installing the package `sf`, the following errors can occur :

> ERROR : dependency 'units' is not available for package 'sf'
> or
> configure: error: gdal-config not found or not executable

To resolve these issues on a linux environment, this command can be executed in the terminal :

> sudo apt-get -y update && sudo apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev

In addition, the package "rgeos" will be necessary, so we want to execute this in our RStudio :

```r
install.packages("rgeos")
```

For more information, see : https://github.com/r-spatial/sf

