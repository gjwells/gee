# https://www.rdocumentation.org/packages/rgee/versions/1.0.7
#remotes::install_github("r-spatial/rgee")
#devtools::install_github("r-spatial/sf")

library(rgee)
# Compute the trend of night-time lights

## Install Python
#ee_install()
# Initialize Earth Engine!
# 4/1AY0e-g58HCHUfQ5h8gXhlQib-BzYtvGESbLyFujH-vcghKJTpM8sDHLJiYw
ee_Initialize()

# ee Interface to main Earth Engine module. Provides access to the top level 
# classes and functions as well as sub-modules (e.g. ee$Image, ee$FeatureCollection$first, etc.).

# function
createTimeBand <-function(img) {
  year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L)
  ee$Image(year)$byte()$addBands(img)
}


collection <- ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
  select('stable_lights')$
  map(createTimeBand)

class(collection)

# use ee linear fit function
col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce) # meta data

# make map
Map$setCenter(9.08203, 47.39835, 3)
Map$addLayer(
  eeObject = col_reduce,
  visParams = list(
    bands = c("scale", "offset", "scale"),
    min = 0,
    max = c(0.18, 20, -0.18)
  ),
  name = "stable lights trend"
)

# 2. Extract precipitation values
library(tidyverse)
library(rgee)
library(sf)

# get shp of North Carolina
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
class(nc)

# generate maps in GEE
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2001-01-01", "2002-01-01")$
  map(function(x) x$reproject("EPSG:4326")$select("pr"))
class(terraclimate)

# extract values
ee_nc_rain <- ee_extract(x = terraclimate, y = nc, sf = FALSE)
class(ee_nc_rain)
colnames(ee_nc_rain)[15:26] <- c(1:12)
ee_nc_rain$name <- nc$NAME
ee_nc_rain <- ee_nc_rain[15:27]

# plot
ee_nc_rain %>%
  pivot_longer(-name, names_to = "month", values_to = "pr") %>%
  ggplot(aes(x = month, y = pr, group = name, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()

