
# Set up environment ------------------------------------------------------

# Load in land shapefile for graphs and packages
load(file = "data/shapes/land_df_wgs.RData")
require(zoo)
require(tidyr)
require(lubridate)
require(purrr)
require(raster)
require(dplyr)
require(ggplot2)
require(oce)
require(birk)
require(stringr)
require(future.apply)
require(cowplot)
require(hms)
require(forcats)
require(nlme)
require(mgcv)
require(mgcViz)
require(adehabitatHR)
require(data.table)
require(devtools)
require(maptools)
require(DHARMa)
require(ROCR)
require(rptR)
require(car)
require(lme4)
require(MuMIn)
require(lmerTest)
require(interactions)
require(jtools)

# var_append --------------------------------------------------------------

# Append variables from a location dataframe to a raw light or
# activity dataframe
var_append <- function(x,
                       pos.df,
                       vars = c("colony", "col.lat",
                                "col.lon"),
                       lon.var = "lon",
                       lat.var = "lat"){
  
  pos.df = pos.df[which(pos.df$individ_id == x$individ_id[1]), ]
  
  if(nrow(pos.df) > 0){
  x[, vars] <- pos.df[1, vars]
  
  x <- split(x, x$date) %>%
    lapply(., function(x){
      ind <- which.closest(pos.df$date, x$date[1])[1]
      
      x$pos_date_offset <- 
        as.numeric(
          difftime(time1 = pos.df$date[ind],
                  time2 = x$date[1],
                  units = "days"))
      
      x$lat <-
        pos.df[ind, lat.var]
      
      x$lon <-
        pos.df[ind, lon.var]
      
      x$loc_type <-
        pos.df[ind, "loc_type"]
      
      x$sun.angle <- sunAngle(t = x$date_time, 
                              longitude = x$lon,
                              latitude = x$lat)$altitude
      x
    }) %>%
    
    do.call(rbind, .)}else{x <- NULL}
  
  x
}
