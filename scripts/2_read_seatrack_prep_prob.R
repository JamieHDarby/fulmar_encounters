
# Import packages required for geolocator processing
# if(!require(GeoLocTools)) install_github("SLisovski/GeoLocTools")
# if(!require(GeoLight)) install_github("SLisovski/GeoLight")

require(GeoLight)
require(GeoLocTools)

# Load geolocation packages
setupGeolocation()

# Load in simple land shapefile for early data exploration
data("wrld_simpl")

# gls_fun -----------------------------------------------------------------

# Function to semi-automate geolcator location processing
gls_fun_prob <- function(GLSpretwl, act, temp, col.lon, col.lat, colony){
  
  # # Removes outliers
  # GLStwl <- twilightEdit(
  #   twilights = GLSpretwl,
  #   offset = 12,
  #   window = 4,
  #   outlier.mins = 45,
  #   stationary.mins = 25,
  #   plot = T)
  
  # Gets rid of outliers
  # GLStwl <- subset(GLStwl, !Deleted)
  
  GLStwl.gl <- export2GeoLight(GLSpretwl)
  
  if(act != "none"){
    
    act <- act[[1]]
  act <- act[, c(4, 5)]
  colnames(act) <- c("dtime","wetdry")
  
  wet.dry.res <- ifelse(max(act$wetdry) == 200, 3,
                        ifelse(max(act$wetdry) == 100, 6, 30))
  }else{act <- NULL; wet.dry.res <- NULL}
  
  
  start <- min(as.Date(GLSpretwl$Twilight))
  end <- max(as.Date(GLSpretwl$Twilight))
  
  if(temp != "none")
  {
    temp <- temp[[1]]
  temp <- temp[, c(4, 7)]
  colnames(temp)<-c("dtime","IntTemp")
  
  temp$IntTemp <- as.numeric(temp$IntTemp)
  
  sst <- sst_deduction(temp$dtime,
                       temp$IntTemp,
                       temp.range = c(min(temp$IntTemp),
                                      max(temp$IntTemp)))
  
  abline(h = c(min(temp$IntTemp),
               max(temp$IntTemp)),
         lty = 2,
         col = "orange")
  
  sst <- sst[sst$SST.remove==F,]
  }else{sst <- NULL}
  
  tw <- twilight_error_estimation(shape = 2.49, scale = 0.94, delay = 0)
  
  pr <- prob_algorithm(trn = GLStwl.gl,
                       sensor = sst, 
                       act = act, 
                       tagging.date = start, 
                       retrieval.date = end, 
                       loess.quartile = 3, 
                       tagging.location = c(col.lon, col.lat), 
                       particle.number = 600, 
                       iteration.number = 60, 
                       sunrise.sd = tw,
                       sunset.sd = tw,
                       range.solar = c(-7,-1), 
                       speed.wet = c(1, 1.3, 5),
                       speed.dry = c(17, 6, 45), 
                       sst.sd = 0.5, 
                       max.sst.diff = 3, 
                       boundary.box = c(-95,90,20,88), 
                       days.around.spring.equinox = c(21,14),   
                       days.around.fall.equinox = c(14,21),
                       ice.conc.cutoff = 0.5, 
                       land.mask = T,
                       med.sea = T,     
                       black.sea = T,
                       baltic.sea = T,
                       caspian.sea = T,
                       east.west.comp = F,
                       wetdry.resolution = wet.dry.res,
                       NOAA.OI.location = "data/SST_data")
  
  plot_map(pr)
  
  path <- as.data.frame(pr$`most probable track`)
  
  # Colony of deployment
  path$colony <- colony
  path$col.lon <- col.lon
  path$col.lat <- col.lat
  
  # Return dataframe
  path
}

