
load(file = "data/cleaned/lig_pos_act_ls_2022.RData")

lig.pos.act.ls.new <- lig.pos.act.ls

load(file = "data/cleaned/SEATRACK/lig_pos_act_ls.RData")

lig.pos.act.ls <- c(lig.pos.act.ls, lig.pos.act.ls.new)

# Create empty list
enc.ls <- list()
enc.meta.ls <- list()

# Loop through lists and pull out encounter data
for(i in c(1:155, 
  157:length(lig.pos.act.ls))){
  
  if(!is.null(lig.pos.act.ls[[i]])){
  # Create dummy df from list element
  df <- lig.pos.act.ls[[i]] %>% filter(abs(pos_date_offset) < 5,
                                   deploy == T)
  
  if(nrow(df) > 0){
  df$logger_class <- ifelse(df$logger_spec %in% c("mk3006", "mk3005",
                                                  "mk15", "mk19",
                                                  "mk3", "mk4", "mk7"), "bt_high",
                            ifelse(df$logger_spec %in% c("mk4093", "mk4083",
                                                        "mk13", "mk14", "mk18"),
                            "bt_low",
                            "mt"))
  
  # Create encounter variable
     df$enc <- ifelse(
    (((df$sun.angle <= -9) &
      (df$std_light_ad > 0.2)) |
       ((df$sun.angle <= -12) &
       (df$std_light_ad > 0.05))),
    1, 0)
  
  # Create an indicator to point to the bout number
  ind <- 0
  
  # Create encounter bout variable
  df$enc_bout <- 0
  
  if(df$logger_class[1] == "mt"){
  # Populate it
    for(j in 8:nrow(df)){
      prop <- sum(df$enc[(j - 7):j])
      if(df$enc[j] == 1 & prop <= 1){
        
        ind <- ind + 1
        
        df$enc_bout[j] <- (ind)}
      
      if(df$enc[j] == 1 & prop >= 2){
        df$enc_bout[j] <- ind}
  }}else{
    for(j in 4:nrow(df)){
      prop <- sum(df$enc[(j - 3):j])
      if(df$enc[j] == 1 & prop <= 1){
        
        ind <- ind + 1
        
        df$enc_bout[j] <- (ind)}
      
      if(df$enc[j] == 1 & prop >= 2){
        df$enc_bout[j] <- ind}
  }}

  # Create a day variable for when the sun is over -9 degrees
  df$day <- ifelse(df$sun.angle > -9, 1, 0)
  
  # Loop through data frame and make sure encounters have "night time" either side
  for(k in 2:(nrow(df) - 1)){
    
    if(df$day[k - 1] == 1 & df$enc[k] == 1){
      df$enc[which(df$enc_bout == df$enc_bout[k])] <- 0
      df$enc_bout[which(df$enc_bout == df$enc_bout[k])] <- 0
    } 
    
    
    if(df$day[k + 1] == 1 & df$enc[k] == 1){
      df$enc[which(df$enc_bout == df$enc_bout[k])] <- 0
      df$enc_bout[which(df$enc_bout == df$enc_bout[k])] <- 0
    } 
  }
  
  # Loop through data frame and make sure encounters are on water
  if(df$logger_class[1] != "mt"){
    for(j in 3:(nrow(df) - 2)){
      
      prop <- sum(df$act[(j - 2):(j + 2)])
      
    if((prop < 0.25) & (df$enc[j] == 1)){
      df$enc[which(df$enc_bout == df$enc_bout[j])] <- 0
      df$enc_bout[which(df$enc_bout == df$enc_bout[j])] <- 0
    }}
    }else{
      for(j in 5:(nrow(df) - 4)){
        
        prop <- sum(df$act[(j - 4):(j + 4)])
      
          if((prop < 0.5) & (df$enc[j] == 1)){
          df$enc[which(df$enc_bout == df$enc_bout[j])] <- 0
          df$enc_bout[which(df$enc_bout == df$enc_bout[j])] <- 0
    }}}
  
  # Send the processed data frame into list element
  enc.ls[[i]] <- df
  
  # Collate daily data from the above
  days.df <- df %>%
    # Split by day
    split(.$date) %>%
    # Create a function to collect relevent data
    lapply(., function(x) {
      # Date
      date <- x$date[1]
      # Proportion of night time per day
      night <- (sum(x$sun.angle <= -9) / nrow(x))
      # Location of points
      lat <- x$lat[1]
      lon <- x$lon[1]
      # ID, colony, distance to colony and sex
      id <- x$individ_id[1]
      colony <- x$colony[1]
      col_lat <- x$col.lat[1]
      col_lon <- x$col.lon[1]
      logger <- x$logger[1]
      logger_spec <- x$logger_spec[1]
      logger_class <- x$logger_class[1]
      pos_date_offset <- x$pos_date_offset[1]
      # Combine data
      out <- data.frame(id, colony, night,
                        col_lat, col_lon, 
                        date, lat, lon, logger, logger_spec,
                        logger_class, pos_date_offset)
    }) %>%
    # Combine into dataframe
    do.call(rbind, .)
  
  # Pick julian day out of the date variable
  days.df$julian <- as.integer(format(days.df$date, "%j"))
  
  # Isolate year out of the date variable, offset by -1/2
  days.df$year <- as.integer(format(as_date(days.df$date), "%y"))
  
  days.df2 <- enc.ls[[i]] %>%
    # Get rid of day time values
    filter(sun.angle <= -9) %>%
    # Split by day
    split(.$date) %>%
    # Create a function to collect relevent data
    lapply(., function(x) {
      # Encounters per day
      encounters <- (length(unique(x$enc_bout)) - 1)
      # Proportion of time spent in encounters per time analysed
      enc_prop <- (sum(x$enc > 0) / nrow(x))
      # Adjusted proportion of time spent in encounters per time analysed
      enc_prop_ad <- ((length(unique(x$enc_bout)) - 1) / nrow(x))
      # Combine data
      out <- data.frame(encounters, enc_prop, enc_prop_ad)
    }) %>%
    # Combine into dataframe
    do.call(rbind, .)
  
  # Transfer these vars over to first df
  vars <- c("encounters", "enc_prop", "enc_prop_ad")
  days.df[, vars] <- 0
  days.df[which(days.df$night > 0), vars] <- days.df2[, vars]

    # Put these above data into the output
  enc.meta.ls[[i]] <- days.df
  }
  else{
    enc.ls[[i]] <- NULL
    enc.meta.ls[[i]] <- NULL
}
    
  # Print progress
  print(i)
}}

# Bind encounter data into data frame
enc.df <- enc.ls %>% bind_rows() %>%
  rename(id = individ_id) %>%
  mutate(month = as.numeric(format(date, "%m")),
         col_dist = pointDistance(p1 = data.frame(.$lon, .$lat),
                                  p2 = data.frame(.$col.lon, .$col.lat), 
                                  lonlat = T,
                                  allpairs = F) / 1000,
         year = year(date),
         year_ad = ifelse(month < 7, year - 1, year),
         year_id = paste(id, year_ad, sep = "_"))

# Append sex to the encounter metadata
sexes <- read.csv(file = "data/sexes/sex_data.csv")

logger_individs <- read.csv(file = "data/SEATRACK/logger_and_individs.csv")

for(i in 1:length(enc.meta.ls)){
  
  if(!is.null(enc.meta.ls[[i]])){
    
  enc.meta.ls[[i]]$sex <- "unknown"
  
  ind_spec <- logger_individs %>%
    filter(logger_individs$individ_id %in% enc.meta.ls[[i]]$id[1])
  
  enc.meta.ls[[i]]$sex <- ind_spec$sex[1]
  
  if(is.na(enc.meta.ls[[i]]$sex[1])){enc.meta.ls[[i]]$sex <- "unknown"}
  
  if(enc.meta.ls[[i]]$id[1] %in% sexes$id)
    enc.meta.ls[[i]]$sex <- sexes[
      (which(sexes$id == enc.meta.ls[[i]]$id[1])), "sex"]
  }}

# Bind encounter metadata into data frame
enc.meta.df <- enc.meta.ls %>%
  bind_rows() %>%
  mutate(month = as.numeric(format(date, "%m")),
         col_dist = pointDistance(p1 = data.frame(.$lon, .$lat),
                                  p2 = data.frame(.$col_lon, .$col_lat), 
                                  lonlat = T,
                                  allpairs = F) / 1000,
         year_ad = ifelse(month < 7, year - 1, year),
         year_id = paste(id, year_ad, sep = "_"))

enc.meta.df$sex[which(enc.meta.df$sex == "F")] <- "female"
enc.meta.df$sex[which(enc.meta.df$sex == "M")] <- "male"

load("data/fisheries/fishing_effort_1deg_corr.RData")

enc.meta.df$fishing_effort <-
  extract(rstr_100_corr,
          enc.meta.df[, c("lon", "lat")],
          method = "bilinear")

save(enc.df, file = "data/cleaned/SEATRACK/enc_df.RData")

save(enc.meta.df, file = "data/cleaned/SEATRACK/enc_meta_df.RData")

load("data/cleaned/SEATRACK/enc_df.RData")

load("data/cleaned/SEATRACK/enc_meta_df.RData")
