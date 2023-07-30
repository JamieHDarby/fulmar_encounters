
load(file = "data/cleaned/act_pos_ls_2022.RData")

act.pos.ls.new <- act.pos.ls

load(file = "data/cleaned/SEATRACK/act_pos_ls.RData")

act.pos.ls <- c(act.pos.ls, act.pos.ls.new)

# Create empty list
bvr.ls <- list()
bvr.meta.ls <- list()

# Loop through lists and pull out activity data
for(i in 1:length(act.pos.ls)){
  
  # Create df from list element
  df <- act.pos.ls[[i]] %>% filter(deploy == T)
  
  if(nrow(df) > 0){
  # Create behaviour variable
  df$bvr <- 2
  
  # Define rest (1) and flight (3) behaviours. 2 is mixed, i.e. everything else
  df$bvr <- ifelse(df$std_conductivity <= 0.05, 1, df$bvr)
  df$bvr <- ifelse(df$std_conductivity >= 0.95, 3, df$bvr)
  
  # Bind df into the behaviour list
  bvr.ls[[i]] <- df
  
  # Collate daily data from the above
  days.df <- bvr.ls[[i]] %>%
    # Split by day
    split(.$date) %>%
    # Create a function to collect relevent data
    lapply(., function(x) {
      # Date
      date <- x$date[1]
      # Location of points
      lat <- x$lat[1]
      lon <- x$lon[1]
      # ID, colony, distance to colony and sex
      id <- x$individ_id[1]
      colony <- x$colony[1]
      col_lat <- x$col.lat[1]
      col_lon <- x$col.lon[1]
      
      # Proportion of each behaviour per day
      prop_rest <- sum(x$bvr == 1) / nrow(x)
      prop_ars <- sum(x$bvr == 2) / nrow(x)
      prop_flight <- sum(x$bvr == 3) / nrow(x)
      
      prop_wet <- sum(x$std_conductivity) / nrow(x)
      
      # Combine data
      out <- data.frame(id, colony,
                        col_lat, col_lon, 
                        date, lat, lon,
                        prop_rest, prop_ars,
                        prop_flight, prop_wet)
    }) %>%
    # Combine into dataframe
    do.call(rbind, .)
  
  # Bind days.df into the behaviour meta list
  bvr.meta.ls[[i]] <- days.df}
  else{
    bvr.ls[[i]] <- NULL
    bvr.meta.ls[[i]] <- NULL
  }
  
  print(i)
  }

# Bind behaviour data into data frame
bvr.df <- bind_rows(bvr.ls) %>%
  rename(id = individ_id) %>%
  mutate(month = as.numeric(format(date, "%m")),
         col_dist = pointDistance(p1 = data.frame(.$lon, .$lat),
                                  p2 = c(-6.584, 52.138), 
                                  lonlat = T) / 1000,
         year = year(date),
         year_ad = ifelse(month < 7, year - 1, year),
         year_id = paste(id, year_ad, sep = "_"))

# Bind encounter metadata into data frame
bvr.meta.df <- bind_rows(bvr.meta.ls) %>%
  mutate(month = as.numeric(format(date, "%m")),
         year = as.numeric(format(date, "%y")),
         col_dist = pointDistance(p1 = data.frame(.$lon, .$lat),
                                  p2 = c(-6.584, 52.138), 
                                  lonlat = T) / 1000,
         year_ad = ifelse(month < 7, year - 1, year),
         year_id = paste(id, year_ad, sep = "_"))

save(bvr.df, file = "data/cleaned/bvr_df.RData")

save(bvr.meta.df, file = "data/cleaned/bvr_meta_df.RData")

load(file = "data/cleaned/bvr_df.RData")

load(file = "data/cleaned/bvr_meta_df.RData")

# Create empty list
bvr.day.ls <- list()
bvr.day.meta.ls <- list()

# Loop through lists and pull out activity data
for(i in 1:length(act.pos.ls)){
  
  # Create df from list element
  df <- act.pos.ls[[i]] %>% filter(deploy == T, sun.angle > -6)
  
  if(nrow(df) > 0){
    # Create behaviour variable
    df$bvr <- 2
    
    # Define rest (1) and flight (3) behaviours. 2 is mixed, i.e. everything else
    df$bvr <- ifelse(df$std_conductivity <= 0.05, 1, df$bvr)
    df$bvr <- ifelse(df$std_conductivity >= 0.95, 3, df$bvr)
    
    # Bind df into the behaviour list
    bvr.day.ls[[i]] <- df
    
    # Collate daily data from the above
    days.df <- bvr.day.ls[[i]] %>%
      # Split by day
      split(.$date) %>%
      # Create a function to collect relevent data
      lapply(., function(x) {
        # Date
        date <- x$date[1]
        # Location of points
        lat <- x$lat[1]
        lon <- x$lon[1]
        # ID, colony, distance to colony and sex
        id <- x$individ_id[1]
        colony <- x$colony[1]
        col_lat <- x$col.lat[1]
        col_lon <- x$col.lon[1]
        
        # Proportion of each behaviour per day
        prop_rest <- sum(x$bvr == 1) / nrow(x)
        prop_ars <- sum(x$bvr == 2) / nrow(x)
        prop_flight <- sum(x$bvr == 3) / nrow(x)
        
        prop_wet <- sum(x$std_conductivity) / nrow(x)
        
        # Combine data
        out <- data.frame(id, colony,
                          col_lat, col_lon, 
                          date, lat, lon,
                          prop_rest, prop_ars,
                          prop_flight, prop_wet)
      }) %>%
      # Combine into dataframe
      do.call(rbind, .)
    
    # Bind days.df into the behaviour meta list
    bvr.day.meta.ls[[i]] <- days.df}
  else{
    bvr.day.ls[[i]] <- NULL
    bvr.day.meta.ls[[i]] <- NULL
  }
  
  print(i)
}

# Bind behaviour data into data frame
bvr.day.df <- bind_rows(bvr.day.ls) %>%
  rename(id = individ_id) %>%
  mutate(month = as.numeric(format(date, "%m")),
         col_dist = pointDistance(p1 = data.frame(.$lon, .$lat),
                                  p2 = c(-6.584, 52.138), 
                                  lonlat = T) / 1000,
         year = year(date),
         year_ad = ifelse(month < 7, year - 1, year),
         year_id = paste(id, year_ad, sep = "_"))

# Bind encounter metadata into data frame
bvr.day.meta.df <- bind_rows(bvr.day.meta.ls) %>%
  mutate(month = as.numeric(format(date, "%m")),
         year = as.numeric(format(date, "%y")),
         col_dist = pointDistance(p1 = data.frame(.$lon, .$lat),
                                  p2 = c(-6.584, 52.138), 
                                  lonlat = T) / 1000,
         year_ad = ifelse(month < 7, year - 1, year),
         year_id = paste(id, year_ad, sep = "_"))

save(bvr.day.df, file = "data/cleaned/bvr_day_df.RData")

save(bvr.day.meta.df, file = "data/cleaned/bvr_day_meta_df.RData")
