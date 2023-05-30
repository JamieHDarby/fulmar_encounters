
load(file = "data/cleaned/pos_ls_2022.RData")

pos.ls.new <- pos.ls

load(file = "data/cleaned/seatrack_pos_ls.RData")

pos.ls <- c(pos.ls, pos.ls.new)

# Little Saltee UDs

ls.spdf <- pos.ls %>%
  
  bind_rows() %>%
  # Filter to only moult locations
  mutate(year_corr = ifelse(month < 7, year - 1, year),
         ID = paste(individ_id, year_corr)) %>%
  # Filter out summer months
  filter(!month %in% c(5, 6, 7, 8),
         colony == "Little Saltee") %>%
  
  split(., .$ID) %>%
  
  lapply(., function(x){if(nrow(x) < 50){x <- NULL}; return(x)}) %>%
  
  bind_rows() %>%
  # Keep useful variables
  dplyr::select(ID, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=52.1392 +lon_0=-6.5916 +x_0=0 +y_0=0 +units=m"))

ls.spdf@data <- data.frame(ls.spdf@data$ID)

table(ls.spdf@data)

UD.all <- kernelUD(xy = ls.spdf)

id <- unlist(gsub(x = names(UD.all), pattern = " .*", replacement = ""))

ls_ud_data <- data.frame(id) %>%
  mutate(areas50 = as.numeric(kernel.area(UD.all, percent = 50, unin = "m", unout = "km2")),
         areas75 = as.numeric(kernel.area(UD.all, percent = 75, unin = "m", unout = "km2")),
         areas90 = as.numeric(kernel.area(UD.all, percent = 90, unin = "m", unout = "km2")),
         areas95 = as.numeric(kernel.area(UD.all, percent = 95, unin = "m", unout = "km2")),
         year = as.numeric(str_sub(names(UD.all), -4, -1)) - 2000,
         year_id = paste(id, year, sep = "_"))

# Eynhallow UDs

ey.spdf <- pos.ls %>%
  
  bind_rows() %>%
  # Filter to only moult locations
  mutate(year_corr = ifelse(month < 7, year - 1, year),
         ID = paste(individ_id, year_corr)) %>%
  # Filter out summer months
  filter(!month %in% c(5, 6, 7, 8),
         colony == "Eynhallow") %>%
  
  split(., .$ID) %>%
  
  lapply(., function(x){if(nrow(x) < 50){x <- NULL}; return(x)}) %>%
  
  bind_rows() %>%
  # Keep useful variables
  dplyr::select(ID, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=59.1421 +lon_0=-3.1165 +x_0=0 +y_0=0 +units=m"))

ey.spdf@data <- data.frame(ey.spdf@data$ID)

table(ey.spdf@data)

UD.all <- kernelUD(xy = ey.spdf)

id <- unlist(gsub(x = names(UD.all), pattern = " .*", replacement = ""))

ey_ud_data <- data.frame(id) %>%
  mutate(areas50 = as.numeric(kernel.area(UD.all, percent = 50, unin = "m", unout = "km2")),
         areas75 = as.numeric(kernel.area(UD.all, percent = 75, unin = "m", unout = "km2")),
         areas90 = as.numeric(kernel.area(UD.all, percent = 90, unin = "m", unout = "km2")),
         areas95 = as.numeric(kernel.area(UD.all, percent = 95, unin = "m", unout = "km2")),
         year = as.numeric(str_sub(names(UD.all), -4, -1)) - 2000,
         year_id = paste(id, year, sep = "_"))

# Skjalfandi UDs

sk.spdf <- pos.ls %>%
  
  bind_rows() %>%
  # Filter to only moult locations
  mutate(year_corr = ifelse(month < 7, year - 1, year),
         ID = paste(individ_id, year_corr)) %>%
  # Filter out summer months
  filter(!month %in% c(5, 6, 7, 8),
         colony == "Skjalfandi") %>%
  
  split(., .$ID) %>%
  
  lapply(., function(x){if(nrow(x) < 50){x <- NULL}; return(x)}) %>%
  
  bind_rows() %>%
  # Keep useful variables
  dplyr::select(ID, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=66.0325 +lon_0=-17.6851 +x_0=0 +y_0=0 +units=m"))

sk.spdf@data <- data.frame(sk.spdf@data$ID)

table(sk.spdf@data)

UD.all <- kernelUD(xy = sk.spdf)

id <- unlist(gsub(x = names(UD.all), pattern = " .*", replacement = ""))

sk_ud_data <- data.frame(id) %>%
  mutate(areas50 = as.numeric(kernel.area(UD.all, percent = 50, unin = "m", unout = "km2")),
         areas75 = as.numeric(kernel.area(UD.all, percent = 75, unin = "m", unout = "km2")),
         areas90 = as.numeric(kernel.area(UD.all, percent = 90, unin = "m", unout = "km2")),
         areas95 = as.numeric(kernel.area(UD.all, percent = 95, unin = "m", unout = "km2")),
         year = as.numeric(str_sub(names(UD.all), -4, -1)) - 2000,
         year_id = paste(id, year, sep = "_"))

# Jan Mayen UDs

jm.spdf <- pos.ls %>%
  
  bind_rows() %>%
  # Filter to only moult locations
  mutate(year_corr = ifelse(month < 7, year - 1, year),
         ID = paste(individ_id, year_corr)) %>%
  # Filter out summer months
  filter(!month %in% c(5, 6, 7, 8),
         colony == "Jan Mayen") %>%
  
  split(., .$ID) %>%
  
  lapply(., function(x){if(nrow(x) < 50){x <- NULL}; return(x)}) %>%
  
  bind_rows() %>%
  # Keep useful variables
  dplyr::select(ID, lon, lat) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = CRS("+init=epsg:4326"),
    data = .) %>%
  # Transform to azemuthal projection in metres for KD estimation
  spTransform(., CRS(
    "+proj=aeqd +lat_0=71.0026 +lon_0=-8.3033 +x_0=0 +y_0=0 +units=m"))

jm.spdf@data <- data.frame(jm.spdf@data$ID)

table(jm.spdf@data)

UD.all <- kernelUD(xy = jm.spdf)

id <- unlist(gsub(x = names(UD.all), pattern = " .*", replacement = ""))

jm_ud_data <- data.frame(id) %>%
  mutate(areas50 = as.numeric(kernel.area(UD.all, percent = 50, unin = "m", unout = "km2")),
         areas75 = as.numeric(kernel.area(UD.all, percent = 75, unin = "m", unout = "km2")),
         areas90 = as.numeric(kernel.area(UD.all, percent = 90, unin = "m", unout = "km2")),
         areas95 = as.numeric(kernel.area(UD.all, percent = 95, unin = "m", unout = "km2")),
         year = as.numeric(str_sub(names(UD.all), -4, -1)) - 2000,
         year_id = paste(id, year, sep = "_"))

ud_data <- rbind(ls_ud_data, ey_ud_data, sk_ud_data, jm_ud_data)

# ud_data$id <- id
# 
# ud_data$year_id <- paste(ud_data$id, ud_data$year, sep = "_")

save(ud_data, file = "data/cleaned/ud_data.RData")
load(file = "data/cleaned/ud_data.RData")
