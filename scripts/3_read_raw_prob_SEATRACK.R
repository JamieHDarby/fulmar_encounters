
source("scripts/raw_process/1_prep.R")
source("scripts/raw_process/2_read_seatrack_prep_prob.R")

lig.ls <- readRDS("data/SEATRACK/light_Northern fulmar.rds") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as.Date(date_time)) %>%
  split(., .$session_id)

act.ls <- readRDS("data/SEATRACK/activity_Northern fulmar.rds") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as.Date(date_time)) %>%
  split(., .$session_id)

temp.ls <- readRDS("data/SEATRACK/temperature_Northern fulmar.rds") %>%
  mutate(date_time = ymd_hms(date_time),
         date = as.Date(date_time)) %>%
  split(., .$session_id)

# Load data
pos.df <- readRDS("data/SEATRACK/SEATRACK_glsIRMAloc_FUGLA_20220307_v2.3_JamieDarby.rds") %>%
  mutate(id = gsub("-", "_", individ_id),
         month = as.numeric(format(timestamp, format = "%m")),
         year = as.numeric(format(timestamp, format = "%Y")),
         year_corr = ifelse(month < 7, year - 1, year),
         date = as.Date(timestamp)) %>%
  dplyr::select(-species, -individ_id) %>%
  as.data.frame()

pos.df$col.lat <- ifelse(grepl("Little Saltee", pos.df$colony), 52.1392,
                         ifelse(grepl("Eynhallow", pos.df$colony), 59.1421,
                                ifelse(grepl("Jan Mayen", pos.df$colony), 71.0026, 66.0325)))

pos.df$col.lon <- ifelse(grepl("Little Saltee", pos.df$colony), -6.5916,
                         ifelse(grepl("Eynhallow", pos.df$colony), -3.1165,
                                ifelse(grepl("Jan Mayen", pos.df$colony), -8.3033, -17.6851)))

lig.ls <- lapply(lig.ls, function(x){
  df <- pos.df[which(pos.df$id == x$individ_id[1]), ]
  vars <- c("colony", "col.lon", "col.lat")
  x[, vars] <- df[1, vars]
  x
})

save(pos.df, file = "data/cleaned/seatrack_pos_df.RData")
save(act.ls, file = "data/cleaned/seatrack_act_ls.RData")
save(lig.ls, file = "data/cleaned/seatrack_lig_ls.RData")
save(temp.ls, file = "data/cleaned/seatrack_temp_ls.RData")

load("data/cleaned/seatrack_pos_df.RData")
load("data/cleaned/seatrack_act_ls.RData")
load("data/cleaned/seatrack_lig_ls.RData")
load("data/cleaned/seatrack_temp_ls.RData")
# 
# pos.ls <- list()
# twls.ls <- list()

load("data/cleaned/seatrack_pos_ls.RData")
load(file = "data/cleaned/seatrack_twl_ls.RData")

for(i in 525){
  
  lig.ls[[i]] <- lig.ls[[i]][order(lig.ls[[i]]$date_time),]
  
  GLSRaw <- lig.ls[[i]][, c("date_time", "raw_light")]

  # Set names for raw data
  names(GLSRaw) <- c("Date", "Light")

  # Sort out NAs
  GLSRaw <- GLSRaw[which(!is.na(GLSRaw$Date)), ]

  # Log transform light data
  GLSRaw$Light  <-
    (log(GLSRaw$Light + 0.0001) +
       abs(min(log(GLSRaw$Light + 0.0001))))

  # Make sure plotting window is setup
  par(mar=c(1,1,1,1))

  # Launches UI for selecting data to process
  twls.ls[[i]] <- preprocessLight(
    GLSRaw,
    threshold = 2.5,
    dark.min = 60,
    offset = 12,
    lmax = 20,
    gr.Device = "x11")
}

save(twls.ls, file = "data/cleaned/seatrack_twl_ls.RData")

lig.ls[[433]]$colony <- "Jan Mayen"
lig.ls[[433]]$col.lat <- 71.0026
lig.ls[[433]]$col.lon <- -8.3033
lig.ls[[446]]$colony <- "Jan Mayen"
lig.ls[[446]]$col.lat <- 71.0026
lig.ls[[446]]$col.lon <- -8.3033
lig.ls[[454]]$colony <- "Eynhallow"
lig.ls[[454]]$col.lat <- 59.1421
lig.ls[[454]]$col.lon <- -3.1165

system.time(
  for(i in 1:length(twls.ls)){

    pos <- gls_fun_prob(
      GLSpretwl = twls.ls[[i]],
      act = ifelse(names(lig.ls[i]) %in% names(act.ls),
                   act.ls[which(names(act.ls) == names(lig.ls[i]))], "none"),
      temp = ifelse(names(lig.ls[i]) %in% names(temp.ls),
        temp.ls[which(names(temp.ls) == names(lig.ls[i]))], "none"),
      colony = lig.ls[[i]]$colony[1],
      col.lon = lig.ls[[i]]$col.lon[1],
      col.lat = lig.ls[[i]]$col.lat[1]) %>%
      mutate(individ_id = lig.ls[[i]]$individ_id[1],
             date = as.Date(dtime))

    pos.ls[[i]] <- pos

    print(i)
  })

save(pos.ls, file = "data/cleaned/seatrack_pos_ls.RData")

require(foreach)
require(doParallel)

registerDoParallel(6)

loop_ind <- c(371:400)

system.time(
  pos.ls.2 <- foreach(i = loop_ind, .verbose = TRUE) %dopar% {
    
    require(dplyr)
    require(TwGeos)
    require(lubridate)
    require(probGLS)
    
    pos <- gls_fun_prob(
      GLSpretwl = twls.ls[[i]],
      act = ifelse(names(lig.ls[i]) %in% names(act.ls),
                   act.ls[which(names(act.ls) == names(lig.ls[i]))], "none"),
      temp = ifelse(names(lig.ls[i]) %in% names(temp.ls),
                    temp.ls[which(names(temp.ls) == names(lig.ls[i]))], "none"),
      colony = lig.ls[[i]]$colony[1],
      col.lon = lig.ls[[i]]$col.lon[1],
      col.lat = lig.ls[[i]]$col.lat[1]) %>%
      mutate(individ_id = lig.ls[[i]]$individ_id[1],
             date = as.Date(dtime))
    
    pos
  })

stopImplicitCluster()

pos.ls[loop_ind] <- pos.ls.2

save(pos.ls, file = "data/cleaned/seatrack_pos_ls.RData")

load("data/cleaned/seatrack_pos_ls.RData")

pos.df.2 <- bind_rows(pos.ls)

plan(multisession)

act.pos.ls <- future_lapply(X = act.ls, FUN = var_append, pos.df = pos.df.2)
lig.pos.ls <- future_lapply(X = lig.ls, FUN = var_append, pos.df = pos.df.2)
temp.pos.ls <- future_lapply(X = temp.ls, FUN = var_append, pos.df = pos.df.2)

plan(sequential)

for(i in 1:length(lig.pos.ls)){
  if(max(lig.pos.ls[[i]]$raw_light, na.rm = T) > 65){
    lig.pos.ls[[i]]$logger <- "mt"}else{
      lig.pos.ls[[i]]$logger <- "lt"
    }
    
    lig.pos.ls[[i]]$std_light_ad <- rep(NA, nrow(lig.pos.ls[[i]]))
    
    if(lig.pos.ls[[i]]$logger[1] == "mt"){
      
      lig.pos.ls[[i]]$std_light_ad <-
        lig.pos.ls[[i]]$std_light - min(lig.pos.ls[[i]]$std_light)
      
      lig.pos.ls[[i]]$std_light_ad <- 
        ifelse(lig.pos.ls[[i]]$std_light_ad > 0.1, 0.1, lig.pos.ls[[i]]$std_light_ad)
      
      lig.pos.ls[[i]]$std_light_ad <-
        lig.pos.ls[[i]]$std_light_ad * 10
    }else{
      lig.pos.ls[[i]]$std_light_ad <- lig.pos.ls[[i]]$std_light
    }
    print(i)
  }

logger_individs <- read.csv(file = "data/SEATRACK/logger_and_individs.csv")

for(i in 1:length(act.pos.ls)){
  logger_session <- logger_individs %>% filter(session_id == act.pos.ls[[i]]$session_id[1])
  
  act.pos.ls[[i]]$logger_spec <- logger_session$logger_model[1]
  
  act.pos.ls[[i]] <- act.pos.ls[[i]] %>%
    mutate(deploy = ifelse(date_time > ymd(logger_session$deployment_date[1]) &
           date_time < ymd(logger_session$retrieval_date[1]),
           T, F))
}

for(i in 1:length(lig.pos.ls)){
  logger_session <- logger_individs %>% filter(session_id == lig.pos.ls[[i]]$session_id[1])
  
  lig.pos.ls[[i]]$logger_spec <- logger_session$logger_model[1]
  
  lig.pos.ls[[i]] <- lig.pos.ls[[i]] %>%
    mutate(deploy = ifelse(date_time > ymd(logger_session$deployment_date[1]) &
                              date_time < ymd(logger_session$retrieval_date[1]),
                            T, F))
}

for(i in 1:length(temp.pos.ls)){
  logger_session <- logger_individs %>% filter(session_id == temp.pos.ls[[i]]$session_id[1])
  
  temp.pos.ls[[i]]$logger_spec <- logger_session$logger_model[1]
  
  temp.pos.ls[[i]] <- temp.pos.ls[[i]] %>%
    mutate(deploy = ifelse(date_time > ymd(logger_session$deployment_date[1]) &
                              date_time < ymd(logger_session$retrieval_date[1]),
                            T, F))
}

save(act.pos.ls, file = "data/cleaned/SEATRACK/act_pos_ls.RData")
save(lig.pos.ls, file = "data/cleaned/SEATRACK/lig_pos_ls.RData")
save(temp.pos.ls, file = "data/cleaned/SEATRACK/temp_pos_ls.RData")

# space_plot <- 
  ggplot(pos.df) +
  # filter(colony != "Eynhallow")) +
  # filter(ID == "ISR-340800")) +
  coord_map(projection = "azequidistant",
            xlim = c(-80, 30),
            ylim = c(30, 90)) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#262624',
               fill = '#262624',
               size = 0) +
  geom_path(aes(x = lon, y = lat, colour = id), alpha = 0.7, size = 1) +
  labs(fill = "", colour = "Logger ID", x = "Longitude", y = "Latitude") +
  theme_light() +
  theme(legend.position = "none")
