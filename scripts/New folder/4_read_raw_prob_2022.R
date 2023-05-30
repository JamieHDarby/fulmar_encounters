
pos.ls <- list()
act.ls <- list()
lig.ls <- list()
temp.ls <- list()
twls.ls <- list()

filepath <- c("data/PT_2022_Eynhallow/MK3006 B3343 Reconstructed_000",
           "data/PT_2022_Eynhallow/MK3006 B4283 Reconstructed_000",
           "data/PT_2022_Eynhallow/MK3006 B4521 Reconstructed_000",
           "data/PT_2022_Eynhallow/MK3006 B751 Reconstructed_000",
           "data/PT_2022_Eynhallow/B4278_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B4282_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B4498_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B4499_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B4503_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B4507_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B4509_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B4510_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B4524_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B4525_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B6331_2022_MK3006_000",
           "data/PT_2022_Eynhallow/B6333_2022_MK3006_000"
)

logger <-  c("B3343", "B4283",
             "B4521", "B751",
             "B4278", "B4282",
             "B4498", "B4499",
             "B4503", "B4507",
             "B4509", "B4510",
             "B4524", "B4525",
             "B6331", "B6333")

ID <- c("GBT_FP96377", "GBT_FH89128",
        "GBT_FP96356", "GBT_FP96159",
        "GBT_FH89108", "GBT_FH89135",
        "GBT_FH89011", "GBT_FH89099",
        "GBT_FH89125", "GBT_FH89005",
        "GBT_FP96241", "GBT_FP05208",
        "GBT_FP05002", "GBT_FH89126",
        "GBT_FH89115", "GBT_FH89114")

colony <- "Eynhallow"

for(i in 1:length(filepath)){
  
  GLSRaw <- ligTrans(paste(filepath[i], ".lig", sep = ""))

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
  
  act <-
    readAct(file = paste(filepath[i], ".act", sep = "")) %>%
    filter(!is.na(Date)) %>%
    mutate(date_time = ymd_hms(Date),
           date = as_date(Date),
           conductivity = Activity,
           std_conductivity = Activity/max(Activity, na.rm = T),
           individ_id = ID[i]) %>%
    dplyr::select(individ_id, date_time,
                  conductivity, std_conductivity, date)
  
  temp <- readTem2(file = paste(filepath[i], ".tem", sep = "")) %>%
    filter(!is.na(Date)) %>%
    mutate(date_time = ymd_hms(Date),
           date = as_date(Date),
           individ_id = ID[i]) %>%
    dplyr::select(individ_id, date_time,
                  Temp, date)
  
  lig <-
    readLig(file = paste(filepath[i], ".lig", sep = "")) %>%
    filter(!is.na(Date)) %>%
    mutate(date_time = ymd_hms(Date),
           date = as_date(Date),
           raw_light = Light,
           std_light = Light/max(Light, na.rm = T),
           individ_id = ID[i]) %>%
    dplyr::select(individ_id, date_time,
                  raw_light, std_light, date)
  
  act.ls[[i]] <- act
  lig.ls[[i]] <- lig
  temp.ls[[i]] <- temp
}

save(twls.ls, file = "data/cleaned/eyn_twl_ls.RData")
load(file = "data/cleaned/eyn_twl_ls.RData")

system.time(
  for(i in 11:length(act.ls)){
    
    pos <- gls_fun_prob(
      GLSpretwl = twls.ls[[i]],
      act = act.ls[i],
      temp = temp.ls[i],
      colony = colony,
      col.lon = -3.1165,
      col.lat = 59.1421) %>%
      mutate(logger = "mt",
             logger_id = logger[i],
             logger_model = "mk3006",
             individ_id = ID[i],
             date = as.Date(tFirst))
    
    pos.ls[[i]] <- pos
    
    print(i)
  })

eyn.act.ls <- act.ls
eyn.lig.ls <- lig.ls
eyn.temp.ls <- temp.ls
eyn.pos.ls <- pos.ls

eyn.pos.ls <- lapply(eyn.pos.ls, function(x){
  x$lat <- st_coordinates(x$geometry)[, 2]
  x$lon <- st_coordinates(x$geometry)[, 1]
  x
})

for(i in 1:length(ID)){
  eyn.act.ls[[i]]$individ_id <- ID[i]
  eyn.lig.ls[[i]]$individ_id <- ID[i]
  eyn.temp.ls[[i]]$individ_id <- ID[i]
  eyn.pos.ls[[i]]$individ_id <- ID[i]
}

eyn.pos.ls <- lapply(eyn.pos.ls,
                     function(x){x$year <- as.numeric(format(x$dtime, "%Y"))
                     x$month <- as.numeric(format(x$dtime, "%m"))
                     x})

save(eyn.act.ls, file = "data/cleaned/eyn_act_ls.RData")
save(eyn.lig.ls, file = "data/cleaned/eyn_lig_ls.RData")
save(eyn.temp.ls, file = "data/cleaned/eyn_temp_ls.RData")
save(eyn.pos.ls, file = "data/cleaned/eyn_pos_ls.RData")

load("data/cleaned/eyn_act_ls.RData")
load("data/cleaned/eyn_lig_ls.RData")
load("data/cleaned/eyn_temp_ls.RData")
load("data/cleaned/eyn_pos_ls.RData")

load("data/ls_2022/LS_2022_lig_ls.RData")
load("data/ls_2022/LS_2022_act_ls.RData")
load("data/ls_2022/LS_2022_temp_ls.RData")
load("data/ls_2022/LS_2022_pos_ls.RData")

act.ls <- c(act.ls, eyn.act.ls)
lig.ls <- c(lig.ls, eyn.lig.ls)
temp.ls <- c(temp.ls, eyn.temp.ls)
pos.ls <- c(pos.ls, eyn.pos.ls)

pos.df <- bind_rows(pos.ls)

act.pos.ls <- future_lapply(X = act.ls, FUN = var_append, pos.df = pos.df)
lig.pos.ls <- future_lapply(X = lig.ls, FUN = var_append, pos.df = pos.df)
temp.pos.ls <- future_lapply(X = temp.ls, FUN = var_append, pos.df = pos.df)

for(i in 1:length(lig.pos.ls)){
  
  logger_session <- pos.df %>% filter(individ_id == lig.pos.ls[[i]]$individ_id[1])
  
  lig.pos.ls[[i]] <- lig.pos.ls[[i]] %>%
    
    mutate(deploy = ifelse(date_time > min(logger_session$dtime) &
                             date_time < max(logger_session$dtime),
                           T, F),
           logger = "lt",
           logger_spec = "mk3006",
           std_light_ad = std_light)
}

for(i in 1:length(act.pos.ls)){
  
  logger_session <- pos.df %>% filter(individ_id == act.pos.ls[[i]]$individ_id[1])
  
  act.pos.ls[[i]] <- act.pos.ls[[i]] %>%
    
    mutate(deploy = ifelse(date_time > min(logger_session$dtime) &
                             date_time < max(logger_session$dtime),
                           T, F),
           logger = "lt",
           logger_spec = "mk3006")
}

for(i in 1:length(temp.pos.ls)){
  
  logger_session <- pos.df %>% filter(individ_id == temp.pos.ls[[i]]$individ_id[1])
  
  temp.pos.ls[[i]] <- temp.pos.ls[[i]] %>%
    
    mutate(deploy = ifelse(date_time > min(logger_session$dtime) &
                             date_time < max(logger_session$dtime),
                           T, F),
           logger = "lt",
           logger_spec = "mk3006")
}

act_transfer <- function(lgh, act){
  
  require(xts)
  require(zoo)
  
  # Create xts object from tucks
  xts_lig <- xts(x = lgh$std_light_ad, order.by = lgh$date_time)
  
  # Create xts object from activitiy data
  xts_act <- xts(x = act$std_conductivity, order.by = act$date_time)
  
  # Merge these two, initially keeping all time points
  xts_merge <- merge.xts(xts_act, xts_lig, all = T, fill = na.locf)
  
  # Merge this merged xts with the activity xts
  # This time, keep only the values at the activity timestamps
  xts_merge <- merge.xts(xts_lig, xts_merge, all = F, fill = na.locf)
  
  # Put the tuck variable back into the original activity dataframe
  lgh$act <- data.frame(xts_merge)$xts_act
  
  lgh
}

lig.pos.ls <- bind_rows(lig.pos.ls) %>% split(., .$individ_id)
act.pos.ls <- bind_rows(act.pos.ls) %>% split(., .$individ_id)
temp.pos.ls <- bind_rows(temp.pos.ls) %>% split(., .$individ_id)

lig.pos.act.ls <- list()

for(i in 1:length(lig.pos.ls)){
  if(names(lig.pos.ls[i]) %in% names(act.pos.ls)){
    
    index <- which(names(act.pos.ls) == names(lig.pos.ls[i]))
    
    lig.pos.act.ls[[i]] <- act_transfer(lgh = lig.pos.ls[[i]],
                                        act = act.pos.ls[[index]])
    
    lig.pos.act.ls[[i]] <- lig.pos.act.ls[[i]] %>%
      filter(date_time > min(act.pos.ls[[index]]$date_time),
             date_time < max(act.pos.ls[[index]]$date_time))
  }
  else{lig.pos.act.ls[[i]] <- NA}
  print(i)
}

save(lig.pos.act.ls, file = "data/cleaned/lig_pos_act_ls_2022.RData")
save(lig.pos.ls, file = "data/cleaned/lig_pos_ls_2022.RData")
save(act.pos.ls, file = "data/cleaned/act_pos_ls_2022.RData")
save(temp.pos.ls, file = "data/cleaned/temp_pos_ls_2022.RData")
save(pos.ls, file = "data/cleaned/pos_ls_2022.RData")
