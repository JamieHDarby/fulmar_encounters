
load(file = "data/cleaned/SEATRACK/lig_pos_ls.RData")

load(file = "data/cleaned/SEATRACK/act_pos_ls.RData")

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

save(lig.pos.act.ls, file = "data/cleaned/SEATRACK/lig_pos_act_ls.RData")

