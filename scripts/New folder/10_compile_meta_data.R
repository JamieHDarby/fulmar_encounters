
load(file = "data/cleaned/ud_data.RData")
load(file = "data/cleaned/bvr_meta_df.RData")
load(file = "data/cleaned/SEATRACK/enc_meta_df.RData")
source("scripts/seatrack_process/1_prep.R")

# Check number of individuals from each colony
enc.meta.df %>%
  split(., .$id) %>%
lapply(., function(x){x <- x[1, "colony"]}) %>%
unlist(.) %>%
  length()

# Check how many years of data are available for each colony
enc.meta.df %>%
  split(., .$year_id) %>%
  lapply(., function(x){x <- x[1, "colony"]}) %>%
  unlist(.) %>%
  table()

# Check how many years of data are available for each colony
enc.meta.df %>%
  split(., .$id) %>%
  lapply(., function(x){x <- x[1, "sex"]}) %>%
  unlist(.) %>%
  length()

# Check what calendar years we have data for
enc.meta.df %>% split(., .$colony) %>%
  lapply(., function(x){unique(x$year)})

id.year.meta.df <- split(enc.meta.df, enc.meta.df$year_id) %>%
  
  lapply(., function(x){
    
    x <- x[which(!x$month %in% c(5, 6, 7, 8)), ]
    
    vars <- c("id", "year_id", "year_ad", "colony",
              "logger", "logger_spec", "logger_class", "sex")
    
    y <- x[1, vars]
    
    y$days <- nrow(x)
    
    y$encounters <- sum(x$encounters)
    y$enc_prop <- mean(x$enc_prop)
    y$enc_prop_ad <- mean(x$enc_prop_ad)
    
    y$fishing_effort <- mean(x$fishing_effort, na.rm = T)
    
    y$max_dist <- max(x$col_dist)
    
    if(y$days < 150){y <- NULL}
    
    y
  }) %>% bind_rows()

id.year.bvr.df <- split(bvr.meta.df, bvr.meta.df$year_id) %>%
  
  lapply(., function(x){
    
    x <- x[which(!x$month %in% c(5, 6, 7, 8)), ]
    
    vars <- c("id", "year_id", "year_ad", "colony")
    
    y <- x[1, vars]
    
    y$days <- nrow(x)
    
    y$prop_rest <- mean(x$prop_rest)
    y$prop_ars <- mean(x$prop_ars)
    y$prop_flight <- mean(x$prop_flight)
    y$prop_wet <- mean(x$prop_wet)
    
    y$max_dist <- max(x$col_dist)
    
    if(y$days < 150){y <- NULL}
    
    y
  }) %>% bind_rows()


for(i in 1:nrow(id.year.meta.df)){
  
  vars <- c("areas50", "areas75", "areas90", "areas95")
  
  if(id.year.meta.df$year_id[i] %in% ud_data$year_id){
    
    id.year.meta.df[i, vars] <-
      ud_data[which(ud_data$year_id == id.year.meta.df[i, "year_id"]), vars] / 1000000
    
  }else{id.year.meta.df[i, vars] <- NA}
  
  vars <- c("prop_rest", "prop_ars", "prop_flight", "prop_wet")
  
  if(id.year.meta.df$year_id[i] %in% id.year.bvr.df$year_id){
    
  id.year.meta.df[i, vars] <-
    id.year.bvr.df[
      which(id.year.bvr.df$year_id == id.year.meta.df[i, "year_id"]), vars]
  
  }else{id.year.meta.df[i, vars] <- NA}
}

id.year.meta.df %>%
  mutate(year = as.factor(year_ad + 2000)) %>%
  ggplot() +
  geom_boxplot(aes(y = enc_prop * 100, x = colony, fill = sex),
               alpha = 0.6, varwidth = T) +
  labs(x = "Period", y = "% total night time spent attending vessels",
       fill = "Year")

ggplot(id.year.bvr.df %>% mutate(year = as.factor(year_ad + 2000))) +
  geom_boxplot(aes(y = prop_wet, x = year, fill = colony),
               alpha = 0.6, varwidth = T)

save(id.year.meta.df, file = "data/cleaned/id_year_meta.RData")
load(file = "data/cleaned/id_year_meta.RData")
