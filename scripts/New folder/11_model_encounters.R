
# Set up data for modelling encounter rate --------------------------------

mod_data <- enc.meta.df %>%
  # Get a factor of id and colony
  mutate(id = as.factor(id),
         logger = as.factor(logger),
         logger_class = as.factor(logger_class),
         colony = as.factor(colony),
         sex = as.factor(sex)) %>%
  # Get rid of handful of NAs in fishing effort values, probably spurious locations
  filter(!is.na(fishing_effort)) %>%
  # Create an encounter boolean variable
  mutate(enc_bool = ifelse(encounters > 0, 1, 0))

# Create ARStart variable to group data into individual years for AR1 structure
mod_data$ARStart <- F
for(i in 2:nrow(mod_data)){
  if(mod_data$year_id[i] != mod_data$year_id[i = 1]){
    mod_data$ARStart[i] <- T
  }
}

# Run the GAMM
system.time(
  enc_mod <- bam(data = mod_data,
             formula = encounters ~
               # s(lon, lat, bs = "ts", k = 20) +
               s(julian, bs = "cc") +
               fishing_effort +
               s(night, bs = "ts") +
               s(id, bs = "re") +
               logger_class +
               sex +
               colony +
               year_ad:colony,
             na.action = na.fail,
             select = T,
             family = poisson(),
             method = "fREML",
             discrete = T,
             AR.start = ARStart,
             rho = 0.2))

# Check the autocorrelation structure
acf(residuals(enc_mod))[1]

# Summarise the model
summary(enc_mod)

# Get some nice plots
plot(getViz(enc_mod))

# Test model residuals, some problems apparent,
# But at least partially due to sample size
testResiduals(simulateResiduals(enc_mod, n = 100))

# Run the GAMM, this time with a binomial response
system.time(
  enc_mod_bin <- bam(data = mod_data,
             formula = enc_bool ~
               # s(lon, lat, bs = "ts", k = 20) +
               s(julian, bs = "cc") +
               fishing_effort +
               s(night, bs = "ts") +
               s(id, bs = "re") +
               logger_class +
               sex +
               colony +
               colony:year_ad,
             na.action = na.fail,
             select = T,
             family = binomial(),
             method = "fREML",
             discrete = T,
             AR.start = ARStart,
             rho = 0.18))

# Check the autocorrelation structure
acf(residuals(enc_mod_bin))[1]

# Summarise the model
summary(enc_mod_bin)

# Get some nice plots
plot(getViz(enc_mod_bin))

# Get chi-squared values for all covariates
anova(enc_mod_bin)

# Predict responses using the fitted binomial model
model_prediction <- 
  as.numeric(predict(enc_mod_bin,
                     mod_data,
                     type = "response")) %>%
  prediction(., mod_data$enc_bool)

#print AUC
print(performance(model_prediction, measure = "auc")@y.values)

# Test the residuals of the binomial model, much better
testResiduals(simulateResiduals(enc_mod_bin, n = 100))

# Model using the meta dataframe of encounters per year -------------------

# Create a dataframe to run models explaining home range and time spent foraging
mod_data <- id.year.meta.df %>%
  # Get a factor of id and colony
  mutate(id = as.factor(id), colony = as.factor(colony),
         # Scale days to between 0 and 1
         days_scaled = (days / max(days)),
         max_dist = max_dist/1000)

# Run the GLMM with an interactive term for colony and year
meta_enc_mod_1 <- glmer(data = mod_data,
                formula = encounters ~
                  areas90 +
                  colony +
                  colony:year_ad +
                  days_scaled +
                  logger_class +
                  sex +
                  (1 | id),
                control = glmerControl(optimizer = "bobyqa"),
                family = "poisson",
                na.action = na.fail)

# Run the GLMM with colony and year separate
meta_enc_mod_2 <- glmer(data = mod_data,
                        formula = encounters ~
                          areas90 +
                          colony +
                          days_scaled +
                          logger_class +
                          sex +
                          (1 | id),
                        control = glmerControl(optimizer = "bobyqa"),
                        family = "poisson", 
                        na.action = na.fail)

# Check which fits better
AIC(meta_enc_mod_1, meta_enc_mod_2)

# Summarise the model
summary(meta_enc_mod_1)

# Dredge to check for best fitting model, turns out to be the full model
dredge(meta_enc_mod_1)

# Check model vifs, all are fine
vif(meta_enc_mod_1)

# Get R2 values for the model
r.squaredGLMM(meta_enc_mod_1)

# Test the residuals of the binomial model, much better
testResiduals(simulateResiduals(meta_enc_mod_1, n = 1000))


# Get repeatability of encounters per year for individuals ----------------

# Run the GLMM using rptR
meta_enc_mod_rpt <- rpt(data = mod_data,
                formula = encounters ~
                  areas90 +
                  colony +
                  colony:year_ad +
                  days_scaled +
                  logger_class +
                  sex +
                  (1 | id),
                grname = "id",
                link = "log",
                parallel = T,
                ncores = 6,
                datatype = "Poisson")

# Summarise the repeatability scores
summary(meta_enc_mod_rpt)
# Plot out the bootstrapped repeatability estimates
plot(meta_enc_mod_rpt)

# Summarise the GLMM within the repeatability model
summary(meta_enc_mod_rpt$mod)
anova(meta_enc_mod_rpt$mod)

r.squaredGLMM(meta_enc_mod_rpt$mod)

relgrad <- with(meta_enc_mod_rpt$mod@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

# Save off this model
save(meta_enc_mod_rpt, file = "data/models/meta_enc_mod_rpt.RData")
load("data/models/meta_enc_mod_rpt.RData")

