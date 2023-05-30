
# Create a dataframe to run models explaining home range and time spent foraging
mod_data <- id.year.meta.df %>%
  # Get a factor of id and colony
  mutate(id = as.factor(id), colony = as.factor(colony),
         sex = as.factor(sex),
         # Scale days to between 0 and 1
         days_scaled = (days / max(days)),
         # Get a square root of vessel encounters
         sqrt_enc = sqrt(encounters))


# Run the GLMM
meta_dist_mod <- lmer(data = mod_data,
                      formula = fishing_effort ~
                        days_scaled +
                        sex +
                        areas90 +
                        year_ad + colony +
                        encounters +
                        (1 | id),
                      na.action = na.fail)
# 
summary(meta_dist_mod)

# # Runn a GLMM explaining proportion of time spent in ARS
meta_dist_mod <- gam(data = mod_data,
                    formula = fishing_effort ~
                      days_scaled +
                      sex +
                      areas90 +
                      year_ad +
                      colony +
                      logger +
                      s(sqrt_enc, bs = "ts", k = 5),
                    gamma = 1.4,
                    family = "gaussian",
                    na.action = na.fail)
# 
summary(meta_dist_mod)
plot(meta_dist_mod)
# 
# dredge(meta_dist_mod)
# 
# # Get the model R2
# r.squaredGLMM(meta_dist_mod)
# 
# # Test the residuals of the binomial model, much better
# testResiduals(simulateResiduals(meta_dist_mod, n = 100))
# 
# 
# ggplot(mod_data) +
#   geom_density(aes(x = areas90, fill = colony), alpha = 0.6) +
#   scale_fill_viridis_d() +
#   scale_x_continuous(trans = "sqrt", breaks = c(1, 4, 10, 20)) +
#   facet_wrap(facets = ~colony, ncol = 1) +
#   labs(fill = "Colony", x = "90% Utilisation Distribution (million km2)")

# Create a dataframe to run models explaining home range and time spent foraging
mod_data <- id.year.meta.df %>%
  # Get a factor of id and colony
  mutate(id = as.factor(id), colony = as.factor(colony),
         logger_class = as.factor(logger_class), sex = as.factor(sex),
         # Scale days to between 0 and 1
         days_scaled = (days / max(days)),
         # Get a square root of vessel encounters
         sqrt_enc = sqrt(encounters)) %>%
  # Filter out NAs from the activity data
  filter(!is.na(prop_ars))

ggplot(mod_data) + geom_density(aes(x = prop_ars))

# Runn a GAM explaining proportion of time spent in ARS
meta_ars_mod <- gam(data = mod_data,
                    formula = prop_ars ~
                      areas90 +
                      sex +
                      days_scaled +
                      colony +
                      s(year_ad, bs = "ts", k = 5) +
                      logger_class +
                      s(sqrt_enc, bs = "ts", k = 5, by = colony),
                    family = "gaussian",
                    na.action = na.fail)


meta_ars_mod <- lmer(data = mod_data,
                    formula = prop_ars ~
                      areas90 +
                      colony +
                      year_ad +
                      sex +
                      logger +
                      sqrt_enc +
                      (1 | id),
                    # family = "gaussian",
                    na.action = na.fail)


dredge(meta_ars_mod)

meta_ars_mod <- lmer(data = mod_data,
                     formula = prop_ars ~
                       colony +
                       sex +
                       logger +
                       sqrt_enc +
                       (1 | id),
                     # family = "gaussian",
                     na.action = na.fail)

# Summarise the model
summary(meta_ars_mod)

# Plot the additive effects from this model
plot(meta_ars_mod)
anova(meta_ars_mod)

vif(meta_ars_mod)

# Test the residuals of the binomial model, much better
testResiduals(simulateResiduals(meta_ars_mod, n = 1000))

r.squaredGLMM(meta_ars_mod)

effect_plot(meta_ars_mod, pred = areas90, partial.residuals = T, interval = T)
interactions::cat_plot(meta_ars_mod, pred = colony, modx = colony, 
                       partial.residuals = F, jitter = 0.01, point.alpha = 0.1) +
  scale_colour_viridis_d(option = "C", end = 0.8) +
  labs(x = "", y = "Proportion of mixed behaviour per year")

ars_enc_plot <- effect_plot(meta_ars_mod, sqrt_enc, partial.residuals = T,
            colors = "#130142",
            point.alpha = 0.4,
                    interval = T) +
  theme_light() +
  scale_x_continuous(breaks = c(0, 5, 10, 15),
                     labels = c("0", "25", "100", "225")) +
  labs(x = "Encounters per year", y = "Proportion of mixed behaviour per year")

ggsave(ars_enc_plot, filename = "plots/ars_enc_plot.png",
       width = 5, height = 5, dpi = 500)

# Test the residuals of the binomial model, much better
testResiduals(simulateResiduals(meta_ars_mod, n = 100))

