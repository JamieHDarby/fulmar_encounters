
load("data/cleaned/SEATRACK/enc_df.RData")

mt_df <- enc.df[which(enc.df$logger == "mt"), ]
mt_df

enc.df$logger_class2 <- enc.df$logger_class
enc.df$logger_class2[which(enc.df$logger_class == "bt_high")] <- "BioTrack High"
enc.df$logger_class2[which(enc.df$logger_class == "bt_low")] <- "BioTrack Low"
enc.df$logger_class2[which(enc.df$logger_class == "mt")] <- "Migratech"

enc.df$logger2 <- enc.df$logger
enc.df$logger2[which(enc.df$logger == "lt")] <- "BioTrack"
enc.df$logger2[which(enc.df$logger == "mt")] <- "Migratech"

raw_plot <- 
enc.df %>%# filter(colony == "Skjalfandi") %>%
ggplot() +
  scale_y_continuous(trans = "sqrt") +
  geom_density(aes(x = raw_light, fill = logger_class2)) +
  scale_fill_viridis_d(option = "C", end = 0.7, guide = F) +
  theme_light() +
  theme(strip.text = element_text(colour = "black")) +
  labs(x = "Raw Light", y = "Density") +
  facet_wrap(facets = ~logger_class2, scales = "free_x")

std_plot <- 
enc.df %>%# filter(colony == "Skjalfandi") %>%
ggplot() +
  scale_y_continuous(trans = "sqrt") +
  geom_density(aes(x = std_light, fill = logger2)) +
  scale_fill_viridis_d(option = "C", end = 0.7, guide = F) +
  theme_light() +
  theme(strip.text = element_text(colour = "black")) +
  labs(x = "Standardised light", y = "Density") +
  facet_wrap(facets = ~logger2, scales = "free_x")

std_ad_plot <- 
enc.df %>%# filter(colony == "Skjalfandi") %>%
  ggplot() +
  scale_y_continuous(trans = "sqrt") +
  geom_density(aes(x = std_light_ad, fill = logger2)) +
  scale_fill_viridis_d(option = "C", end = 0.7, guide = F) +
  theme_light() +
  theme(strip.text = element_text(colour = "black")) +
  labs(x = "Standardised adjusted light", y = "Density") +
  facet_wrap(facets = ~logger2, scales = "free_x")

logger_lig_plot <- 
  plot_grid(raw_plot, std_plot, std_ad_plot, nrow = 3)

ggsave(logger_lig_plot, filename = "plots/logger_lig_plot.png",
       height = 7, width = 5, dpi = 600)


enc.df$std_light_ad2 <- enc.df$std_light_ad


mt_df$std_light_ad2 <-
  mt_df$std_light - min(
    mt_df$std_light)

mt_df$std_light_ad2 <- 
  ifelse(mt_df$std_light_ad2 > 0.06, 0.06, 
         mt_df$std_light_ad2)

mt_df$std_light_ad2 <-
  mt_df$std_light_ad2 * (1/0.06)


enc.df[which(enc.df$logger == "mt"), ] <- mt_df

#std_ad_plot <- 
enc.df %>% filter(colony == "Skjalfandi") %>%
  ggplot() +
  geom_density(aes(x = std_light_ad, fill = logger)) +
  scale_fill_viridis_d(option = "B", end = 0.7) +
  facet_wrap(facets = ~logger, scales = "free_x")


enc.df %>% filter(sun.angle < 3, sun.angle > -12) %>%
  .[sample(x = 1:nrow(.), size = 10000), ] %>%
  ggplot(aes(x = sun.angle, y = std_light_ad)) +
  geom_density_2d_filled(aes(x = sun.angle, y = std_light_ad), h = c(2, 0.2),
                         adjust = 3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linewidth = 2, ) +
  geom_vline(aes(xintercept = -6), linewidth = 2) +
  geom_vline(aes(xintercept = -9), linewidth = 2, colour = "dark red") +
  labs(y = "Standardised light", x = "Solar angle (degrees to horizon)",
       fill = "Density") +
  theme(legend.position = "none") +
  facet_wrap(facets = ~logger2, scales = "free_x")

ggsave(filename = "plots/light_test.png",
       dpi = 600, height = 4, width = 8)
