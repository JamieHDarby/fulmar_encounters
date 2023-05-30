
# Load in the encounter meta df to use for the locations
load("data/cleaned/SEATRACK/enc_meta_df.RData")

# Plot for Jan Mayen fulmars
jan_mayen_plot <-
  ggplot(enc.meta.df %>% filter(!month %in% c(3, 4, 5, 6, 7, 8),
                              colony == "Jan Mayen")) +
  coord_map(projection = "azequidistant",
            xlim = c(-65, 20),
            ylim = c(40, 90)) +
  stat_density2d_filled(aes(x = lon, y = lat,
                 fill = ..level..),
                 breaks = (seq(0.007, ((0.006)^(1/2)), by = 0.005) ^ 2),
                 contour_var = "density",
                 n = 300,
                 alpha = 0.8) +
  scale_fill_viridis_d(option = "C", ) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#121212',
               fill = '#121212',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             colour = "red", shape = 8, size = 4, stroke = 2) +
  labs( x = "", y = "") +
  theme(legend.position = "none", 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#a1b0c4"),
        panel.grid.major = element_line(colour = "#636c78"),
        strip.text = element_text(size = 14)) +
  facet_wrap(facets = ~colony)

# Plot for Eynhallow fulmars
eynhallow_plot <- 
  ggplot(enc.meta.df %>% filter(!month %in% c(3, 4, 5, 6, 7, 8),
                              colony == "Eynhallow")) +
  coord_map(projection = "azequidistant",
            xlim = c(-65, 20),
            ylim = c(40, 90)) +
  stat_density2d_filled(aes(x = lon, y = lat,
                            fill = ..level..),
                        breaks = (seq(0.006, ((0.014)^(1/2)), by = 0.005) ^ 2),
                        contour_var = "density",
                        n = 200,
                        adjust = 3,
                        alpha = 0.8) +
  scale_fill_viridis_d(option = "C") +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#121212',
               fill = '#121212',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             colour = "red", shape = 8, size = 4, stroke = 2) +
  labs( x = "", y = "") +
  theme(legend.position = "none", 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#a1b0c4"),
        panel.grid.major = element_line(colour = "#636c78"),
        strip.text = element_text(size = 14)) +
  facet_wrap(facets = ~colony)

# Plot for Little Saltee fulmars
saltee_plot <- 
  ggplot(enc.meta.df %>% filter(!month %in% c(3, 4, 5, 6, 7, 8),
                              colony == "Little Saltee")) +
  coord_map(projection = "azequidistant",
            xlim = c(-65, 20),
            ylim = c(40, 90)) +
  stat_density2d_filled(aes(x = lon, y = lat,
                            fill = ..level..),
                        breaks = (seq(0.005, ((0.018)^(1/2)), by = 0.005) ^ 2),
                        contour_var = "density",
                        alpha = 0.8,
                        n = 400,
                        adjust = 3) +
  scale_fill_viridis_d(option = "C") +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#121212',
               fill = '#121212',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             colour = "red", shape = 8, size = 4, stroke = 2) +
  labs( x = "", y = "") +
  theme(legend.position = "none", 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#a1b0c4"),
        panel.grid.major = element_line(colour = "#636c78"),
        strip.text = element_text(size = 14)) +
  facet_wrap(facets = ~colony)

# Plot for Skjalfandi fulmars
skjalfandi_plot <- 
  ggplot(enc.meta.df %>% filter(!month %in% c(3, 4, 5, 6, 7, 8),
                              colony == "Skjalfandi")) +
  coord_map(projection = "azequidistant",
            xlim = c(-65, 20),
            ylim = c(40, 90)) +
  stat_density2d_filled(aes(x = lon, y = lat,
                            fill = ..level..),
                        breaks = (seq(0.005, ((0.0065)^(1/2)), by = 0.005) ^ 2),
                        contour_var = "density",
                        alpha = 0.8,
                        n = 200,
                        adjust = 1) +
  scale_fill_viridis_d(option = "C") +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#121212',
               fill = '#121212',
               size = 0) +
  geom_point(aes(x = col_lon, y = col_lat),
             colour = "red", shape = 8, size = 4, stroke = 2) +
  labs(x = "", y = "") +
  theme(legend.position = "none", 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#a1b0c4"),
        panel.grid.major = element_line(colour = "#636c78"),
        strip.text = element_text(size = 14)) +
  facet_wrap(facets = ~colony)

# skjalfandi_plot

# Combine these together into a grid
big_plot <- plot_grid(skjalfandi_plot, jan_mayen_plot,
                      saltee_plot, eynhallow_plot,
                      rel_widths = c(1, 1, 1, 1))

# Save it off
ggsave(big_plot, filename = "plots/big_plot2.png", dpi = 500,
       width = 10, height = 8)


# Plot for Jan Mayen fulmars
enc_space_plot <-
  enc.meta.df %>% filter(!month %in% c(3, 4, 5, 6, 7, 8)) %>%
  ggplot() +
  coord_map(projection = "fisheye",
            n = 4,
            xlim = c(-62, 27),
            ylim = c(35, 90),
            orientation = c(80, 10, 2)) +
  stat_bin2d(aes(x = lon,
                  y = lat,
                  weight = encounters,
                  alpha = log(after_stat(count) + 0.0001)),
              binwidth = 2) +
  scale_fill_viridis_c(option = "C", trans = "sqrt", breaks = c(10, 200, 600)) +
  scale_alpha_continuous(range = c(0.2, 1), guide = "none") +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#121212',
               fill = '#121212',
               size = 0) +
  labs(x = "", y = "", fill = "Encounters",
       title = "B: Distribution of fulmar encounters") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#a1b0c4"),
        panel.grid.major = element_line(colour = "#636c78"),
        strip.text = element_text(size = 14))

# Plot for Jan Mayen fulmars
pos_space_plot <-
  enc.meta.df %>% filter(!month %in% c(3, 4, 5, 6, 7, 8)) %>%
  ggplot() +
  coord_map(projection = "fisheye",
            n = 4,
            xlim = c(-62, 27),
            ylim = c(35, 90),
            orientation = c(80, 10, 2)) +
  stat_bin2d(aes(x = lon,
                 y = lat,
                 alpha = log(after_stat(count) + 0.0001)),
             binwidth = 2) +
  scale_fill_viridis_c(option = "C", trans = "sqrt", breaks = c(10, 1000, 4000)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#121212',
               fill = '#121212',
               size = 0) +
  labs(x = "", y = "", fill = "Locations", shape= "Colony",
       title = "A: Distribution of all fulmar locations") +
  geom_point(aes(x = col_lon, y = col_lat, shape = colony),
             colour = "#2dc4a3", size = 3, stroke = 2) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#a1b0c4"),
        panel.grid.major = element_line(colour = "#636c78"),
        strip.text = element_text(size = 14))

total_space_plot <- plot_grid(pos_space_plot, enc_space_plot,
                              nrow = 2, align = "hv", axis = "l")

total_space_plot

ggsave(total_space_plot, filename = "plots/space_plot.png", dpi = 500,
       height = 10, width = 7)
