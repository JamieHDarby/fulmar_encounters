

fishing_effort_plot <- 
  effect_plot(enc_mod_bin,
            pred = fishing_effort,
            interval = T,
            int.type = "confidence",
            int.width = 0.95,
            x.label =  expression("Fishing effort" ~ ("hours per km"^2)),
            y.label = "P(Encounter)",
            colors = "#2f7bf5") +
  labs(title = "B") +
  theme_light()


sex_plot <-
  cat_plot(enc_mod_bin,
           pred = sex,
           modx = sex,
           plot.points = F, interval = T) +
  scale_colour_viridis_d(option = "D", end = 0.7) +
  scale_x_discrete(labels = c("Female\n(n = 77)", "Male\n(n = 106)", "Unknown\n(n = 113)")) +
  labs(x = "", title = "C",
       y = "P(Encounter)") +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10))

eynhallow_plot <- 
  interact_plot(enc_mod_bin,
                pred = year_ad,
                modx = colony,
                centered = "all",
                at = list(colony = "Eynhallow"),
                plot.points = F, interval = T,
                main.title = "A",
                int.type = "confidence",
                int.width = 0.95,
                x.label = "",
                y.label = "P(Encounter)") +
  scale_fill_viridis_d(option = "C", begin = 0.2) +
  scale_color_viridis_d(option = "C", begin = 0.2) +
  theme_light() +
  labs(subtitle = "Scotland (Eynhallow, n = 150)") +
  theme(legend.position = "none",
        axis.text.x = element_blank()) +
  geom_vline(aes(xintercept = c(6)), colour = "black") +
  geom_vline(aes(xintercept = c(20.85)), colour = "darkblue") +
  scale_y_continuous(limits = c(0, 0.05)) +
  scale_x_continuous(breaks = c(6, 10, 14, 18),
                     limits = c(6, 21),
                     labels = c("2006", "2010", "2014", "2018"))

saltee_polygon <- data.frame(x = c(6, 6, 9, 9),
                              y = c(0, 0.05, 0.05, 0))

saltee_plot <- 
  interact_plot(enc_mod_bin,
                pred = year_ad,
                modx = colony,
                centered = "all",
                at = list(colony = "Little Saltee"),
                plot.points = F, interval = T,
                main.title = "",
                int.type = "confidence",
                int.width = 0.95,
                x.label = "",
                y.label = "P(Encounter)") +
  scale_fill_viridis_d(option = "C", begin = 0, end = 0.8) +
  scale_color_viridis_d(option = "C", begin = 0, end = 0.8) +
  theme_light() +
  labs(subtitle = "Ireland (Little Saltee, n = 38)") +
  theme(legend.position = "none",
        axis.text.x = element_blank()) +
  geom_vline(aes(xintercept = c(9)), colour = "black") +
  geom_vline(aes(xintercept = c(20.85)), colour = "darkblue") +
  scale_y_continuous(limits = c(0, 0.05)) +
  scale_x_continuous(breaks = c(6, 10, 14, 18),
                     limits = c(6, 21),
                     labels = c("2006", "2010", "2014", "2018"))

skjalfandi_plot <- 
  interact_plot(enc_mod_bin,
                pred = year_ad,
                modx = colony,
                at = list(colony = "Skjalfandi"),
                plot.points = F, interval = T,
                main.title = "",
                int.type = "confidence",
                int.width = 0.95,
                x.label = "",
                y.label = "P(Encounter)") +
  scale_fill_viridis_d(option = "C", begin = 0.4) +
  scale_color_viridis_d(option = "C", begin = 0.4) +
  theme_light() +
  labs(subtitle = "Iceland (Skjalfandi, n = 54)") +
  theme(legend.position = "none",
        axis.text.x = element_blank()) +
  geom_vline(aes(xintercept = c(14)), colour = "black") +
  geom_vline(aes(xintercept = c(20)), colour = "darkblue") +
  scale_y_continuous(limits = c(0, 0.05)) +
  scale_x_continuous(breaks = c(6, 10, 14, 18),
                     limits = c(6, 21),
                     labels = c("2006", "2010", "2014", "2018"))

janmayen_plot <- 
  interact_plot(enc_mod_bin,
                pred = year_ad,
                modx = colony,
                at = list(colony = "Jan Mayen"),
                plot.points = F, interval = T,
                main.title = "",
                int.type = "confidence",
                int.width = 0.95,
                x.label = "Year",
                y.label = "P(Encounter)") +
  scale_fill_viridis_d(option = "C", begin = 0.6) +
  scale_color_viridis_d(option = "C", begin = 0.6) +
  theme_light() +
  labs(subtitle = "Jan Mayen (n = 54)") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 0.05)) +
  geom_vline(aes(xintercept = c(14)), colour = "black") +
  geom_vline(aes(xintercept = c(18)), colour = "darkblue") +
  scale_x_continuous(breaks = c(6, 10, 14, 18),
                     limits = c(6, 21),
                     labels = c("2006", "2010", "2014", "2018"))

colony_plot <- plot_grid(eynhallow_plot, saltee_plot, skjalfandi_plot, janmayen_plot,
          nrow = 4, rel_heights = c(1.2,1,1,1.1))

colony_plot

# covariate_plot <- plot_grid(colony_plot,
#                             plot_grid(fishing_effort_plot, sex_plot,
#                                       nrow = 2),
#                             nrow = 1)

# covariate_plot
# 
# ggsave(covariate_plot, filename = "plots/covariate_plot.png",
#        dpi = 600, height = 7, width = 7)

viz_ob <- getViz(enc_mod_bin)

date_plot <- plot(sm(viz_ob, 1), seWithMean = T) +
  l_fitLine(linetype = 1, linewidth = 1.2)  +
  l_ciPoly(fill = "#162c4d", alpha = 0.3) +
  scale_x_continuous(breaks = c(1, 60, 121, 182, 244, 305),
                     labels = c("January", "March", "May",
                                "July", "September", "November")) +
    scale_colour_viridis_d(option = "C", end = 0.8) +
  theme_light() +
    theme(legend.position = "bottom") +
    labs(colour = "", x = "Month", y = "logit(encounter probability)", title = "D") +
    geom_errorbar(aes(y = -0.85, xmin = 91,
                      xmax = 120, colour = "1. Pre-breeding exodus"),
                  linewidth = 1.5, width = 0.1) +
    geom_errorbar(aes(y = -0.85, xmin = 130,
                      xmax = 230, colour = "2. Breeding season"),
                  linewidth = 1.5, width = 0.1) +
    geom_errorbar(aes(y = -0.85, xmin = 244,
                      xmax = 305, colour = "3. Moult period"),
                  linewidth = 1.5, width = 0.1)

ggsave(filename = "plots/day_plot.png",
       height = 4, width = 7, dpi = 500)

covariate_plot <- plot_grid(plot_grid(colony_plot,
                            plot_grid(fishing_effort_plot, sex_plot,
                                      nrow = 2),
                            nrow = 1),
                            mgcViz::gridPrint(date_plot),
                            nrow = 2, rel_heights = c(1, 0.5))

covariate_plot

ggsave(covariate_plot, filename = "plots/covariate_plot_2b.png",
       dpi = 600, height = 10, width = 7)

night_plot <- 
  plot(sm(viz_ob, 3), seWithMean = T) +
  l_fitLine(linetype = 1)  +
  l_ciPoly(fill = "#162c4d", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  # scale_y_continuous(limits = c(-1, 1.25)) +
  xlab(label = "Length of night as proportion of 24 hours") +
  ylab(label = "Probability of encounter") +
  theme_grey()

plot(pterm(viz_ob, 2))
  
  dummy_model <- meta_enc_mod_rpt$mod

# Plot out the linear effect of year and its interaction with colony
colony_year_plot <-
  interact_plot(meta_enc_mod_rpt$mod,
                pred = year_ad,
                modx = colony,
                # modx.values = c("Eynhallow", "Little Saltee",
                #                 "Skjalfandi"),
                plot.points = F, interval = T,
                partial.residuals = T,
                jitter = 0.05,
                int.width = 0.95,
                facet.modx = F,
                point.size = 1.2,
                point.alpha = 0.5,
                int.type = "confidence",
                x.label = "Year",
                y.label = "Encounters per year",
                legend.main = "") +
  # scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  scale_fill_brewer(palette = "Dark2", labels = c("Scotland", "Jan Mayen",
                                                  "Ireland", "Iceland")) +
  scale_colour_brewer(palette = "Dark2", labels = c("Scotland", "Jan Mayen",
                                                    "Ireland", "Iceland")) +
  # scale_colour_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  scale_x_continuous(breaks = c(6, 10, 14, 18),
                     labels = c("2006", "2010", "2014", "2018")) +
  scale_y_continuous(limits = c(0, 22)) +
  theme_light() +
  scale_linetype_discrete(guide = F) +
  labs(fill = "", colour = "") +
  theme(legend.position = "right")

colony_year_plot

ggsave(colony_year_plot, file = "plots/colony_year_plot.png",
       dpi = 500, height = 5, width = 6)


sex_plot <-
  cat_plot(meta_enc_mod_rpt$mod,
           pred = sex,
           modx = sex,
           plot.points = F, interval = T,
           partial.residuals = T, jitter = 0.2, point.alpha = 0.2) +
  scale_colour_viridis_d(option = "C", end = 0.7) +
  scale_y_continuous(trans = "log", breaks = c(2, 7, 20, 54, 150)) +
  scale_x_discrete(labels = c("Female", "Male", "Unknown")) +
  labs(x = "", title = "A",
       y = "Encounters per year") +
  theme(legend.position = "none")

sex_plot 

area_plot <- 
effect_plot(meta_enc_mod_rpt$mod,
            pred = areas90,
            plot.points = F, interval = T,
            int.width = 0.95,
            partial.residuals = F, jitter = 0.2,
            colors = "#130142") +
  scale_y_continuous(limits = c(1.5, 14)) +
  theme_light() +
  labs(x = expression("90% UD" ~ (paste("million ", km^{2}, sep = ""))),
       y = "Encounters per year", title = "A")

area_plot

colony_plot <- 
  interactions::cat_plot(meta_enc_mod_rpt$mod,
              pred = colony,
              modx = colony,
              plot.points = F, interval = T,
              partial.residuals = T, jitter = 0.2, point.alpha = 0.2) +
  scale_colour_viridis_d(option = "C", end = 0.7) +
  labs(x = "", title = "C",
       y = "Encounters per year") +
theme(legend.position = "none")

colony_plot

area_colony_plot <- plot_grid(area_plot,colony_plot, nrow = 1)

ggsave(area_colony_plot, filename = "plots/area_colony_plot.png",
       dpi = 500, width = 7, height = 4)

sex_area_colony_plot <- plot_grid(sex_plot, area_plot,colony_plot, nrow = 1)

sex_area_colony_plot

ggsave(sex_area_colony_plot, filename = "plots/sex_area_colony_plot.png",
       dpi = 500, width = 8, height = 5)

summary(lm(data = id.year.meta.df, formula = sqrt(areas90) ~ fishing_effort))

area_fishing_plot <- 
  ggplot(id.year.meta.df, aes(x = areas90, y = fishing_effort)) +
  geom_point(aes(colour = sqrt(encounters)), size = 1) +
  scale_colour_viridis_c(breaks = c(0, 5, 10), labels = c(0, 25, 100)) +
  scale_y_continuous(limits = c(0, 30)) +
  scale_x_continuous(trans = "sqrt", breaks = c(0.5, 3, 8, 15)) +
  geom_smooth(method = "lm", linewidth = 1.5, colour = "black", alpha = 0.8) +
  theme_light() +
  labs(x = expression("90% UD" ~ (paste("million ", km^{2}, sep = ""))),
       y = expression("Mean fishing effort " ~ (paste("hours per ", km^{2}, sep = ""))),
       colour = "Encounters\nper year",
       title = "B")

area_comp_plot <- plot_grid(area_plot, area_fishing_plot, nrow = 1, rel_widths = c(1, 1.3))
       
area_comp_plot

ggsave(area_comp_plot, filename = "plots/area_comp_plot.png",
       dpi = 500, width = 7, height = 3.5)

ggplot(id.year.meta.df) +
  geom_boxplot(aes(fill = colony, y = fishing_effort)) +
  facet_wrap(facets = ~year_ad)


