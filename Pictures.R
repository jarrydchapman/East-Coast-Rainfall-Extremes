library(tidyverse)
library(ozmaps)
library(sf)
library(zoo)
library(RColorBrewer)
library(colorspace)
library(ggpubr)
library(gridExtra)

setwd("~/Documents/CLEX Summer Research/East Coast")
coords <- readRDS("dataframe.rds")
coords <- coords %>%
  filter(land == TRUE)

ec <- ozmap("states")  %>%
  filter(NAME == "Queensland" | NAME == "New South Wales")


towns <- data.frame(location = c("Bundaberg",
                                 "Sunshine Coast",
                                 "Brisbane",
                                 "Gold Coast",
                                 "Newcastle",
                                 "Sydney",
                                 "Wollongong"),
                    latitude = c(-24.8661,
                                 -26.6500,
                                 -27.4705,
                                 -28.0167,
                                 -32.9283,
                                 -33.7507,
                                 -34.4248),
                    longitude = c(152.3489,
                                  153.0667,
                                  153.0260,
                                  153.4000,
                                  151.7817,
                                  151.2093,
                                  150.8931))

mapplot <- function(region, fits, voi) {
  ggplot(data = region) +
    geom_rect(data = fits,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = voi)) +
    geom_sf(alpha = 0.01, colour = "black") +
    #geom_point(data = locations, aes(x = longitude, y = latitude, colour = location), shape = 15) +
    scale_x_continuous(limits = c(150.5, 153.75), breaks = seq(151, 153, 1)) +
    scale_y_continuous(limits = c(-34.5, -24.75), breaks = seq(-34, -24, 2)) +
    labs(x = "Longitude",
         y = "Latitude",
         colour = "City/Town") +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))
}


setwd("~/Documents/CLEX Summer Research/East Coast/Outputs/Plots")

# Threshold Map
thresh_plot <- mapplot(ec, coords, coords$threshold) +
  scale_fill_continuous_divergingx(palette = "RdBu", mid = mean(coords$threshold),
                                   limits = c(min(coords$threshold), max(coords$threshold))) +
  labs(fill = "Threshold (mm)")

thresh_plot

saveRDS(thresh_plot, "thresh_plot.rds")

# Count of Extremes
ext_plot <- mapplot(ec, coords, coords$ext) +
  scale_fill_continuous_divergingx(palette = "RdBu", mid = mean(coords$ext),
                                   limits = c(min(coords$ext), max(coords$ext))) +
  labs(fill = "# of Extremes")

ext_plot

saveRDS(ext_plot, "ext_plot.rds")

# Count of Declustered Extremes
dc_plot <- mapplot(ec, coords, coords$dc) +
  scale_fill_continuous_divergingx(palette = "RdBu", mid = mean(coords$dc),
                                   limits = c(min(coords$dc), max(coords$dc))) +
  labs(fill = "# of Extremes")

dc_plot

saveRDS(dc_plot, "dc_plot.rds")

# Percentage of Extremes lost to Declustering
extpc_plot <- mapplot(ec, coords, coords$extpc) +
  scale_fill_continuous_divergingx(palette = "RdBu", mid = mean(coords$extpc),
                                   limits = c(min(coords$extpc), max(coords$extpc))) +
  labs(fill = "Proportion")

extpc_plot

saveRDS(extpc_plot, "extpc_plot.rds")



# HPP Plot
hp_plot <- mapplot(ec, coords, coords$hp90) +
  scale_fill_continuous_divergingx(palette = "RdBu", mid = mean(coords$hp90),
                                   limits = c(min(coords$hp90), max(coords$hp90))) +
  labs(fill = expression("Intensity" ~ (lambda)))

hp_plot
saveRDS(hp_plot, "hp90_plot.rds")

hpprob_plot <- mapplot(ec, coords, coords$hp90prob) +
  scale_fill_continuous_divergingx(palette = "RdBu", mid = mean(coords$hp90prob),
                                   limits = c(min(coords$hp90prob), max(coords$hp90prob))) +
  labs(fill = "Probability")

hpprob_plot
saveRDS(hpprob_plot, "hp90_prob_plot.rds")


# Harmonic Plot

## The idea is to put all the variables we want to facet by into long form
## and keep the rest as identifiers. That means taking the seasons as the things
## to make long, and keeping the boxes as identifiers.
coords_long <- coords %>%
  select(
    summer, autumn, winter, spring,
    xmin, xmax, ymin, ymax
  ) %>%
  pivot_longer(!c(xmin, xmax, ymin, ymax), names_to = "season") %>%
  ## Convenience to make the seasons look nice
  mutate(season = str_to_sentence(season)) %>%
  ## Default is alphabetical ordering
  mutate(season = factor(season, levels=c("Summer", "Autumn", "Winter", "Spring")))

seasonal_plot <- ggplot(ec) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = value), data = coords_long) +
  geom_sf(alpha = 0.01, colour="black") +
  scale_x_continuous(limits = c(150.5, 153.75), breaks = seq(151, 153, 1)) +
  scale_y_continuous(limits = c(-34.5, -24.75), breaks = seq(-34, -24, 2)) +
  labs(
    x = "Longitude",
    y = "Latitude",
    colour = "City/Town",
    fill = expression("Intensity"~(lambda))
  ) +
  facet_wrap(~season, nrow = 1) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0.325) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

seasonal_plot

saveRDS(seasonal_plot, "seasonal_plot.rds")


doy_plot <- ggplot(harm_doy, aes(x = doy, y = intensity)) +
  geom_line() +
  labs(title = "Harmonic Intensity Function for Eastern Australia",
       x = "Day of the Year",
       y = "Intensity") +
  theme_bw()

doy_plot


coords_long <- coords %>%
  select(
    xmin, xmax, ymin, ymax,
    soi_95, dmi_95, anom_95
  ) %>%
  pivot_longer(!c(xmin, xmax, ymin, ymax)) %>%
  mutate(name = factor(name, levels = c("soi_95", "dmi_95", "anom_95"), labels = c("SOI", "DMI", "Temperature Anomaly")))

lt_plot95 <- ggplot(ec) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = value), data = coords_long) +
  geom_sf(alpha = 0.01, colour="black") +
  scale_x_continuous(limits = c(150.5, 153.75), breaks = seq(151, 153, 1)) +
  scale_y_continuous(limits = c(-34.5, -24.75), breaks = seq(-34, -24, 2)) +
  labs(
    x = "Longitude",
    y = "Latitude",
    colour = "City/Town",
    fill = expression("%" * Delta ~ lambda)
  ) +
  facet_wrap(~name, nrow = 1) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

lt_plot95


saveRDS(lt_plot95, "lt_plot95.rds")
#saveRDS(lt_plot99, "lt_plot99.rds")

## ST
mslp_plot <- mapplot(ec, coords, coords$mslp_99) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.65)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "MSLP")
mslp_plot



u250_plot <- mapplot(ec, coords, coords$u250_99) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.65)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "U250")  
u250_plot

u850_plot <- mapplot(ec, coords, coords$u850_99) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.65)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "U850")
u850_plot

v250_plot <- mapplot(ec, coords, coords$v250_99) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.65)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "V250")
v250_plot

v850_plot <- mapplot(ec, coords, coords$v850_99) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.65)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "V850")
v850_plot

dew_plot <- mapplot(ec, coords, coords$dew_99) +
  scale_fill_continuous_divergingx(palette = 'RdBu',
                                   limits = c(0.4, 9.72)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "Dewpoint")
dew_plot

st_plot99 <- ggarrange(mslp_plot, u250_plot, u850_plot,
                     v250_plot, v850_plot, dew_plot, nrow = 2, ncol = 3,
                         common.legend = TRUE, legend = "bottom")
st_plot95
st_plot99

saveRDS(dew_plot, "st_dew_plot99.rds")
saveRDS(st_plot99, "st_plot99.rds")

# Lasso
lmslp_plot <- mapplot(ec, coords, coords$lmslp_5) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.55)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "MSLP")
lmslp_plot

lu250_plot <- mapplot(ec, coords, coords$lu250_5) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.55)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "U250")  
lu250_plot

lu850_plot <- mapplot(ec, coords, coords$lu850_5) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.55)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "U850")
lu850_plot

lv250_plot <- mapplot(ec, coords, coords$lv250_5) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.55)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "V250")
lv250_plot

lv850_plot <- mapplot(ec, coords, coords$lv850_5) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.55)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "V850")
lv850_plot

ldew_plot <- mapplot(ec, coords, coords$ldew_5) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(0.07, 10.3)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "Dewpoint Temperature")
ldew_plot

lsoi_plot <- mapplot(ec, coords, coords$lsoi_5) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.55)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "SOI")
lsoi_plot

ldmi_plot <- mapplot(ec, coords, coords$ldmi_5) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.55)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "DMI")
ldmi_plot

ltemp_plot <- mapplot(ec, coords, coords$lanom_5) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(-0.6, 2.55)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = expression("%" * Delta ~ lambda),
       title = "Anomaly (\u00B0C)")
ltemp_plot

lasso_plot5 <- ggarrange(lmslp_plot, lu250_plot, lu850_plot, lv250_plot, 
                        lv850_plot, lsoi_plot, ldmi_plot, ltemp_plot,
                        nrow = 2, ncol = 4,
                     common.legend = TRUE, legend = "bottom")
lasso_plot1
lasso_plot5

saveRDS(lasso_plot5, "lasso_plot5.rds")
saveRDS(ldew_plot, "lasso_dew_plot5.rds")


# Deviance Plots
harmdev_plot <- mapplot(ec, coords, coords$harm_dev) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(0, 1)) +
  labs(fill = "Deviance (%)",
       title = "Harmonic")
harmdev_plot

ltdev_plot <- mapplot(ec, coords, coords$lt_dev) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(0, 1)) +
  labs(fill = "Deviance (%)",
       title = "LT")
ltdev_plot  

stdev_plot <- mapplot(ec, coords, coords$st_dev) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(0, 1)) +
  labs(fill = "Deviance (%)",
       title = "ST")
stdev_plot  

lassodev_plot <- mapplot(ec, coords, coords$lasso_dev) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0,
                                   limits = c(0, 1)) +
  labs(fill = "Deviance (%)",
       title = "Lasso")
lassodev_plot  

dev_plot <- ggarrange(harmdev_plot, ltdev_plot,
                         stdev_plot, lassodev_plot,
                         nrow = 1, ncol = 4,
                         common.legend = TRUE, legend = "bottom")
dev_plot

saveRDS(dev_plot, "dev_plot.rds")


# Logarithmic Scores
hp_log_plot <- mapplot(ec, coords, coords$log15_hp) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = -0.03,
                                   limits = c(-0.06, 0)) +
  labs(fill = "Score",
       title = "HPP")
hp_log_plot

harm_log_plot <- mapplot(ec, coords, coords$log90_harm) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = -0.23,
                                   limits = c(-0.46, 0)) +
  labs(fill = "Score",
       title = "Harmonic")
harm_log_plot

lt_log_plot <- mapplot(ec, coords, coords$log90_lt) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = -0.23,
                                   limits = c(-0.46, 0)) +
  labs(fill = "Score",
       title = "LT")
lt_log_plot

st_log_plot <- mapplot(ec, coords, coords$log90_st) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = -0.23,
                                   limits = c(-0.46, 0)) +
  labs(fill = "Score",
       title = "ST")
st_log_plot

lasso_log_plot <- mapplot(ec, coords, coords$log15_lasso) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid = -0.03,
                                   limits = c(-0.06, 0)) +
  labs(fill = "Score",
       title = "Lasso")
lasso_log_plot

log15_plot <- ggarrange(hp_log_plot,
                        lasso_log_plot,
                      nrow = 1, ncol = 2,
                      common.legend = TRUE, legend = "bottom")
log90_plot


log15_plot <- annotate_figure(log15_plot, 
                              top = text_grob("15-Day Log Score",
                                                         face = "bold",
                                                         size = 20))
log_plot <- ggarrange(log15_plot, log90_plot,
                      nrow = 1, ncol = 2)

log_plot

saveRDS(log_plot, "log_plot.rds")

# MAE
hp_mae_plot <- mapplot(ec, coords, coords$mae90_hp) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   rev = TRUE,
                                   mid = 0.45,
                                   limits = c(0.28, 0.62)) +
  labs(fill = "MAE",
       title = "HPP")
hp_mae_plot

harm_mae_plot <- mapplot(ec, coords, coords$mae90_harm) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   rev = TRUE,
                                   mid = 0.45,
                                   limits = c(0.28, 0.62)) +
  labs(fill = "MAE",
       title = "Harmonic")
harm_mae_plot

lt_mae_plot <- mapplot(ec, coords, coords$mae90_lt) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   rev = TRUE,
                                   mid = 0.45,
                                   limits = c(0.28, 0.62)) +
  labs(fill = "MAE",
       title = "LT")
lt_mae_plot

st_mae_plot <- mapplot(ec, coords, coords$mae90_st) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   rev = TRUE,
                                   mid = 0.45,
                                   limits = c(0.28, 0.62)) +
  labs(fill = "MAE",
       title = "ST")
st_mae_plot

lasso_mae_plot <- mapplot(ec, coords, coords$mae90_lasso) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   rev = TRUE,
                                   mid = 0.45,
                                   limits = c(0.28, 0.62)) +
  labs(fill = "MAE",
       title = "Lasso")
lasso_mae_plot

mae90_plot <- ggarrange(hp_mae_plot, harm_mae_plot,
                        lt_mae_plot, st_mae_plot,
                        lasso_mae_plot,
                        nrow = 1, ncol = 5,
                        common.legend = TRUE, legend = "bottom")

mae90_plot <- ggarrange(hp_mae_plot, lasso_mae_plot,
                        nrow = 1, ncol = 2,
                        common.legend = TRUE, legend = "bottom")

mae15_plot


mae90_plot <- annotate_figure(mae90_plot, top = text_grob("90-Day MAE",
                                            face = "bold",
                                            size = 20))

mae_plot <- ggarrange(mae15_plot, mae90_plot,
                      nrow = 1, ncol = 2)

mae_plot

saveRDS(mae_plot, "mae_plot.rds")


# Unseen 15
dc3y <- coords %>%
  mutate(dc3 = ifelse(dc60 >= 2, dc60, NA))
dc_15d_plot <- mapplot(ec, coords, dc3y$dc3) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   mid = 0,
                                   limits = c(0, 5)) +
  labs(fill = "Extremes",
       title = "Observed")
dc_15d_plot

hp_15d_plot <- mapplot(ec, coords, coords$hp3) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   mid = 0,
                                   limits = c(0, 16)) +
  labs(fill = "Extremes",
       title = "HPP")
hp_15d_plot

harm_15d_plot <- mapplot(ec, coords, coords$harm3) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   mid = 0,
                                   limits = c(0, 16)) +
  labs(fill = "Extremes",
       title = "Harmonic")
harm_15d_plot

lt_15d_plot <- mapplot(ec, coords, coords$lt3) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   mid = 0,
                                   limits = c(0, 16)) +
  labs(fill = "Extremes",
       title = "LT")
lt_15d_plot

st_15d_plot <- mapplot(ec, coords, coords$st3) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   mid = 0,
                                   limits = c(0, 16)) +
  labs(fill = "Extremes",
       title = "ST")
st_15d_plot

lasso_15d_plot <- mapplot(ec, coords, coords$lasso3) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   mid = 0,
                                   limits = c(0, 16)) +
  labs(fill = "Extremes",
       title = "Lasso")
lasso_15d_plot

fit_plot <- ggarrange(dc_15d_plot,hp_15d_plot, 
                        harm_15d_plot,lt_15d_plot, 
                        st_15d_plot, lasso_15d_plot,
                        nrow = 1, ncol = 6,
                        common.legend = TRUE, legend = "bottom")

fit3_plot

lasso_prob60_plot <- mapplot(ec, coords, coords$lasso60_prob) +
  scale_fill_continuous_divergingx(palette = 'RdBu', 
                                   mid = 0,
                                   limits = c(0, 1)) +
  labs(fill = "Probability",
       title = "Lasso")
lasso_prob60_plot  

prob60_plot <- ggarrange(dc_15d_plot, lasso_prob60_plot,
                         nrow = 1, ncol = 2)
prob60_plot

saveRDS(prob60_plot, "prob60_plot.rds")
