library(tidyverse)
library(ozmaps)
library(sf)
library(zoo)
library(RColorBrewer)
library(colorspace)
library(ggpubr)
library(gridExtra)

coords <- readRDS("coords.rds")

aus <- ozmap("states")
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
    theme_bw()
}



