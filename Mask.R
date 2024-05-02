library(tidyverse)
library(sf)
library(ozmaps)
library(zoo)

coordinates <- coords[c("lon", "lat")]
coordinates <- st_as_sf(coordinates, coords = c("lon", "lat"), crs = 4326)
coastline <- st_transform(ec, crs = st_crs(coordinates))
land <- st_join(coordinates, coastline, join = st_within)
land.na <- c(is.na(land$NAME))

coords <- coords %>%
  mutate(land = ifelse(land.na == FALSE, "TRUE", "FALSE"))

            