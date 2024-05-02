library(tidyverse)
library(data.table)
library(svMisc)

# Rain Data
setwd("~/Documents/CLEX Summer Research/East Coast/Precipitation Data")
precip <- readRDS("precipitation.rds")[, 9001:11773]
extremes <- readRDS("extremes.rds")[,9001:11773]
declustered <- readRDS("declustered.rds")[, 9001:11773]


precipcoords <- readRDS("coords.rds")[9001:11773,] %>%
  as_tibble() %>%
  mutate(latround = round(lat*4) / 4,
         lonround = round(lon*4) / 4)

setwd("~/Documents/CLEX Summer Research/East Coast/Temperature Data")
anomaly <- readRDS("anomaly.rds")[,9002:11774]


# Atmospheric Data
setwd("~/Documents/CLEX Summer Research/East Coast/Atmospheric Data")
mslp <- readRDS("mslp.rds")[, 2:521]
u250 <- readRDS("u250.rds")[, 2:521]
u850 <- readRDS("u850.rds")[, 2:521]
v250 <- readRDS("v250.rds")[, 2:521]
v850 <- readRDS("v850.rds")[, 2:521]
dew <- readRDS("dewpoint.rds")[, 2:521]

soi <- readRDS("soi.rds")
dmi <- readRDS("dmi.rds")

largescaledates <- function(data) {
  clean <- data %>%
    select(-c(...1)) %>%
    filter(date >= "1940-01-01") %>%
    filter(date <= "2022-12-31")
  
  clean <- clean[,2]
}

soi <- largescaledates(soi)
dmi <- largescaledates(dmi)

atmoscoords <- readRDS("atmospheric_coords.rds") %>%
  as_tibble()

# Time Data
harmonic <- seq.Date(from = ymd("1940-01-01"), to = ymd("2022-12-31"), by = "day") %>%
  as_tibble() %>%
  mutate(day = yday(value),
         cos = cos(2*pi*day / 365),
         sin = sin(2*pi*day / 365)) %>%
  select(cos, sin)


df <- lapply(1:ncol(precip), function(i) {
  matching_lat <- which(atmoscoords[,2] == precipcoords[[i, 8]])
  matching_lon <- which(atmoscoords[,3] == precipcoords[[i, 9]])
  matching_coords <- intersect(matching_lat, matching_lon)
  
  df <- cbind(precip[,i], extremes[,i], declustered[,i], harmonic[,1], harmonic[,2],
                   mslp[, matching_coords], u250[, matching_coords],
                   u850[, matching_coords], v250[, matching_coords],
                   v850[, matching_coords], dew[, matching_coords], 
                   soi[,1], dmi[,1], anomaly[,i])
  names(df)[1] <- "mm"
  names(df)[2] <- "ext"
  names(df)[3] <- "dc"
  names(df)[4] <- "cos"
  names(df)[5] <- "sin"
  names(df)[6] <- "mslp"
  names(df)[7] <- "u250"
  names(df)[8] <- "u850"
  names(df)[9] <- "v250"
  names(df)[10] <- "v850"
  names(df)[11] <- "dewpoint"
  names(df)[12] <- "soi"
  names(df)[13] <- "dmi"
  names(df)[14] <- "anomaly"
  
  
  
  df <- df %>%
    as_tibble() %>%
    mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day")) %>%
    relocate(date)
})

rm(precip, extremes, declustered, mslp, u250, u850, v250, v850, dew, soi, dmi,
   harmonic, anomaly, largescaledates)
gc()


remove.declustered <- function(data, cat) {
  data <- data %>%
    mutate(dc.ext = ifelse(ext == 1 & dc == 0, 1, 0)) %>%
    filter(dc.ext == cat) %>%
    select(-dc.ext)
}

rem.list <- lapply(seq_along(df), function(i) {
  remove.declustered(df[[i]], 1)
})


df <- lapply(seq_along(df), function(i) {
  remove.declustered(df[[i]], 0)
})


scale.fun <- function(var) {
  (var - mean(var)) / sd(var)
}

scale.covariates <- function(data) {
  data <- data %>%
    mutate(cos = scale.fun(cos),
           sin = scale.fun(sin),
           mslp = scale.fun(mslp),
           u250 = scale.fun(u250),
           u850 = scale.fun(u850),
           v250 = scale.fun(v250),
           v850 = scale.fun(v850),
           dewpoint = scale.fun(dewpoint), 
           soi = scale.fun(soi),
           dmi = scale.fun(dmi),
           anomaly = scale.fun(anomaly))
}

scale.covariates.remove <- function(removed, original) {
  removed <- removed %>%
    mutate(cos = (cos - mean(original$cos)) / sd(original$cos),
           sin = (sin - mean(original$sin)) / sd(original$sin),
           mslp = (mslp - mean(original$mslp)) / sd(original$mslp),
           u250 = (u250 - mean(original$u250)) / sd(original$u250),
           u850 = (u850 - mean(original$u850)) / sd(original$u850),
           v250 = (v250 - mean(original$v250)) / sd(original$v250),
           v850 = (v850 - mean(original$v850)) / sd(original$v850),
           dewpoint = (dewpoint - mean(original$dewpoint)) / sd(original$dewpoint), 
           soi = (soi - mean(original$soi)) / sd(original$soi),
           dmi = (dmi - mean(original$dmi)) / sd(original$dmi),
           anomaly = (anomaly - mean(original$anomaly)) / sd(original$anomaly))
}

for (i in 1:length(rem.list)) {
  rem.list[[i]] <- scale.covariates.remove(rem.list[[i]], df[[i]])
  progress(i, length(rem.list))
}

df <- lapply(seq_along(df), function(i) {
  df[[i]] %>%
    scale.covariates()
})
