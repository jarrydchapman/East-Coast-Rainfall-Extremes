library(ncdf4)
library(tidyverse)

u250list <- list("3am" = nc_open("u250_3am.nc"),
                 "9am" = nc_open("u250_9am.nc"),
                 "3pm" = nc_open("u250_3pm.nc"),
                 "9pm" = nc_open("u250_9pm.nc"))

u850list <- list("3am" = nc_open("u850_3am.nc"),
                 "9am" = nc_open("u850_9am.nc"),
                 "3pm" = nc_open("u850_3pm.nc"),
                 "9pm" = nc_open("u850_9pm.nc"))

v850list <- list("3am" = nc_open("v850_3am.nc"),
                 "9am" = nc_open("v850_9am.nc"),
                 "3pm" = nc_open("v850_3pm.nc"),
                 "9pm" = nc_open("v850_9pm.nc"))

mslplist <- list("3am" = nc_open("mslp_3am.nc"),
                 "9am" = nc_open("mslp_9am.nc"),
                 "3pm" = nc_open("mslp_3pm.nc"),
                 "9pm" = nc_open("mslp_9pm.nc"))

dewlist <- list("3am" = nc_open("dew_3am.nc"),
                "9am" = nc_open("dew_9am.nc"),
                "3pm" = nc_open("dew_3pm.nc"),
                "9pm" = nc_open("dew_9pm.nc"))

sstlist <- list("3am" = nc_open("sst_3am.nc"),
                "9am" = nc_open("sst_9am.nc"),
                "3pm" = nc_open("sst_3pm.nc"),
                "9pm" = nc_open("sst_9pm.nc"))


### Extract U-Wind Variable
varget.uwind <- function(periods, data) {
  listdf <- list()
  for (i in 1:periods) {
    listdf[[i]] <- ncvar_get(data[[i]], "v") %>%
      as_tibble()
  }
  return(listdf)
}

### Extract MSLP Variable
varget.mslp <- function(periods, data) {
  listdf <- list()
  for (i in 1:periods) {
    listdf[[i]] <- ncvar_get(data[[i]], "msl") %>%
      as_tibble()
  }
  return(listdf)
}

### Extract Dewpoint Variable
varget.dew <- function(periods, data) {
  listdf <- list()
  for (i in 1:periods) {
    listdf[[i]] <- ncvar_get(data[[i]], "d2m") %>%
      as_tibble()
  }
  return(listdf)
}

varget.sst <- function(periods, data) {
  listdf <- list()
  for (i in 1:periods) {
    listdf[[i]] <- ncvar_get(data[[i]], "sst") %>%
      as_tibble()
  }
  return(listdf)
}

# Create Dataframe
list250df <- varget.uwind(length(v850list), v850list)
list850df <- varget.uwind(length(u850list), u850list)
listmslpdf <- varget.mslp(length(mslplist), mslplist)
listdewdf <- varget.dew(length(dewlist), dewlist)
listsstdf <- varget.sst(length(sstlist), sstlist)


# Function to report variable at each grid point daily


lon <- sstlist[[1]]$dim$longitude$len
lat <- sstlist[[1]]$dim$latitude$len

dailyval <- function(data, lat, lon) {
  cols <- ncol(data)
  days <- cols / lat
  var.df <- data.frame(matrix(NA, nrow = days, ncol = lat * lon))

for (i in 1:lat) {
  choose <- seq(i, cols, by = lat)
  for (j in 1:lon) {
    var.df[, (i - 1) * lon + j] <- t(data[j, choose, drop = FALSE])
  }
}
  var.df <- var.df %>%
    as_tibble()
  return(var.df)
}

# Apply function to list object with hourly observations
v250list.df <- list()
for (i in 1:length(list250df)) {
  v250list.df[[i]] <- dailyval(list250df[[i]], lat, lon)  
}

u850list.df <- list()
for (i in 1:length(list850df)) {
  u850list.df[[i]] <- dailyval(list850df[[i]], lat, lon)  
}

mslplist.df <- list()
for (i in 1:length(listmslpdf)) {
  mslplist.df[[i]] <- dailyval(listmslpdf[[i]], lat, lon)  
}

dewlist.df <- list()
for (i in 1:length(listdewdf)) {
  dewlist.df[[i]] <- dailyval(listdewdf[[i]], lat, lon)  
}

sstlist.df <- list()
for (i in 1:length(listsstdf)) {
  sstlist.df[[i]] <- dailyval(listsstdf[[i]], lat, lon)  
}

# Function to take daily average of hourly values
dailyavg.uwind <- function(data) {
  avg <- (data[[1]] + data[[2]] + data[[3]] + data[[4]]) / 4
  avg <- avg*60*60/1000
  avg <- avg %>%
    as_tibble() %>%
    mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day")) %>%
    relocate(date)
  return(avg)
}

dailyavg.mslp <- function(data) {
  avg <- (data[[1]] + data[[2]] + data[[3]] + data[[4]]) / 4
  avg <- avg/100
  avg <- avg %>%
    as_tibble() %>%
    mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day")) %>%
    relocate(date)
  return(avg)
}

dailyavg.dew <- function(data) {
  avg <- (data[[1]] + data[[2]] + data[[3]] + data[[4]]) / 4
  avg <- avg - 273.15
  avg <- avg %>%
    as_tibble() %>%
    mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day")) %>%
    relocate(date)
  return(avg)
}


v250daily <- dailyavg.uwind(v250list.df)  
u850daily <- dailyavg.uwind(u850list.df)
mslpdaily <- dailyavg.mslp(mslplist.df)
dewdaily <- dailyavg.dew(dewlist.df)
sstdaily <- dailyavg.dew(sstlist.df)

write_csv(v250daily, "v850.csv")
write_csv(u850daily, "u850.csv")
write_csv(mslpdaily, "mslp.csv")
write_csv(dewdaily, "dewpoint.csv")
write_csv(sstdaily, "sst.csv")
