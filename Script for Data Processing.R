library(ncdf4)
library(tidyverse)
library(extRemes)

### Load Net CDF Files
listncdf <- list("1940" = nc_open("1940.nc"),
                 "1941" = nc_open("1941.nc"),
                 "1942" = nc_open("1942.nc"),
                 "1943" = nc_open("1943.nc"),
                 "1944" = nc_open("1944.nc"),
                 "1945" = nc_open("1945.nc"),
                 "1946" = nc_open("1946.nc"),
                 "1947" = nc_open("1947.nc"),
                 "1948" = nc_open("1948.nc"),
                 "1949" = nc_open("1949.nc"),
                 "1950" = nc_open("1950.nc"),
                 "1951" = nc_open("1951.nc"),
                 "1952" = nc_open("1952.nc"),
                 "1953" = nc_open("1953.nc"),
                 "1954" = nc_open("1954.nc"),
                 "1955" = nc_open("1955.nc"),
                 "1956" = nc_open("1956.nc"),
                 "1957" = nc_open("1957.nc"),
                 "1958" = nc_open("1958.nc"),
                 "1959" = nc_open("1959.nc"),
                 "1960" = nc_open("1960.nc"),
                 "1961" = nc_open("1961.nc"),
                 "1962" = nc_open("1962.nc"),
                 "1963" = nc_open("1963.nc"),
                 "1964" = nc_open("1964.nc"),
                 "1965" = nc_open("1965.nc"),
                 "1966" = nc_open("1966.nc"),
                 "1967" = nc_open("1967.nc"),
                 "1968" = nc_open("1968.nc"),
                 "1969" = nc_open("1969.nc"),
                 "1970" = nc_open("1970.nc"),
                 "1971" = nc_open("1971.nc"),
                 "1972" = nc_open("1972.nc"),
                 "1973" = nc_open("1973.nc"),
                 "1974" = nc_open("1974.nc"),
                 "1975" = nc_open("1975.nc"),
                 "1976" = nc_open("1976.nc"),
                 "1977" = nc_open("1977.nc"),
                 "1978" = nc_open("1978.nc"),
                 "1979" = nc_open("1979.nc"),
                 "1980" = nc_open("1980.nc"),
                 "1981" = nc_open("1981.nc"),
                 "1982" = nc_open("1982.nc"),
                 "1983" = nc_open("1983.nc"),
                 "1984" = nc_open("1984.nc"),
                 "1985" = nc_open("1985.nc"),
                 "1986" = nc_open("1986.nc"),
                 "1987" = nc_open("1987.nc"),
                 "1988" = nc_open("1988.nc"),
                 "1989" = nc_open("1989.nc"),
                 "1990" = nc_open("1990.nc"),
                 "1991" = nc_open("1991.nc"),
                 "1992" = nc_open("1992.nc"),
                 "1993" = nc_open("1993.nc"),
                 "1994" = nc_open("1994.nc"),
                 "1995" = nc_open("1995.nc"),
                 "1996" = nc_open("1996.nc"),
                 "1997" = nc_open("1997.nc"),
                 "1998" = nc_open("1998.nc"),
                 "1999" = nc_open("1999.nc"),
                 "2000" = nc_open("2000.nc"),
                 "2001" = nc_open("2001.nc"),
                 "2002" = nc_open("2002.nc"),
                 "2003" = nc_open("2003.nc"),
                 "2004" = nc_open("2004.nc"),
                 "2005" = nc_open("2005.nc"),
                 "2006" = nc_open("2006.nc"),
                 "2007" = nc_open("2007.nc"),
                 "2008" = nc_open("2008.nc"),
                 "2009" = nc_open("2009.nc"),
                 "2010" = nc_open("2010.nc"),
                 "2011" = nc_open("2011.nc"),
                 "2012" = nc_open("2012.nc"),
                 "2013" = nc_open("2013.nc"),
                 "2014" = nc_open("2014.nc"),
                 "2015" = nc_open("2015.nc"),
                 "2016" = nc_open("2016.nc"),
                 "2017" = nc_open("2017.nc"),
                 "2018" = nc_open("2018.nc"),
                 "2019" = nc_open("2019.nc"),
                 "2020" = nc_open("2020.nc"),
                 "2021" = nc_open("2021.nc"),
                 "2022" = nc_open("2022.nc"))

### Function to extract precipitation data from ncdf
varget <- function(years, data) {
  listdf <- list()
  for (i in 1:years) {
    listdf[[i]] <- ncvar_get(data[[i]], "precip") %>%
      as_tibble()
  }
  return(listdf)
}


### List object with precipitation data
listdf <- varget(length(listncdf), listncdf)

### Number of Longitude and Latitude coordinates
latitude <- seq(-34.45, -24.85, by = 0.05)
lat <- dim(listdf[[2]])[2] / 365
longitude <- seq(150.65, 153.65, by = 0.05)
lon <- dim(listdf[[1]])[1]

### Function to create annual dataframe of precipitation at each grid point
dailyrain <- function(data, lat, lon) {
  cols <- ncol(data)
  days <- cols / lat
  rain.df <- matrix(nrow = days, ncol = lat*lon)
  
  for (i in 1:lat) {
    choose <- seq(i, cols, by = lat)
    for (j in 1:lon) {
      rain.df[, (i - 1) * lon + j] <- t(data[j, choose, drop = FALSE])
    }
  }
  rain.df <- rain.df %>%
    as_tibble()
  return(rain.df)
}

### Run function on actual data
rainlist <- list()
for (i in 1:length(listdf)) {
rainlist[[i]] <- dailyrain(listdf[[i]], lat, lon)  
}

### Merge all list objects into a single dataframe
precip.df <- bind_rows(rainlist) %>%
  as.data.frame()

library(readr)
precip.df <- readRDS("precipitation.rds") %>%
  as.data.frame()

### Define Threshold for all grid points
thresh.df <- data.frame(matrix(NA, nrow = ncol(precip.df)))
for (i in 1:ncol(precip.df)) {
  temp.df <- precip.df[,i] %>%
    as_tibble() %>%
    filter(value > 0) %>%
    as.data.frame()
  thresh.df[i,] <- quantile(temp.df[, 1], 0.99)
}

binary.df <- data.frame(matrix(NA, nrow = nrow(precip.df), ncol = ncol(precip.df)))
for (i in 1:ncol(precip.df)) {
  binary.df[,i] <- ifelse(precip.df[,i] >= thresh.df[i,], 1, 0)
}


### Function to create declustered extremes dataframe
extremes.df <- data.frame(matrix(NA, nrow = nrow(precip.df), ncol = ncol(precip.df)))
for (i in 1:ncol(precip.df)) {
  extremes.df[, i] <- decluster(precip.df[, i], thresh.df[i,],
                                r = 2)
}

### Function to create binary series of declustered extremes
dc.df <- data.frame(matrix(NA, nrow = nrow(extremes.df), ncol = ncol(extremes.df)))
for (i in 1:ncol(extremes.df)) {
  dc.df[,i] <- ifelse(extremes.df[, i] > thresh.df[i,], 1, 0)
}



saveRDS(precip.df, "precipitation.rds")
saveRDS(thresh.df, "thresholds.rds")
saveRDS(binary.df, "extremes.rds")
saveRDS(dc.df, "declustered.rds")


sum(binary.df[,1])
sum(dc.df[,1])

sumfun <- function(data) {
df <- data.frame(matrix(NA, nrow = ncol(data)))
for (i in 1:ncol(data)) {
  df[i,] <- sum(data[,i])
}
return(df)
}

nondc <- sumfun(binary.df)
dc <- sumfun(dc.df)
diff <- nondc - dc
hp <- dc / (nrow(precip.df) - diff)

