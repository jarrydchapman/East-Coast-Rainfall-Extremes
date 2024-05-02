library(readr)
library(tidyverse)
library(lubridate)
library(svMisc)

start <- Sys.time()

temperature <- readRDS("temperature.rds")[,11001:11773]

temperature <- temperature %>%
  as_tibble() %>%
  mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day"),
         month = month(date),
         year = year(date)) %>%
  relocate(c(date, month, year))

base <- temperature %>%
  filter(year >= 1961 & year <= 1990) %>%
  select(-c(date, year)) %>%
  group_by(month) %>%
  summarise_all(mean)

actual <- temperature %>%
  select(-date) %>%
  group_by(month, year) %>%
  summarise_all(mean)

date <- temperature %>%
  select(date)

rm(temperature)

anomaly.df <- data.frame(matrix(NA, nrow = nrow(actual), ncol = 1000))
for (i in 1:1000) {
  for(j in 1:nrow(actual)) {
  month <- which(base[,1] == actual[[j,1]])
  anomaly.df[j,i] <- actual[j,i+2] - base[month, i+1]
  }
  progress(i, 1000)
}

anomaly.df <- anomaly.df[,1:773]

saveRDS(anomaly.df, "anomaly12000.rds")

end <- Sys.time()
