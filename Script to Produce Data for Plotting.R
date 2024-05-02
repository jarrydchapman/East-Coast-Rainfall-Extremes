# Required Packages
library(tidyverse)
library(sf)
library(ozmaps)
library(zoo)
library(svMisc)
library(RcppRoll)
library(pROC)
library(data.table)

# Mask
setwd("~/Documents/CLEX Summer Research/East Coast")

coords <- readRDS("dataframe.rds")
ec <- ozmap("states") %>%
  filter(NAME == "Queensland" | NAME == "New South Wales")

coordinates <- coords[c("lon", "lat")]
coordinates <- st_as_sf(coordinates, coords = c("lon", "lat"), crs = 4326)
coastline <- st_transform(ec, crs = st_crs(coordinates))
land <- st_join(coordinates, coastline, join = st_within)
land.na <- c(is.na(land$NAME))

coords <- coords %>%
  mutate(land = ifelse(land.na == FALSE, "TRUE", "FALSE"))

rm(coastline, coordinates, ec, land, land.na)

# Preliminary Analysis
setwd("~/Documents/CLEX Summer Research/East Coast/Precipitation Data")

## Threshold Rainfall Amount
thresholds <- readRDS("thresholds.rds")
names(thresholds)[1] <- "threshold"
coords <- coords %>%
  mutate(threshold = thresholds$threshold)

rm(thresholds)

## Number of Extremes
ext <- readRDS("extremes.rds")
extdf <- data.frame(matrix(NA, nrow = 11773, ncol = 1))
for (i in 1:11773) {
  extdf[i,] <- sum(ext[,i])
  progress(i, 11773)
}
names(extdf)[1] <- "ext"

coords <- coords %>%
  mutate(ext = extdf$ext)

rm(ext, extdf)

## Number of Declustered Extremes
dc <- readRDS("declustered.rds")
dcdf <- data.frame(matrix(NA, nrow = 11773, ncol = 1))
for (i in 1:11773) {
  dcdf[i,] <- sum(dc[,i])
  progress(i, 11773)
}
names(dcdf)[1] <- "dc"

coords <- coords %>%
  mutate(dc = dcdf$dc)

rm(dc, dcdf)

# Percentage of Extremes lost via Declustering
coords <- coords %>%
  mutate(extpc = 1 - (dc / ext))

# Intensity Functions
setwd("~/Documents/CLEX Summer Research/East Coast/Outputs/Lambda")

prob.fun <- function(k, lambda) {
  if(k == 1) {
    probge1 <- 1 - ((exp(-lambda)*(lambda^0)) / factorial(0))
  } else {
    
  if(k == 2) {
    probge2 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                      ((exp(-lambda)*(lambda^1)) / factorial(1)))
  } else {
    if(k == 3) {
      probge3 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                        ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                        ((exp(-lambda)*(lambda^2)) / factorial(2)))
    } else {
      if(k == 4) {
        probge4 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                          ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                          ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                          ((exp(-lambda)*(lambda^3)) / factorial(3)))
      } else {
        if(k == 5) {
          probge5 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                            ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                            ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                            ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                            ((exp(-lambda)*(lambda^4)) / factorial(4)))
        } else {
          if(k == 6) {
            probge6 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                              ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                              ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                              ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                              ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                              ((exp(-lambda)*(lambda^5)) / factorial(5)))
          } else {
            if(k == 7) {
              probge7 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                                ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                                ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                                ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                                ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                                ((exp(-lambda)*(lambda^5)) / factorial(5)) +
                                ((exp(-lambda)*(lambda^6)) / factorial(6)))
            } else {
              if(k == 8) {
                probge8 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                                  ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                                  ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                                  ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                                  ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                                  ((exp(-lambda)*(lambda^5)) / factorial(5)) +
                                  ((exp(-lambda)*(lambda^6)) / factorial(6)) +
                                  ((exp(-lambda)*(lambda^7)) / factorial(7)))
              } else {
                if(k == 9) {
                  probge9 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                                    ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                                    ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                                    ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                                    ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                                    ((exp(-lambda)*(lambda^5)) / factorial(5)) +
                                    ((exp(-lambda)*(lambda^6)) / factorial(6)) +
                                    ((exp(-lambda)*(lambda^7)) / factorial(7)) +
                                    ((exp(-lambda)*(lambda^8)) / factorial(8)))
                } else {
                  if(k == 10) {
                    probge10 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                                       ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                                       ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                                       ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                                       ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                                       ((exp(-lambda)*(lambda^5)) / factorial(5)) +
                                       ((exp(-lambda)*(lambda^6)) / factorial(6)) +
                                       ((exp(-lambda)*(lambda^7)) / factorial(7)) +
                                       ((exp(-lambda)*(lambda^8)) / factorial(8)) +
                                       ((exp(-lambda)*(lambda^9)) / factorial(9)))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  }
}

hp <- readRDS("hp_lambda.rds")[,2:11774]
hp <- t(hp[1,]) %>%
  as.data.frame()
names(hp)[1] <- "hp"
hp <- hp %>%
  as_tibble() %>%
  mutate(hp90 = hp*90,
         hp1prob = prob.fun(1, hp),
         hp90prob = prob.fun(2, hp90))
coords <- coords %>%
  mutate(hp1 = hp$hp,
         hp90 = hp$hp90,
         hp1prob = hp$hp1prob,
         hp90prob = hp$hp90prob)

rm(hp)

# Harmonic Intensity
harm <- readRDS("harm_lambda.rds")
date <- harm[1]
harm <- harm %>% 
  select(-date)

names(harm) <- paste0("V", seq(1:11773))

harm <- harm[367:731,]

harm <- harm %>%
  as_tibble() %>%
  mutate(date = seq.Date(ymd("1941-01-01"), ymd("1941-12-31"), by = "day"),
         doy = yday(date)) %>%
  relocate(c(date, doy))

harm <- harm %>%
  select(-date)

harm_doy <- harm %>%
  pivot_longer(cols = starts_with("V"), names_to = "gridpoint", values_to = "intensity")

harm <- harm %>%
  mutate(date = seq.Date(ymd("1941-01-01"), ymd("1941-12-31"), by = "day"),
         month = month(date),
         season = ifelse(month == 1 | month == 2 | month == 12, "Summer",
                  ifelse(month == 3 | month == 4 | month == 5, "Autumn",
                  ifelse(month == 6 | month == 7 | month == 8, "Winter",
                  ifelse(month == 9 | month == 10 | month == 11, "Spring", NA))))) %>%
  select(-month) %>%
  relocate(season, .before = V1)

seas.lam <- harm %>%
  select(-c(date, doy)) %>%
  group_by(season) %>%
  summarise_all(sum)

seasonal.lam <- data.frame(matrix(NA, nrow = 11773, ncol = 4))
for (i in 1:11773) {
  for (j in 1:4) {
    seasonal.lam[i,j] <- seas.lam[j, i+1]
  }
  progress(i, 11773)
}
names(seasonal.lam)[1] <- "autumn"
names(seasonal.lam)[2] <- "spring"
names(seasonal.lam)[3] <- "summer"
names(seasonal.lam)[4] <- "winter"

coords <- coords %>%
  mutate(summer = seasonal.lam$summer,
         autumn = seasonal.lam$autumn,
         winter = seasonal.lam$winter,
         spring = seasonal.lam$spring,
         summerprob = prob.fun(2, summer),
         autumnprob = prob.fun(2, autumn),
         winterprob = prob.fun(2, winter),
         springprob = prob.fun(2, spring))

rm(date, harm, seas.lam, seasonal.lam)


# Long-Term Covariates
setwd("~/Documents/CLEX Summer Research/East Coast/Outputs/Coefficients")
lt <- readRDS("temp_lt_coef.rds")

sig.coef.fun <- function(data, sig_level) {
  
  cols <- seq(3, ncol(data), 3)
  df <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(cols)))
  
  for (i in 1:length(cols)) {
    df[,i] <- ifelse(data[,cols[i]] < sig_level, data[,(cols[i]-2)], NA)
  }
  return(df)
}

lt95 <- sig.coef.fun(lt, 0.05)
names(lt95) <- c("intercept", "cos", "sin", "soi", "dmi", "anomaly")
lt99 <- sig.coef.fun(lt, 0.01)
names(lt99) <- c("intercept", "cos", "sin", "soi", "dmi", "anomaly")

coords <- coords %>%
  mutate(soi_95 = exp(lt95$soi) - 1,
         dmi_95 = exp(lt95$dmi) - 1,
         anom_95 = exp(lt95$anomaly) - 1,
         soi_99 = exp(lt99$soi) - 1,
         dmi_99 = exp(lt99$dmi) - 1,
         anom_99 = exp(lt99$anomaly) - 1)

rm(lt, lt95, lt99)


# Short-Term Covariates
st <- readRDS("st_coef.rds")
st95 <- sig.coef.fun(st, 0.05)
names(st95) <- c("intercept", "cos", "sin", "mslp", "u250", "u850",
                 "v250", "v850", "dewpoint")
st99 <- sig.coef.fun(st, 0.01)
names(st99) <- c("intercept", "cos", "sin", "mslp", "u250", "u850",
                 "v250", "v850", "dewpoint")

coords <- coords %>%
  mutate(mslp_95 = exp(-1*st95$mslp) - 1,
         u250_95 = exp(-1*st95$u250) - 1,
         u850_95 = exp(-1*st95$u850) - 1,
         v250_95 = exp(st95$v250) - 1,
         v850_95 = exp(st95$v850) - 1,
         dew_95 = exp(st95$dewpoint) - 1,
         mslp_99 = exp(-1*st99$mslp) - 1,
         u250_99 = exp(-1*st99$u250) - 1,
         u850_99 = exp(-1*st99$u850) - 1,
         v250_99 = exp(st99$v250) - 1,
         v850_99 = exp(st99$v850) - 1,
         dew_99 = exp(st99$dewpoint) - 1)

rm(st, st95, st99)


# Lasso Covariates
lasso <- readRDS("lasso_coef.rds")

lasso.coef.fun <- function(data, threshold) {
  
  df <- data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
  
  for(i in 1:ncol(data)) {
    df[,i] <- ifelse(abs(data[,i]) > threshold, data[,i], NA)
  }
  return(df)
}

lasso_1 <- lasso.coef.fun(lasso, 0.01)
names(lasso_1) <- c("intercept", "cos", "sin", "mslp", "u250", "u850",
                    "v250", "v850", "dewpoint", "soi", "dmi", "anomaly")
lasso_5 <- lasso.coef.fun(lasso, 0.05)
names(lasso_5) <- c("intercept", "cos", "sin", "mslp", "u250", "u850",
                    "v250", "v850", "dewpoint", "soi", "dmi", "anomaly")

coords <- coords %>%
  mutate(lmslp_1 = exp(-1*lasso_1$mslp) - 1,
         lu250_1 = exp(-1*lasso_1$u250) - 1,
         lu850_1 = exp(-1*lasso_1$u850) - 1,
         lv250_1 = exp(lasso_1$v250) - 1,
         lv850_1 = exp(lasso_1$v850) - 1,
         ldew_1 = exp(lasso_1$dewpoint) - 1,
         lsoi_1 = exp(lasso_1$soi) - 1,
         ldmi_1 = exp(lasso_1$dmi) - 1,
         lanom_1 = exp(lasso_1$anomaly) - 1,
         lmslp_5 = exp(-1*lasso_5$mslp) - 1,
         lu250_5 = exp(-1*lasso_5$u250) - 1,
         lu850_5 = exp(-1*lasso_5$u850) - 1,
         lv250_5 = exp(lasso_5$v250) - 1,
         lv850_5 = exp(lasso_5$v850) - 1,
         ldew_5 = exp(lasso_5$dewpoint) - 1,
         lsoi_5 = exp(lasso_5$soi) - 1,
         ldmi_5 = exp(lasso_5$dmi) - 1,
         lanom_5 = exp(lasso_5$anomaly) - 1)


rm(lasso, lasso_1, lasso_5)


# Deviance
setwd("~/Documents/CLEX Summer Research/East Coast/Outputs/Coefficients")
hp <- readRDS("hp_coef.rds")
harm <- readRDS("harm_coef.rds")
lt <- readRDS("temp_lt_coef.rds")
st <- readRDS("st_coef.rds")
lasso <- readRDS("lasso_coef.rds")

deviance <- cbind(harm[ncol(harm)],
                  lt[ncol(lt)],
                  st[ncol(st)],
                  lasso[ncol(lasso)])
names(deviance) <- c("harm_dev", "lt_dev", "st_dev", "lasso_dev")
deviance <- deviance %>%
  as_tibble()

coords <- coords %>%
  mutate(harm_dev = deviance$harm_dev,
         lt_dev = deviance$lt_dev,
         st_dev = deviance$st_dev,
         lasso_dev = deviance$lasso_dev)

# Interval Intensity and Probability
setwd("~/Documents/CLEX Summer Research/East Coast/Precipitation Data")
dc <- readRDS("declustered.rds")

setwd("~/Documents/CLEX Summer Research/East Coast/Outputs/Lambda")
hp <- readRDS("hp_lambda.rds")[,2:11774]
harm <- readRDS("harm_lambda.rds")[,2:11774]
lt <- readRDS("lt_lambda.rds")[,2:11774]
st <- readRDS("st_lambda.rds")[,2:11774]
lasso <- readRDS("lasso_lambda.rds")[,2:11774]

roll.fun <- function(data, size) {
  roll.df <- data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
  
  data <- data %>%
    as.matrix()
  
  for (i in 1:ncol(data)) {
    temp <- data[,i]
    roll.df[,i] <- roll_sum(temp, n = size, fill = NA, align = "right")
    progress(i, ncol(data))
  }
  
  roll.df <- roll.df %>%
    as_tibble() %>%
    mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day")) %>%
    relocate(date)
  
  return(roll.df)
}

lambda_15 <- list(dc = roll.fun(dc, 15),
                  hp = roll.fun(hp, 15),
                  harm = roll.fun(harm, 15),
                  lt = roll.fun(lt, 15),
                  st = roll.fun(st, 15),
                  lasso = roll.fun(lasso, 15))

lambda_30 <- list(dc = roll.fun(dc, 30),
                  hp = roll.fun(hp, 30),
                  harm = roll.fun(harm, 30),
                  lt = roll.fun(lt, 30),
                  st = roll.fun(st, 30),
                  lasso = roll.fun(lasso, 30))

lambda_60 <- list(dc = roll.fun(dc, 60),
                  hp = roll.fun(hp, 60),
                  harm = roll.fun(harm, 60),
                  lt = roll.fun(lt, 60),
                  st = roll.fun(st, 60),
                  lasso = roll.fun(lasso, 60))

lambda_90 <- list(dc = roll.fun(dc, 90),
                  hp = roll.fun(hp, 90),
                  harm = roll.fun(harm, 90),
                  lt = roll.fun(lt, 90),
                  st = roll.fun(st, 90),
                  lasso = roll.fun(lasso, 90))

lambda_3y <- list(dc = roll.fun(dc, 1096),
                  hp = roll.fun(hp, 1096),
                  harm = roll.fun(harm, 1096),
                  lt = roll.fun(lt, 1096),
                  st = roll.fun(st, 1096),
                  lasso = roll.fun(lasso, 1096))


rm(dc, harm, hp, lasso, lt, st)

disjoint_fun <- function(data, size) {

date <- seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day")
start <- ymd("1940-01-01")
end <- ymd("2022-12-31")

interval_dates <- seq(from = end, to = start, by = -size) %>%
  rev()

keep_dates <- which(date %in% interval_dates)

df <- data.frame(matrix(NA, nrow = length(keep_dates), ncol = ncol(data)))
for (i in 1:length(keep_dates)) {
  df[i,] <- data[keep_dates[i],]
  progress(i, length(keep_dates))
}

df <- df %>%
  select(-X1)

names(df) <- c(paste0("V", seq(1:11773)))

df <- df %>%
  as_tibble() %>%
  mutate(date = interval_dates) %>%
  relocate(date) %>%
  na.omit()

return(df)
}

disjoint_15 <- list(dc = disjoint_fun(lambda_15[[1]], 15),
                    hp = disjoint_fun(lambda_15[[2]], 15),
                    harm = disjoint_fun(lambda_15[[3]], 15),
                    lt = disjoint_fun(lambda_15[[4]], 15),
                    st = disjoint_fun(lambda_15[[5]], 15),
                    lasso = disjoint_fun(lambda_15[[6]], 15))

disjoint_30 <- list(dc = disjoint_fun(lambda_30[[1]], 30),
                    hp = disjoint_fun(lambda_30[[2]], 30),
                    harm = disjoint_fun(lambda_30[[3]], 30),
                    lt = disjoint_fun(lambda_30[[4]], 30),
                    st = disjoint_fun(lambda_30[[5]], 30),
                    lasso = disjoint_fun(lambda_30[[6]], 30))

disjoint_60 <- list(dc = disjoint_fun(lambda_60[[1]], 60),
                    hp = disjoint_fun(lambda_60[[2]], 60),
                    harm = disjoint_fun(lambda_60[[3]], 60),
                    lt = disjoint_fun(lambda_60[[4]], 60),
                    st = disjoint_fun(lambda_60[[5]], 60),
                    lasso = disjoint_fun(lambda_60[[6]], 60))

disjoint_90 <- list(dc = disjoint_fun(lambda_90[[1]], 90),
                    hp = disjoint_fun(lambda_90[[2]], 90),
                    harm = disjoint_fun(lambda_90[[3]], 90),
                    lt = disjoint_fun(lambda_90[[4]], 90),
                    st = disjoint_fun(lambda_90[[5]], 90),
                    lasso = disjoint_fun(lambda_90[[6]], 90))


dc_binary_fun <- function(data) {
df <- data
for (i in 1:(ncol(df)-1)) {
  df[,i+1] <- ifelse(df[,i+1] >= 2, 1, 0)
  progress(i, ncol(df))
}

return(df)
}

prob.fun <- function(k, lambda) {
  if(k == 2) {
    probge2 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                      ((exp(-lambda)*(lambda^1)) / factorial(1)))
  } else {
    if(k == 3) {
      probge3 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                        ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                        ((exp(-lambda)*(lambda^2)) / factorial(2)))
    } else {
      if(k == 4) {
        probge4 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                          ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                          ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                          ((exp(-lambda)*(lambda^3)) / factorial(3)))
      } else {
        if(k == 5) {
          probge5 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                            ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                            ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                            ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                            ((exp(-lambda)*(lambda^4)) / factorial(4)))
        } else {
          if(k == 6) {
            probge6 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                              ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                              ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                              ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                              ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                              ((exp(-lambda)*(lambda^5)) / factorial(5)))
          } else {
            if(k == 7) {
              probge7 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                                ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                                ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                                ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                                ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                                ((exp(-lambda)*(lambda^5)) / factorial(5)) +
                                ((exp(-lambda)*(lambda^6)) / factorial(6)))
            } else {
              if(k == 8) {
                probge8 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                                  ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                                  ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                                  ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                                  ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                                  ((exp(-lambda)*(lambda^5)) / factorial(5)) +
                                  ((exp(-lambda)*(lambda^6)) / factorial(6)) +
                                  ((exp(-lambda)*(lambda^7)) / factorial(7)))
              } else {
                if(k == 9) {
                  probge9 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                                    ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                                    ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                                    ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                                    ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                                    ((exp(-lambda)*(lambda^5)) / factorial(5)) +
                                    ((exp(-lambda)*(lambda^6)) / factorial(6)) +
                                    ((exp(-lambda)*(lambda^7)) / factorial(7)) +
                                    ((exp(-lambda)*(lambda^8)) / factorial(8)))
                } else {
                  if(k == 10) {
                    probge10 <- 1 - (((exp(-lambda)*(lambda^0)) / factorial(0)) +
                                       ((exp(-lambda)*(lambda^1)) / factorial(1)) +
                                       ((exp(-lambda)*(lambda^2)) / factorial(2)) +
                                       ((exp(-lambda)*(lambda^3)) / factorial(3)) +
                                       ((exp(-lambda)*(lambda^4)) / factorial(4)) +
                                       ((exp(-lambda)*(lambda^5)) / factorial(5)) +
                                       ((exp(-lambda)*(lambda^6)) / factorial(6)) +
                                       ((exp(-lambda)*(lambda^7)) / factorial(7)) +
                                       ((exp(-lambda)*(lambda^8)) / factorial(8)) +
                                       ((exp(-lambda)*(lambda^9)) / factorial(9)))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

prob_df_fun <- function(data, events) {
  prob_list <- list()
  for (i in 2:6) {
    prob_list[[i-1]] <- data[[i]] %>%
      select(-date)
    
    for (j in 1:11773) {
      prob_list[[i-1]][j] <- prob.fun(events, prob_list[[i-1]][j])
      progress(j, 11773)
    }
  }
  names(prob_list) <- c("hp", "harm", "lt", "st", "lasso")
  return(prob_list)
}

brier_score <- function(probability, actual) {
  (probability - actual)^2
}

log_score <- function(probability, actual) {
  actual*log(probability) + (1 - actual)*log(1 - probability)
}

score_fun <- function(data, actual, method) {
  actual_df <- dc_binary_fun(actual) %>%
    select(-date)
  score_list <- list()
  for (i in 1:length(data)) {
    pred_df <- data[[i]]
    score_df <- data.frame(matrix(NA, nrow = 11773, ncol = 1))
    
    if(method == 1) {
    for(j in 1:11773) {
      score_df[j,1] <- sum(log_score(pred_df[j], actual_df[j])) / nrow(pred_df)
      progress(j, 11773)
}
      } else if(method == 2) {
        for(k in 1:11773) {
          score_df[k,1] <- sum(brier_score(pred_df[k], actual_df[k])) / nrow(pred_df)
          progress(k, 11773)
        }
      }
  names(score_df) <- "score"
  score_list[[i]] <- score_df
  }
  score <- data.frame(matrix(NA, nrow = 11773, ncol = 5))
  for (n in 1:5) {
    score[,n] <- score_list[[n]]
  }
  colnames(score) <- c("hp", "harm", "lt", "st", "lasso")
  return(score)
}

error_fun <- function(data, actual, method) {
  actual_df <- dc_binary_fun(actual) %>%
    select(-date)
  error_list <- list()
  for (i in 2:length(data)) {
    intensity_df <- data[[i]] %>%
      select(-date)
    error.df <- data.frame(matrix(NA, nrow = 11773, ncol = 1))
    
    if(method == 1) {
    ae <- abs(actual_df - intensity_df)
    
  for (j in 1:11773) {
    error.df[j,1] <- sum(ae[j]) / nrow(ae[j])
    progress(j, 11773)
  }
    } else {
      if(method == 2) {
        se <- (actual_df - intensity_df)^2
        
        for(k in 1:11773) {
          error.df[k,1] <- sqrt(sum(se[k]) / nrow(se[k]))
          progress(k, 11773)
        }
      }
    }
      names(error.df) <- "error"
      error_list[[i-1]] <- error.df
    }
  error <- data.frame(matrix(NA, nrow = 11773, ncol = 5))
  for (l in 1:5) {
    error[,l] <- error_list[[l]]
  }
  colnames(error) <- c("hp", "harm", "lt", "st", "lasso")
  return(error)
}

event_fun <- function(data, period) {
  event <- data.frame(matrix(NA, nrow = 11774, ncol = 6))
  for (i in 1:6) {
    event[,i] <- data.table::transpose(data[[i]] %>%
                                         filter(date == period))
    progress(i, 6)
  }
  event <- event[2:11774,]
  colnames(event) <- c("dc", "hp", "harm", "lt", "st", "lasso")
  return(event)
}

cond_error_fun <- function(data) {
  error_list <- list()
  for (i in 1:5) {
    cond_error_df <- data.frame(matrix(NA, nrow = 11773, ncol = 1))
    for(j in 1:11773) {
      ext <- data[[1]][j+1]
      pred <- data[[i+1]][j+1]
      clus <- which(ext >= 2)
      
      temp <- data.frame(matrix(NA, nrow = length(clus), ncol = 2))
      for (k in 1:length(clus)) {
        temp[k,1] <- ext[clus[k],]
        temp[k,2] <- pred[clus[k],]
      }
      cond_error_df[j,1] <- sum(abs(temp[1] - temp[2])) / nrow(temp)
    }
    error_list[[i]] <- cond_error_df
  }
  cond_error <- data.frame(matrix(NA, nrow = 11773, ncol = 5))
  for (l in 1:5) {
    cond_error[,l] <- error_list[[l]]
  }
  colnames(cond_error) <- c("hp", "harm", "lt", "st", "lasso")
  return(cond_error)
}

cond_score_fun <- function(actual, probability) {
  score_list <- list()
  for (i in 1:5) {
    cond_score_df <- data.frame(matrix(NA, nrow = 11773, ncol = 1))
    for(j in 1:11773) {
      ext <- actual[[1]][j+1]
      pred <- probability[[i]][j]
      clus <- which(ext >= 2)

      temp <- data.frame(matrix(NA, nrow = length(clus), ncol = 2))
      for (k in 1:length(clus)) {
        temp[k,1] <- ext[clus[k],]
        temp[k,1] <- ifelse(temp[k,1] >= 2, 1, 0)
        temp[k,2] <- pred[clus[k],]
      }
cond_score_df[j,1] <- sum(log_score(temp[2], temp[1])) / nrow(temp)
    }
    score_list[[i]] <- cond_score_df
  }
  cond_score <- data.frame(matrix(NA, nrow = 11773, ncol = 5))
  for (l in 1:5) {
    cond_score[,l] <- score_list[[l]]
  }
  colnames(cond_score) <- c("hp", "harm", "lt", "st", "lasso")
  return(cond_score)
}


# Probability Dataframe
prob_15 <- prob_df_fun(disjoint_90, 2)


# Logarithmic Scoring
log_15 <- score_fun(prob_15, disjoint_90[[1]], 1) ###
coords <- coords %>%
  mutate(log90_hp = log_15$hp,
         log90_harm = log_15$harm,
         log90_lt = log_15$lt,
         log90_st = log_15$st,
         log90_lasso = log_15$lasso)

cond_score_15 <- cond_score_fun(disjoint_90, prob_15) ###
coords <- coords %>%
  mutate(cond_log90_hp = cond_score_15$hp,
         cond_log90_harm = cond_score_15$harm,
         cond_log90_lt = cond_score_15$lt,
         cond_log90_st = cond_score_15$st,
         cond_log90_lasso = cond_score_15$lasso)

# Brier Scoring
brier_15 <- score_fun(prob_15, disjoint_90[[1]], 2)
coords <- coords %>%
  mutate(brier90_hp = brier_15$hp,
         brier90_harm = brier_15$harm,
         brier90_lt = brier_15$lt,
         brier90_st = brier_15$st,
         brier90_lasso = brier_15$lasso)


# Mean Absolute Error
mae_15 <- error_fun(disjoint_90, disjoint_90[[1]], 1) ###
coords <- coords %>%
  mutate(mae90_hp = mae_15$hp,
         mae90_harm = mae_15$harm,
         mae90_lt = mae_15$lt,
         mae90_st = mae_15$st,
         mae90_lasso = mae_15$lasso)

cond_mae_15 <- cond_error_fun(disjoint_90) ###
coords <- coords %>%
  mutate(cond_mae90_hp = cond_mae_15$hp,
         cond_mae90_harm = cond_mae_15$harm,
         cond_mae90_lt = cond_mae_15$lt,
         cond_mae90_st = cond_mae_15$st,
         cond_mae90_lasso = cond_mae_15$lasso)

# 2022 Event Intensity
unseen_15 <- event_fun(lambda_3y, ymd("2022-12-31"))
coords <- coords %>%
  mutate(dc3 = unseen_15$dc,
         hp3 = unseen_15$hp,
         harm3 = unseen_15$harm,
         lt3 = unseen_15$lt,
         st3 = unseen_15$st,
         lasso3 = unseen_15$lasso)
  

prob_unseen_15 <- unseen_15 %>%
  mutate(hp = prob.fun(10, hp),
         harm = prob.fun(10, harm),
         lt = prob.fun(10, lt),
         st = prob.fun(10, st),
         lasso = prob.fun(10, lasso)) ###

coords <- coords %>%
  mutate(hp3_prob = prob_unseen_15$hp,
         harm3_prob = prob_unseen_15$harm,
         lt3_prob = prob_unseen_15$lt,
         st3_prob = prob_unseen_15$st,
         lasso3_prob = prob_unseen_15$lasso)

setwd("~/Documents/CLEX Summer Research/East Coast")
saveRDS(coords, "dataframe.rds")

# Triple Dip La Nina








