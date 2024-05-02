library(tidyverse)
library(glmnet)
library(svMisc)

## Run Script for Dataframes

startscript <- Sys.time()

# Homogeneous Poisson
hp <- list()
for(i in 1:length(df)) {
  hp[[i]] <- glm(dc ~ 1,
                 family = poisson(),
                 data = df[[i]])
  progress(i, length(df))
}

fitted.hp <- function(data, model) {
  df <- data.frame(matrix(NA, nrow = 30316, ncol = length(data)))
  for (i in 1:length(data)) {
    date <- data[[i]] %>%
      select(date)
    df[,i] <- date %>%
      mutate(fitted = model[[i]]$fitted.values) %>%
      complete(date = seq.Date(min(date), max(date), by = "day")) %>%
      fill(fitted, .direction = "down") %>%
      select(fitted)
      progress(i, length(df))
  }
  df <- df %>%
    mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), "day")) %>%
    relocate(date)
  return(df)
}

hp.lambda <- fitted.hp(df, hp)


hp.coef <- data.frame(matrix(NA, nrow = length(hp), ncol = 4))
for (i in 1:length(hp)) {
  hp.coef[i,1] <- summary(hp[[i]])$coefficients[,1]
  hp.coef[i,2] <- summary(hp[[i]])$coefficients[,2]
  hp.coef[i,3] <- summary(hp[[i]])$coefficients[,4]
  hp.coef[i,4] <- summary(hp[[i]])$aic
  progress(i, length(hp))
}



saveRDS(hp.lambda, "hp_lambda_12000.rds")
saveRDS(hp.coef, "hp_coef_2000.rds")
rm(hp, fitted.hp, hp.lambda, hp.coef)

# Harmonic Poisson
nhpp.harm <- list()
for (i in 1:length(df)) {
  nhpp.harm[[i]] <- glm(dc ~ cos + sin,
                        family = poisson(),
                        data = df[[i]])
  progress(i, length(df))
}

fitted.df <- function(data, model) {
  df <- data.frame(matrix(NA, nrow = 30316, ncol = length(df)))
  for (i in 1:length(df)) {
    date <- data[[i]] %>%
      select(date)
    df[,i] <- date %>%
      mutate(fitted = model[[i]]$fitted.values) %>%
      complete(date = seq.Date(min(date), max(date), by = "day")) %>%
      select(fitted)
    progress(i, length(df))
  }
  df <- df %>%
    mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), "day")) %>%
    relocate(date)
  return(df)
}

harm.df <- fitted.df(df, nhpp.harm)


for (i in 1:length(nhpp.harm)) {
  missing <- which(is.na(harm.df[,i+1]))
  for (j in 1:length(missing)) {
    harm.df[missing[j], i+1] <- exp(nhpp.harm[[i]]$coefficients[1] + 
                                  nhpp.harm[[i]]$coefficients[2]*rem.list[[i]]$cos[j] +
                                  nhpp.harm[[i]]$coefficients[3]*rem.list[[i]]$sin[j])
  }
  progress(i, length(nhpp.harm))
}

harm.coef <- data.frame(matrix(NA, nrow = length(nhpp.harm), ncol = 11))
for (i in 1:length(nhpp.harm)) {
  harm.coef[i,1] <- summary(nhpp.harm[[i]])$coefficients[1,1]
  harm.coef[i,2] <- summary(nhpp.harm[[i]])$coefficients[1,2]
  harm.coef[i,3] <- summary(nhpp.harm[[i]])$coefficients[1,4]
  harm.coef[i,4] <- summary(nhpp.harm[[i]])$coefficients[2,1]
  harm.coef[i,5] <- summary(nhpp.harm[[i]])$coefficients[2,2]
  harm.coef[i,6] <- summary(nhpp.harm[[i]])$coefficients[2,4]
  harm.coef[i,7] <- summary(nhpp.harm[[i]])$coefficients[3,1]
  harm.coef[i,8] <- summary(nhpp.harm[[i]])$coefficients[3,2]
  harm.coef[i,9] <- summary(nhpp.harm[[i]])$coefficients[3,4]
  harm.coef[i,10] <- summary(nhpp.harm[[i]])$aic
  harm.coef[i,11] <- 1 - (nhpp.harm[[i]]$deviance/nhpp.harm[[i]]$null.deviance)
  progress(i, length(nhpp.harm))
}

setwd("~/Documents/CLEX Summer Research/East Coast/Outputs")
saveRDS(harm.coef, "harm_coef_9000.rds")
saveRDS(harm.df, "harm_lambda_9000.rds")
rm(harm.df, nhpp.harm, i, j, start, end, missing, harm.coef)






# Seasonal Poisson
nhpp.lt <- list()
for (i in 1:length(df)) {
  nhpp.lt[[i]] <- glm(dc ~ cos + sin + soi + dmi + anomaly,
                        family = poisson(),
                        data = df[[i]])
  progress(i, length(df))
}

lt.df <- fitted.df(df, nhpp.lt)

for (i in 1:length(nhpp.lt)) {
  missing <- which(is.na(lt.df[,i+1]))
  for (j in 1:length(missing)) {
    lt.df[missing[j], i+1] <- exp(nhpp.lt[[i]]$coefficients[1] +
                                  nhpp.lt[[i]]$coefficients[2]*rem.list[[i]]$cos[j] +
                                  nhpp.lt[[i]]$coefficients[3]*rem.list[[i]]$sin[j] +
                                  nhpp.lt[[i]]$coefficients[4]*rem.list[[i]]$soi[j] +
                                  nhpp.lt[[i]]$coefficients[5]*rem.list[[i]]$dmi[j] +
                                  nhpp.lt[[i]]$coefficients[6]*rem.list[[i]]$anomaly[j])
  }
  progress(i, length(nhpp.lt))
}

lt.coef <- data.frame(matrix(NA, nrow = length(nhpp.lt), ncol = 20))
for (i in 1:length(nhpp.lt)) {
  lt.coef[i,1] <- summary(nhpp.lt[[i]])$coefficients[1,1]
  lt.coef[i,2] <- summary(nhpp.lt[[i]])$coefficients[1,2]
  lt.coef[i,3] <- summary(nhpp.lt[[i]])$coefficients[1,4]
  lt.coef[i,4] <- summary(nhpp.lt[[i]])$coefficients[2,1]
  lt.coef[i,5] <- summary(nhpp.lt[[i]])$coefficients[2,2]
  lt.coef[i,6] <- summary(nhpp.lt[[i]])$coefficients[2,4]
  lt.coef[i,7] <- summary(nhpp.lt[[i]])$coefficients[3,1]
  lt.coef[i,8] <- summary(nhpp.lt[[i]])$coefficients[3,2]
  lt.coef[i,9] <- summary(nhpp.lt[[i]])$coefficients[3,4]
  lt.coef[i,10] <- summary(nhpp.lt[[i]])$coefficients[4,1]
  lt.coef[i,11] <- summary(nhpp.lt[[i]])$coefficients[4,2]
  lt.coef[i,12] <- summary(nhpp.lt[[i]])$coefficients[4,4]
  lt.coef[i,13] <- summary(nhpp.lt[[i]])$coefficients[5,1]
  lt.coef[i,14] <- summary(nhpp.lt[[i]])$coefficients[5,2]
  lt.coef[i,15] <- summary(nhpp.lt[[i]])$coefficients[5,4]
  lt.coef[i,16] <- summary(nhpp.lt[[i]])$coefficients[6,1]
  lt.coef[i,17] <- summary(nhpp.lt[[i]])$coefficients[6,2]
  lt.coef[i,18] <- summary(nhpp.lt[[i]])$coefficients[6,4]
  lt.coef[i,19] <- summary(nhpp.lt[[i]])$aic
  lt.coef[i,20] <- 1 - (nhpp.lt[[i]]$deviance/nhpp.lt[[i]]$null.deviance)
  progress(i, length(nhpp.lt))
}

setwd("~/Documents/CLEX Summer Research/East Coast/Outputs")
saveRDS(lt.df, "lt_lambda_6000.rds")
saveRDS(lt.coef, "lt_coef_6000.rds")
rm(lt.df, nhpp.lt, i, j, start, end, missing, lt.coef)



# Subseasonal Poisson
nhpp.st <- list()
for (i in 1:length(df)) {
  nhpp.st[[i]] <- glm(dc ~ cos + sin + mslp + u250 +
                        u850 + v250  +
                        v850 + dewpoint,
                      family = poisson(),
                      data = df[[i]])
  progress(i, length(df))
}

st.df <- fitted.df(df, nhpp.st)

for (i in 1:length(nhpp.st)) {
  missing <- which(is.na(st.df[,i+1]))
  for (j in 1:length(missing)) {
    st.df[missing[j], i+1] <- exp(nhpp.st[[i]]$coefficients[1] + 
                                  nhpp.st[[i]]$coefficients[2]*rem.list[[i]]$cos[j] +
                                  nhpp.st[[i]]$coefficients[3]*rem.list[[i]]$sin[j] +
                                  nhpp.st[[i]]$coefficients[4]*rem.list[[i]]$mslp[j] +
                                  nhpp.st[[i]]$coefficients[5]*rem.list[[i]]$u250[j] +
                                  nhpp.st[[i]]$coefficients[6]*rem.list[[i]]$u850[j] +
                                  nhpp.st[[i]]$coefficients[7]*rem.list[[i]]$v250[j] +
                                  nhpp.st[[i]]$coefficients[8]*rem.list[[i]]$v850[j] +
                                  nhpp.st[[i]]$coefficients[9]*rem.list[[i]]$dewpoint[j])
  }
  progress(i, length(nhpp.st))
}

# #st.coef <- data.frame(matrix(NA, nrow = length(nhpp.st), ncol = 41))
# for (i in 1:length(nhpp.st)) {
#   st.coef[i,1] <- summary(nhpp.st[[i]])$coefficients[1,1]
#   st.coef[i,2] <- summary(nhpp.st[[i]])$coefficients[1,2]
#   st.coef[i,3] <- summary(nhpp.st[[i]])$coefficients[1,4]
#   st.coef[i,4] <- summary(nhpp.st[[i]])$coefficients[2,1]
#   st.coef[i,5] <- summary(nhpp.st[[i]])$coefficients[2,2]
#   st.coef[i,6] <- summary(nhpp.st[[i]])$coefficients[2,4]
#   st.coef[i,7] <- summary(nhpp.st[[i]])$coefficients[3,1]
#   st.coef[i,8] <- summary(nhpp.st[[i]])$coefficients[3,2]
#   st.coef[i,9] <- summary(nhpp.st[[i]])$coefficients[3,4]
#   st.coef[i,10] <- summary(nhpp.st[[i]])$coefficients[4,1]
#   st.coef[i,11] <- summary(nhpp.st[[i]])$coefficients[4,2]
#   st.coef[i,12] <- summary(nhpp.st[[i]])$coefficients[4,4]
#   st.coef[i,13] <- summary(nhpp.st[[i]])$coefficients[5,1]
#   st.coef[i,14] <- summary(nhpp.st[[i]])$coefficients[5,2]
#   st.coef[i,15] <- summary(nhpp.st[[i]])$coefficients[5,4]
#   st.coef[i,16] <- summary(nhpp.st[[i]])$coefficients[6,1]
#   st.coef[i,17] <- summary(nhpp.st[[i]])$coefficients[6,2]
#   st.coef[i,18] <- summary(nhpp.st[[i]])$coefficients[6,4]
#   st.coef[i,19] <- summary(nhpp.st[[i]])$coefficients[7,1]
#   st.coef[i,20] <- summary(nhpp.st[[i]])$coefficients[7,2]
#   st.coef[i,21] <- summary(nhpp.st[[i]])$coefficients[7,4]
#   st.coef[i,22] <- summary(nhpp.st[[i]])$coefficients[8,1]
#   st.coef[i,23] <- summary(nhpp.st[[i]])$coefficients[8,2]
#   st.coef[i,24] <- summary(nhpp.st[[i]])$coefficients[8,4]
#   st.coef[i,25] <- summary(nhpp.st[[i]])$coefficients[9,1]
#   st.coef[i,26] <- summary(nhpp.st[[i]])$coefficients[9,2]
#   st.coef[i,27] <- summary(nhpp.st[[i]])$coefficients[9,4]
#   st.coef[i,28] <- summary(nhpp.st[[i]])$coefficients[10,1]
#   st.coef[i,29] <- summary(nhpp.st[[i]])$coefficients[10,2]
#   st.coef[i,30] <- summary(nhpp.st[[i]])$coefficients[10,4]
#   st.coef[i,31] <- summary(nhpp.st[[i]])$coefficients[11,1]
#   st.coef[i,32] <- summary(nhpp.st[[i]])$coefficients[11,2]
#   st.coef[i,33] <- summary(nhpp.st[[i]])$coefficients[11,4]
#   st.coef[i,34] <- summary(nhpp.st[[i]])$coefficients[12,1]
#   st.coef[i,35] <- summary(nhpp.st[[i]])$coefficients[12,2]
#   st.coef[i,36] <- summary(nhpp.st[[i]])$coefficients[12,4]
#   st.coef[i,37] <- summary(nhpp.st[[i]])$coefficients[13,1]
#   st.coef[i,38] <- summary(nhpp.st[[i]])$coefficients[13,2]
#   st.coef[i,39] <- summary(nhpp.st[[i]])$coefficients[13,4]
#   st.coef[i,40] <- summary(nhpp.st[[i]])$aic
#   st.coef[i,41] <- ((nhpp.st[[i]]$null.deviance - nhpp.st[[i]]$deviance) / nhpp.st[[i]]$null.deviance) * 100
#   progress(i, length(nhpp.st))
# }



st.coef <- data.frame(matrix(NA, nrow = length(nhpp.st), ncol = 29))
for (i in 1:length(nhpp.st)) {
  st.coef[i,1] <- summary(nhpp.st[[i]])$coefficients[1,1]
  st.coef[i,2] <- summary(nhpp.st[[i]])$coefficients[1,2]
  st.coef[i,3] <- summary(nhpp.st[[i]])$coefficients[1,4]
  st.coef[i,4] <- summary(nhpp.st[[i]])$coefficients[2,1]
  st.coef[i,5] <- summary(nhpp.st[[i]])$coefficients[2,2]
  st.coef[i,6] <- summary(nhpp.st[[i]])$coefficients[2,4]
  st.coef[i,7] <- summary(nhpp.st[[i]])$coefficients[3,1]
  st.coef[i,8] <- summary(nhpp.st[[i]])$coefficients[3,2]
  st.coef[i,9] <- summary(nhpp.st[[i]])$coefficients[3,4]
  st.coef[i,10] <- summary(nhpp.st[[i]])$coefficients[4,1]
  st.coef[i,11] <- summary(nhpp.st[[i]])$coefficients[4,2]
  st.coef[i,12] <- summary(nhpp.st[[i]])$coefficients[4,4]
  st.coef[i,13] <- summary(nhpp.st[[i]])$coefficients[5,1]
  st.coef[i,14] <- summary(nhpp.st[[i]])$coefficients[5,2]
  st.coef[i,15] <- summary(nhpp.st[[i]])$coefficients[5,4]
  st.coef[i,16] <- summary(nhpp.st[[i]])$coefficients[6,1]
  st.coef[i,17] <- summary(nhpp.st[[i]])$coefficients[6,2]
  st.coef[i,18] <- summary(nhpp.st[[i]])$coefficients[6,4]
  st.coef[i,19] <- summary(nhpp.st[[i]])$coefficients[7,1]
  st.coef[i,20] <- summary(nhpp.st[[i]])$coefficients[7,2]
  st.coef[i,21] <- summary(nhpp.st[[i]])$coefficients[7,4]
  st.coef[i,22] <- summary(nhpp.st[[i]])$coefficients[8,1]
  st.coef[i,23] <- summary(nhpp.st[[i]])$coefficients[8,2]
  st.coef[i,24] <- summary(nhpp.st[[i]])$coefficients[8,4]
  st.coef[i,25] <- summary(nhpp.st[[i]])$coefficients[9,1]
  st.coef[i,26] <- summary(nhpp.st[[i]])$coefficients[9,2]
  st.coef[i,27] <- summary(nhpp.st[[i]])$coefficients[9,4]
  st.coef[i,28] <- summary(nhpp.st[[i]])$aic
  st.coef[i,29] <- 1 - (nhpp.st[[i]]$deviance/nhpp.st[[i]]$null.deviance)
  progress(i, length(nhpp.st))
}

setwd("~/Documents/CLEX Summer Research/East Coast/Outputs")
saveRDS(st.df, "st_lambda_12000.rds")
saveRDS(st.coef, "st_coef_12000.rds")
rm(st.df, nhpp.st, i, j, start, end, missing, st.coef)


endscript <- Sys.time()

## Come Back Later
set.seed(30567092)
lasso.fun <- function(data, rem) {

fitted.df <- data.frame(matrix(NA, nrow = 30316, ncol = length(data)))
coef.df <- data.frame(matrix(NA, nrow = length(data), ncol = 13))

for (i in 1:length(data)) {
y <- as.matrix(data[[i]][,4])
x <- as.matrix(data[[i]][,5:15])


lasso <- cv.glmnet(x, y, family = "poisson", standardize = FALSE,
               intercept = TRUE, standardize.response = FALSE)

minerror <- which.min(lasso$cvm)
lambda <- lasso$lambda[minerror]
deviance <- lasso$glmnet.fit$dev.ratio[minerror]
intercept <- lasso$glmnet.fit$a0[minerror]
cos <- lasso$glmnet.fit$beta[1,minerror]
sin <- lasso$glmnet.fit$beta[2,minerror]
mslp <- lasso$glmnet.fit$beta[3,minerror]
u250 <- lasso$glmnet.fit$beta[4,minerror]
u850 <- lasso$glmnet.fit$beta[5,minerror]
v250 <- lasso$glmnet.fit$beta[6,minerror]
v850 <- lasso$glmnet.fit$beta[7,minerror]
dewpoint <- lasso$glmnet.fit$beta[8,minerror]
soi <- lasso$glmnet.fit$beta[9,minerror]
dmi <- lasso$glmnet.fit$beta[10,minerror]
anomaly <- lasso$glmnet.fit$beta[11, minerror]
coefficients <- c(cos, sin, mslp, u250,
                  u850, v250, v850,
                  dewpoint, soi,
                  dmi, anomaly)

date <- data[[i]] %>%
    select(date)
  fitted.df[,i] <- date %>%
    mutate(fitted = exp(intercept + x %*% coefficients)) %>%
    complete(date = seq.Date(min(date), max(date), by = "day")) %>%
    select(fitted)

rem.x <- as.matrix(rem[[i]][,5:15])

missing <- which(is.na(fitted.df[,i]))
for (j in 1:length(missing)) {
  fitted.df[missing[j], i] <- exp(intercept +
                                  cos*rem.x[j,1] +
                                  sin*rem.x[j,2] +
                                  mslp*rem.x[j,3] +
                                  u250*rem.x[j,4] +
                                  u850*rem.x[j,5] +
                                  v250*rem.x[j,6] +
                                  v850*rem.x[j,7] +
                                  dewpoint*rem.x[j,8] +
                                  soi*rem.x[j,9] +
                                  dmi*rem.x[j,10] +
                                  anomaly*rem.x[j,11])
}

coef.df[i,] <- c(intercept, coefficients, deviance)
progress(i, length(data))
}
fitted.df <- fitted.df %>%
  mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day")) %>%
  relocate(date)

return(list(fitted.df, coef.df))
}

library(glmnet)
library(svMisc)

start <- Sys.time()
lasso.mod <- lasso.fun(df, rem.list)
lasso_lambda <- lasso.mod[[1]]
lasso_coef <- lasso.mod[[2]]
end <- Sys.time()

saveRDS(lasso_lambda, "lasso_lambda_12000.rds")
saveRDS(lasso_coef, "lasso_coef_12000.rds")
  
 
