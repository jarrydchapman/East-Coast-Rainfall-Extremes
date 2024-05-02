setwd("~/Documents/CLEX Summer Research/East Coast/Outputs/Coefficients")

hp <- readRDS("hp_coef.rds")
harm <- readRDS("harm_coef.rds")
lt <- readRDS("temp_lt_coef.rds")
st <- readRDS("st_coef.rds")
lasso <- readRDS("lasso_coef.rds")

# Deviance
deviance <- cbind(harm[ncol(harm)],
                  lt[ncol(lt)],
                  st[ncol(st)],
                  lasso[ncol(lasso)])
names(deviance) <- c("harm_dev", "lt_dev", "st_dev", "lasso_dev")
deviance <- deviance %>%
  as_tibble()


# Significance
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

st95 <- sig.coef.fun(st, 0.05)
names(st95) <- c("intercept", "cos", "sin", "mslp", "u250", "u850",
                 "v250", "v850", "dewpoint")
st99 <- sig.coef.fun(st, 0.01)
names(st99) <- c("intercept", "cos", "sin", "mslp", "u250", "u850",
                 "v250", "v850", "dewpoint")

# Lasso Shrinkage
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
