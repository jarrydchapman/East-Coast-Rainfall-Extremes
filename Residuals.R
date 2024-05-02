## Run Script for Intervals


mae.fun <- function(actual, fitted, size) {
  temp.actual <- actual[size:nrow(actual), 2:ncol(actual)]
  temp.fitted <- fitted[size:nrow(fitted), 2:ncol(fitted)]
  
  error.df <- abs(temp.actual - temp.fitted)
  
  rm(temp.actual, temp.fitted)
  gc()
  
  mae.df <- data.frame(matrix(NA, nrow = 11773, ncol = 1))
  
  for (i in 1:11773) {
    mae.df[i,1] <- sum(error.df[i]) / nrow(error.df[i])
    progress(i, 11773)
  }
  
  return(mae.df)
}

start <- Sys.time()
lasso15_mae <- mae.fun(dc15, lasso15, 90)
names(lasso15_mae)[1] <- "mae"
end <- Sys.time()


coords <- coords %>%
  as_tibble() %>%
  mutate("lasso90mae" = lasso15_mae$mae)

coords <- coords %>%
  mutate(hp90imp = (hp90mae / lasso90mae) - 1,
         harm90imp = (harm90mae / lasso90mae) - 1)
