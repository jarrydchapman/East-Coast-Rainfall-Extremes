library(tidyverse)
library(RcppRoll)
library(svMisc)

hp <- readRDS("hp_lambda.rds")[,2:11774]
harm <- readRDS("harm_lambda.rds")[,2:11774]
st <- readRDS("st_lambda.rds")[,2:11774]
lt <- readRDS("lt_lambda.rds")[,2:11774]
ensoiod <- readRDS("ensoiod_lambda.rds")[,1:11774]
lasso <- readRDS("lasso_lambda.rds")[,2:11774]
ext <- readRDS("extremes.rds")
dc <- readRDS("declustered.rds")


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

dc15 <- roll.fun(dc, 90)
hp15 <- roll.fun(hp, 90)
harm15 <- roll.fun(harm, 90)
lt15 <- roll.fun(lt, 15)
st15 <- roll.fun(st, 15)
lasso15 <- roll.fun(lasso, 90)

dc2022 <- dc[29587:30163,]
lasso2022 <- lasso[29587:30163,]


prob <- data.frame(matrix(NA, nrow = 11773, ncol = 8))

dcsum <- dc2022 %>%
  summarise_all(sum)
dcsum <- t(dcsum)

lassosum <- lasso2022 %>%
  summarise_all(sum) %>%
  t()



for (i in 1:11773) {
  prob[i,2] <- lasso15[28222, i+1]
}
for (i in 1:11773) {
  prob[i,4] <- lasso15[29676, i+1]
}
for (i in 1:11773) {
  prob[i,6] <- lasso15[30019, i+1]
}
for (i in 1:11773) {
  prob[i,8] <- lasso15[30163, i+1]
}





names(prob)[1] <- "dc"
names(prob)[2] <- "lasso"
names(prob)[3] <- "harm"
names(prob)[4] <- "lasso"







coords <- coords %>%
  as_tibble() %>%
  mutate(dcprob = prob$dc,
         hpprob = prob$hp,
         harmprob = prob$harm,
         lassoprob = prob$lasso)


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



prob.df.fun <- function(data, k) {
  prob.df <- data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)-1))
  for (i in 2:ncol(data)) {
    prob.df[,i-1] <- prob.fun(k, data[,i])
    progress(i-1, ncol(data))
  }
  prob.df <- prob.df %>%
    as_tibble() %>%
    mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day")) %>%
    relocate(date)
  
  return(prob.df)
}

hp_prob <- prob.df.fun(hp15, 2)
harm_prob <- prob.df.fun(harm15, 2)
lasso_prob <- prob.df.fun(lasso15, 2)

dcprob <- prob.fun(5, dcsum)
lassoprob <- prob.fun(5, lassosum)

coords <- coords %>%
  mutate(dc22 = dcsum,
         lasso22 = lassoprob)
