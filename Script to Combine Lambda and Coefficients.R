setwd("~/Documents/CLEX Summer Research/East Coast/Outputs")



d1 <- readRDS("lasso_lambda_3000.rds")[,2:3001]
d2 <- readRDS("lasso_lambda_6000.rds")[,2:3001]
d3 <- readRDS("lasso_lambda_9000.rds")[,2:3001]
d4 <- readRDS("lasso_lambda_12000.rds")[,2:2774]


lambda.df.fun <- function(df1, df2, df3, df4) {
  
lambda_df <- cbind(df1, df2, df3, df4)
colnames(lambda_df) <- c(paste0("V", 1:11773))
lambda_df <- lambda_df %>%
             as_tibble() %>%
             mutate(date = seq.Date(ymd("1940-01-01"), ymd("2022-12-31"), by = "day")) %>%
             relocate(date)

return(lambda_df)
}

lambda <- lambda.df.fun(d1, d2, d3, d4)

colnames(lambda) <- c(paste0("V", 1:11773))
which(is.na(lambda))

saveRDS(lambda, "lasso_lambda.rds")




coef.df.fun <- function(df1, df2, df3, df4) {
  
coef_df <- rbind(df1, df2, df3, df4)

return(coef_df)
}

coef <- coef.df.fun(d1, d2, d3, d4)

saveRDS(coef, "st_coef.rds")

