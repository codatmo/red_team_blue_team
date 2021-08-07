# Run from command line: Rscript run.R
# If running from RStudio remember to set the working directory
# >Session>Set Working Directory>To Source File Location

# Simulate data with parameters we are trying to recover with one predictor
library(ggplot2)
set.seed(9999)
beta_coef_111_call_true <- rnorm(1,.3,.2)
alpha_intercept_true <- rnorm(1, mean=50, sd=20)
sigma_sd_true <- abs(rnorm(1,40,10))
n_days_data <- 100
x_calls_111 <- round(runif(n_days_data, 10, 1000))
y_cvd_hosp <- round(rnorm(n_days_data, 
                                  alpha_intercept_true + beta_coef_111_call_true * x_calls_111, 
                                  sigma_sd_true))

# plot(x_calls_111 ~ y_cvd_hosp_20_days_later)
# lm(y_cvd_hosp_20_days_later ~ x_calls_111)
# library(rstanarm)
# stan_glm(y_cvd_hosp_20_days_later ~ x_calls_111)


dump(c('n_days_data','x_calls_111', 'y_cvd_hosp'), file="data/data.R")
cat(sprintf(paste("simulation parameters are:",
                  "\nalpha_intercept_true=%.1f",
                  "\nbeta_coef_111_call_true=%.1f",
                  "\nsigma_sd_true=%.1f",
                  "\nn_days_data=%d\n"),
            alpha_intercept_true,
            beta_coef_111_call_true,
            sigma_sd_true,
            n_days_data))

data <- data.frame(x_calls_111,y_cvd_hosp)
ggplot(data) + aes(x=x_calls_111,y=y_cvd_hosp) + geom_point()

# simulation parameters are: 
# alpha_intercept_true=23.7 
# true_coefficient_111_call_count_beta=0.1 
# true_std_dev_sigma=71.8 
# days_data=10000
