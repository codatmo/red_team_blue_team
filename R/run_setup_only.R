#todo
# serialize run_df?? json?
# dependencies
# Figure out why UNINOVE model is so much better than baseline?
# Tweets vs no-tweets
# Explore performance against non-isomorphic models
# Store runs on disk
# Vary infection 


library(tidyverse)
#library(cmdstanr)
library(data.table)
library(kableExtra)
#library(shinystan)
library(rjson)

#set_cmdstan_path("/home/breck/.cmdstanr/cmdstan-2.27.0")
source(here::here("R","util.R"))
source(here::here("R","sim_configs.R"))
source(here::here("R", "data_configs.R"))
source(here::here("R","modeling_configs.R"))
setup_run_df <- setup_run_df(seed = 93435, n_pop = 214110287, n_days = 291) # in R/util.R

iso_basic_df <- sim_SIRD_easy(setup_run_df) # in R/sim_configs.R
iso_brazil_df <- sim_SIRD_Brazil(setup_run_df)
brazil_df <- data_brazil_1(setup_run_df)
#draws_df <- sim_draw_params_sird(n_sims = 20, iso_basic_df)
iso_draws_df <- rbind(iso_basic_df,iso_brazil_df)

run_df_brazil <- model_stan_baseline(brazil_df) #in R/modeling_configs.R
#run_df_brazil <- copy_run(run_df_brazil, '140heldOut')
run_df_brazil$ode_solver <- 'rk45'
run_df_brazil <- copy_run(run_df_brazil, 'rk45')
#run_df_brazil <- model_stan_UNINOVE_Brazil(brazil_df)

run_df_brazil_no_tweets <- copy_run(run_df_brazil, 'noTweets')
run_df_brazil_no_tweets$use_tweets <- 0
run_df_brazil_use_tweets <- copy_run(run_df_brazil, 'tweets')
run_df_brazil_use_tweets$use_tweets <- 1

run_df <- rbind(run_df_brazil_no_tweets, run_df_brazil_use_tweets)

run_df$reports <- list(c(''))

run_df$compute_likelihood <- 1
run_df <- copy_run(run_df,'non_centered')
run_stan = TRUE

j <- 0
while (j < nrow(run_df)) {
  tryCatch({
  j <- j + 1

  fit <- NA
  if (dir.exists(here::here("output",run_df[j,]$dir_name))) {
     print(paste("Deleting directory", 
                 here::here("output", run_df[j,]$dir_name)))
     unlink(here::here("output",run_df[j,]$dir_name), recursive = TRUE)
  }
  dir.create(here::here("output",run_df[j,]$dir_name))
  #read json
  if (run_df[j,]$model_to_run == 'baseline') {
    stan_data <-
      list(n_days = run_df[j,]$n_days,
           sDay1 = run_df[j,]$n_pop - 1,
           iDay1 = 1,
           rDay1 = 0,
           dDay1 = 0,
           Npop = run_df[j,]$n_pop,
           tweets = unlist(run_df[j,]$tweets),
           deaths = unlist(run_df[j,]$d),
           compute_likelihood = run_df[j,]$compute_likelihood,
           run_twitter = run_df[j,]$use_tweets,
           run_block_ODE = ifelse(run_df[j,]$ode_solver == 'block', 1, 0),
           run_rk45_ODE = ifelse(run_df[j,]$ode_solver == 'rk45', 1, 0),
           scale = 0,
           center = 0,
           prior_beta_mean = .3,
           prior_beta_std = .2,
           prior_gamma_mean = .3,
           prior_gamma_std = .2,
           prior_death_prob = .02,
           prior_death_prob_std = .005,
           prior_twitter_lambda = 1.0,
           prior_twitter_std = 1.0,
           days_held_out = 0,
           I2DandR = 0,
           I2D2R = 1,
           debug = 0)
    write(toJSON(run_df[j,]), here::here("output",run_df[j,]$dir_name, 
                                         "config.json"))
    data = here::here("output",run_df[j,]$dir_name, "data.json")
    output = here::here("output",run_df[j,]$dir_name)
    stan_exe = here::here("stan/baseline")
    write(toJSON(stan_data), data)
    if (run_stan) {
        for (i in 1:4) {
          out_file = paste0(output, "/out.", i, ".csv")
          system2(stan_exe, 
              args=paste0("sample data file=", data, " output file=", out_file),
              wait = (i == 4))
        }
    }
  }
  }
)}
