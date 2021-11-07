# todo
# serialize run_df?? json?
# rename randomization to jitter
# view in jupyter
# Tweets vs no-tweets
# Explore performance against non-isomorphic models
# Daily variation of infection
# Sort out diagnostics on baseline



library(tidyverse)
library(cmdstanr)
library(data.table)
library(kableExtra)
library(shinystan)

set_cmdstan_path("/home/breck/.cmdstanr/cmdstan-2.27.0")
source(here::here("R","util.R"))
source(here::here("R","sim_configs.R"))
source(here::here("R", "data_configs.R"))
source(here::here("R","modeling_configs.R"))
r1 <- setup_run_df(seed = 93435, n_pop = 214110287, n_days = 291) # in R/util.R
#iso_basic_df <- sim_SIRD_easy(setup_run_df) # in R/sim_configs.R
#r2 <- data_brazil_1(r1)
r2_sim <- sim_Brazil2020(r1)
#r2_drawn <- sim_draw_SIRD(r2_sim)
#draws_df <- sim_draw_params_sird(n_sims = 20, iso_basic_df)
#iso_draws_df <- rbind(iso_basic_df,draws_df)

#r3 <- model_stan_linear_reg(r2_drawn) #in R/modeling_configs.R
r3_baseline <- model_stan_baseline(r2_sim)
r3_baseline$use_tweets <- 0
r4 <- copy_run(r3_baseline,'140_trunc')
r4$truncate_data <- 140
#r5 <- rbind(r3_baseline, r4)
#run_df_brazil$ode_solver <- 'block'
#run_df_brazil <- model_stan_UNINOVE_Brazil(brazil_df)

#r6 <- copy_run(r5,'tweets')
#r6$use_tweets <- 1
#r7 <- copy_run(r5,'no_tweets')
#7$use_tweets <- 0



run_df <- r3_baseline

run_df$reports <- list(c('graph_data', 'plot_pred_death_draws'))
run_df$compute_likelihood <- 1

j <- 0
while (j < nrow(run_df)) {
  j <- j + 1
  fit <- NA
  if (dir.exists(here::here("output",run_df[j,]$dir_name))) {
    print(paste("Deleting directory", 
                here::here("output", run_df[j,]$dir_name)))
    unlink(here::here("output",run_df[j,]$dir_name), recursive = TRUE)
  }
  dir.create(here::here("output",run_df[j,]$dir_name))
  if (run_df[j,]$model_to_run == 'linear_reg') {
    stan_data <-
      list(n_days = run_df[j,]$n_days,
           tweets = unlist(run_df[j,]$tweets),
           deaths = unlist(run_df[j,]$d),
           compute_likelihood = run_df[j,]$compute_likelihood,
           use_tweets = run_df[j,]$use_tweets,
           scale = 1,
           days_held_out = run_df[j,]$truncate_data,
           debug = 0)
    model <- cmdstan_model(here::here("stan", "linear_reg.stan"))
    write(toJSON(run_df[j,]), here::here("output",run_df[j,]$dir_name, 
                                         "config.json"))
    data = here::here("output",run_df[j,]$dir_name, "data.json")
    write(toJSON(stan_data), data)
    fit <- model$sample(data=stan_data,
                        output_dir = here::here("output/",run_df[j,]$dir_name),
                        parallel_chains = 4,
                        iter_warmup = 1000,
                        iter_sampling = 1000,
                        chains = 4,
                        seed = 4857)
    run_df[j,]$fit = list(fit)
  }
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
    model <- cmdstan_model(here::here("stan", "SIRD.stan"))
    write(toJSON(run_df[j,]), here::here("output",run_df[j,]$dir_name, 
                                         "config.json"))
    data = here::here("output",run_df[j,]$dir_name, "data.json")
    write(toJSON(stan_data), data)
    fit <- model$sample(data=stan_data,
                        output_dir = here::here("output/",run_df[j,]$dir_name),
                        parallel_chains = 4,
                        iter_warmup = 1000,
                        iter_sampling = 1000,
                        chains = 4,
                        seed = 4857)
    run_df[j,]$fit = list(fit)
  }
  else if (run_df[j,]$model_to_run == 'UNINOVE_Brazil') {
    stan_data_2 <- list(n_days = run_df[j,]$n_days,
                        y0 = c(run_df[j,]$n_pop - run_df[j,]$n_patient_zero,
                               run_df[j,]$n_patient_zero, 0, 0, 0), # one more zero here
                        t0 = 0,
                        ts = 1:run_df[j,]$n_days,
                        compute_likelihood = run_df[j,]$compute_likelihood,
                        use_twitter = run_df[j,]$apply_twitter_data,
                        death_count = unlist(run_df[j,]$d),
                        symptomaticTweets = unlist(run_df[j,]$tweets),
                        prior_beta_mean = 0,
                        prior_beta_std = 10,
                        prior_omega_mean = 0,
                        prior_omega_std = 10,
                        prior_dI_mean = 0,
                        prior_dI_std = 10,
                        prior_dT_mean = 0,
                        prior_dT_std = 10,
                        prior_twitter_lambda = 1,
                        days_held_out = 0
                        )
    model2 <- cmdstan_model(here::here("stan", "tweet_sirtd_negbin_ODE.stan"))
    fit <- model2$sample(data=stan_data_2,
                         parallel_chains = 4,
                         iter_warmup = 1000,
                         iter_sampling = 1000,
                         chains = 4,
                         seed = 4857)
  }
  #if (run_df[j,]$model_to_run != 'none') {
    #d_tweets_in_interval = countPredictionsInQuantile(fit = fit,
    #                                                  run_df = run_df,
    #                                                  j = j, print = TRUE)
    #run_df[j,]$d_in_interval = d_tweets_in_interval[1]
    #run_df[j,]$tweets_in_interval = d_tweets_in_interval[2]
  #}
# run models
  #else {
   # print(sprintf("no model selected, got:'%s'",run_df[j,]$model_to_run));
  #}
# section 6
  plot <- ggplot(data = NULL, aes(x = day, y = count))
  if ('graph_data' %in% unlist(run_df[j,]$reports)) {
    plot <- graph_real_data(data_df = run_df[j,], plot = plot)
  }
  if ('graph_sim' %in% unlist(run_df[j,]$reports)) {
    plot <- graph_sim_data(data_df = run_df[j,], hide_s = TRUE, plot = plot)

  }
  if ('graph_ODE' %in% unlist(run_df[j,]$reports)) {
    plot <- graph_ODE(data_df = run_df[j,], fit = fit, hide_s = FALSE,
                             plot = plot)
  }
  if ('plot_pred_death_draws' %in% unlist(run_df[j,]$reports)) {
    plot <- plot_draws(plot = plot, variable = 'pred_deaths', n_columns = run_df[j,]$n_days, color = 'blue', fit = fit)
  }
  if ('plot_pred_tweets_draws' %in% unlist(run_df[j,]$reports)) {
    plot <- plot_draws(plot = plot, variable = 'pred_tweets', 
                       n_columns = run_df[j,]$n_days, color = 'red', fit = fit)
  }
  if ('graph_pred_tweets' %in% unlist(run_df[j,]$reports)) {
    plot <- plot_predictions(plot = plot, prediction_label = 'pred_tweets',
                             fit = fit,
                             show_ribbon = TRUE)
  }
  if ('graph_pred_deaths' %in% unlist(run_df[j,]$reports)) {
    plot <- plot_predictions(plot = plot, prediction_label = 'pred_deaths',
                             fit = fit,
                             show_ribbon = TRUE)
  }

  plot = plot + xlim(0, 400) + 
    # ylim(0, 200000) + 
    theme(legend.position = "none")
  
  if (length(unlist(run_df[j,]$reports)) > 0) {
    print(plot)
  }
# section 6
# section 7
  if ('param_recovery' %in% run_df[j,]$reports) {
    cat(param_recovery(data_df = run_df[j,], fit = fit))
  }
# section 7
}
# section 8
summary_cols = c('sim_run_id', 'model_to_run', 'beta_mean', 'gamma', 'death_prob',
                 'tweet_rate', 'days2death', 'description',
                 'd_in_interval', 'tweets_in_interval','n_days')
print(run_df[,summary_cols])
# section 8
