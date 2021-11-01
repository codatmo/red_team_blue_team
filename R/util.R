library(ggplot2)
library(ggrepel)
library(tidyverse)


#' Count the number of truth data points contained in the quantile interval of
#' model fit
#' @param truth vector of true or simulated values
#' @param fitPredDf dataframe with quantiles specified
#' 
countPredInInterval <- function(truth = truth, fitPredDf = fitPredDf,
                                maxQuantileLabel = maxQuantileLabel,
                                minQuantileLabel = minQuantileLabel) {
  actualsCovered = 0
  for (day in 1:length(truth)) {
    if (truth[day] <= fitPredDf[day,][[maxQuantileLabel]] &&
        truth[day] >= fitPredDf[day,][[minQuantileLabel]]) {
      actualsCovered = actualsCovered + 1
    }
  }
  return(actualsCovered)
} 

#' Take a fit, a run_df and the run index and compute the number of predicted
#' d and tweets are in the .2 to .8 central interval
#' @param fit cmdstanR fit object
#' @param run_df a dataframe setup for running models
#' @param j the current row being run
#' @param print boolean to control whether function prints, defaults to FALSE
#' 
countPredictionsInQuantile <- function(fit, run_df, j, print = FALSE) {
    minQuantile = .2
    maxQuantile = .8
    minQuantileLabel = '20%'
    maxQuantileLabel = '80%'
  
    predCasesDf = fit$summary(variables = c('pred_deaths'), mean,
                            ~quantile(.x, probs = c(minQuantile, maxQuantile),
                                      na.rm = TRUE))  #set to FALSE when Jose fixes his model
    predCasesDf$day = 1:nrow(predCasesDf)
  
    predTweetsDf = fit$summary(variables = c('pred_tweets'), mean,
                             ~quantile(.x, probs = c(minQuantile, maxQuantile),
                                       na.rm = TRUE))  #set to FALSE when Jose fixes his model
    predTweetsDf$day = 1:nrow(predTweetsDf)
  
    deaths_in_interval = countPredInInterval(truth = unlist(run_df[j,]$d),
                                                  fitPredDf = predCasesDf,
                                                  maxQuantileLabel = maxQuantileLabel,
                                                  minQuantileLabel = minQuantileLabel)
  
    tweets_in_interval = countPredInInterval(truth = unlist(run_df[j,]$tweets),
                                                   fitPredDf = predTweetsDf,
                                                   maxQuantileLabel = maxQuantileLabel,
                                                   minQuantileLabel = minQuantileLabel)
  
    if (print) {
      days_p = sprintf("Over %d days, %d predicted deaths", run_df[j,]$n_days,
                        deaths_in_interval)
      i = sprintf(" were in the %.1f and %.1f quantiles of the truth sd",
                    minQuantile, maxQuantile)
      tweets_p = sprintf("\n%d predicted tweets,", tweets_in_interval)
      i_2 = sprintf(" were within %.1f and %.1f of truth",
                    minQuantile, maxQuantile)
      cat(paste0(days_p, i, tweets_p, i_2))
    }
    return(c(deaths_in_interval, tweets_in_interval))
  }

#' Graph internal state counts for simulation and corresponding tweets for a run
#' of the runEval framework. Returns a ggplot geom_point element with x = days
#' y = count.
#' @param data_df one row of the run_df with simulation data added
#' @param hide_s Boolean to control whether to hide the s or susceptible counts
#' @param plot ggplot plot to add to
#' @return ggplot with SIRD states per day added
graph_sim_data <- function(data_df, hide_s, plot) {
    if ('t' %in% data_df) { 
      sim_df = data.frame(day = 1:data_df$n_days, 
                        tweets = unlist(data_df$tweets), 
                        s = unlist(data_df$s),
                        i = unlist(data_df$i),
                        r = unlist(data_df$r),
                        t = unlist(data_df$t),
                        d = unlist(data_df$d))

      compartment_names <- c('s', 'i', 'r', 't', 'd')
    }
    else {
       sim_df = data.frame(day = 1:data_df$n_days, 
                        tweets = unlist(data_df$tweets), 
                        s = unlist(data_df$s),
                        i = unlist(data_df$i),
                        r = unlist(data_df$r),
                        d = unlist(data_df$d))
       compartment_names <- c('s', 'i', 'r', 'd')
    }
    if (hide_s) {
      compartment_names <- compartment_names[-1]
    }
    i_mean = mean(sim_df$i)
    gt_mean_days = sim_df[sim_df$i >= i_mean,]$day
    display_day = gt_mean_days[round(length(gt_mean_days)/2)]
    sim_long_df = gather(data = sim_df, key = "compartment_sim", value = "count",
                         all_of(c('tweets', compartment_names)))
    return(plot + 
             geom_point(data = sim_long_df, aes(y = count, 
                                                color = compartment_sim),
                      size = .5) + 
             geom_label_repel(data = subset(sim_long_df, 
                                            day == display_day), 
                              aes(label = compartment_sim,
                                  color = compartment_sim)))
    
}

#' Returns ggplot with geom_line() added with observed data for tweets and deaths
#' @param data_df dataframe with data
#' @param plot ggplot object to add to
#' @return ggplot with geom_line() added
graph_observed_data <- function(data_df, plot) {
  real_deaths_data_df <- data.frame(count = unlist(data_df$d), 
                             day = 1:data_df$n_days,
                             source = 'deaths')
  real_tweets_data_df <- data.frame(count = unlist(data_df$tweets), 
                                    day = 1:data_df$n_days,
                                    source = 'tweets')
  real_data_df <- rbind(real_deaths_data_df,real_tweets_data_df)
  return(plot + 
         geom_line(data = real_data_df, aes(y = count, 
                                            color = source),
                  size = .5) + 
         geom_label_repel(data = subset(real_data_df, 
                                        day == round(data_df$n_days)), 
                            aes(label = source,
                                color = source)))
}


#' Graph daily ODE means from SIRTD model. Returns a ggplot geom_line element
#' @param data_df A row from run_df
#' @param fit A fit object returned by cmdstanR with ode_states defined
#' @param hide_s Boolean to control whether to hide susceptible counts
graph_ODE <- function(data_df, fit, hide_s, plot) {
  compartment_names = c('S', 'I', 'R', 'D')
  if (hide_s) {
    compartment_names <- compartment_names[-1]
  }
  ODE_long_df = data.frame()
  for (name in compartment_names) {
    compartment_ODE_df = fit$summary(variables = c(name))
    compartment_ODE_df$compartment_ODE = name
    compartment_ODE_df$day = 1:nrow(compartment_ODE_df)
    compartment_ODE_df$count = compartment_ODE_df$mean  # maybe should be median
    ODE_long_df = rbind(ODE_long_df,compartment_ODE_df)
  }
  return(
    plot + 
      geom_line(data = ODE_long_df, aes(color = compartment_ODE)) + 
      geom_label_repel(data = subset(ODE_long_df, day == data_df$n_days), 
                              aes(label = compartment_ODE,
                                  color = compartment_ODE))
  )
}

#' returns ggplot geom_line with optional ribbon around 20/80 central interval
#' of predictions. 
#' @param plot ggplot that this is adding to
#' @param prediction_label the label for the prediciton in fit object
#' @param fit cmdstanR fit object
#' @param show_ribbon control whether to show ribbon, default = TRUE
plot_predictions <- function(plot, prediction_label, fit, show_ribbon = TRUE,
                             y_label = 'count') {
    minQuantile = .2
    maxQuantile = .8
    minQuantileLabel = '20%'
    maxQuantileLabel = '80%'
    pred_df = fit$summary(variables = c(prediction_label), mean,
                            ~quantile(.x, probs = c(minQuantile, maxQuantile),
                                      na.rm = TRUE))  #set to FALSE when Jose fixes his model
    pred_df$day = 1:nrow(pred_df)
    pred_df[[y_label]] = pred_df$mean
    pred_df$label = prediction_label
    

    plot = plot + geom_line(data = pred_df)
    if (show_ribbon) {
      plot <- plot + geom_ribbon(data = pred_df, aes(ymin = .data[[minQuantileLabel]],
                                              ymax = .data[[maxQuantileLabel]],
                                              fill = "blue"),
                          alpha = 0.3, fill = "blue")
            
    }
    plot <- plot + geom_label_repel(data = subset(pred_df, day == nrow(pred_df)), 
                              aes(label = label),
                              xlim = 300)
    return(plot)
}

#' Plots draws for variable from fit object
#' @param plot plot being added to, x=day X y=count
#' @param variable name of predicted variable to plot
#' @param n_columns number of days to select
#' @param n_draws number of draws, default 40
#' @param color color to plot
#' @param fit cmdStanR fit object
#' @param y_label string label to apply to y axis, default 'count'
#' @return ggplot geom_line 
plot_draws <- function(plot, variable, n_columns, n_draws = 40, color, fit, 
                       y_label = 'count') {

  drawsDf <- fit$draws(variables = c(variable), format = 'draws_df')
  sampleDrawsDf <- drawsDf[sample(nrow(drawsDf), n_draws, replace = FALSE),]
  colnames(sampleDrawsDf)[1:n_columns] <- 1:n_columns
  longSampleDrawsDf <- gather(sampleDrawsDf, key=day, value=variable,
                            1:all_of(n_columns))

  longSampleDrawsDf$day <- as.numeric(longSampleDrawsDf$day)
  longSampleDrawsDf[[y_label]] <- as.numeric(longSampleDrawsDf$variable)
  return( plot +
          geom_line(data = longSampleDrawsDf, alpha = 0.2, color = color, 
          aes(group = .draw)))
}

#' Returns string with calulation of parameter recovery
#' @param data_df row of run_df
#' @param fit cmdStanR fit object
#' @return string with fit info
param_recovery <- function(data_df, fit) {
  recov_pars = 
    fit$summary(variables = c('gamma', 'beta', 'deathRate', 'lambda_twitter'), 
                mean, sd)
    return(paste(sprintf("\nBeta sim = %.4f vs recovered %.4f, sd=%.4f",
            data_df$beta_mean, 
            recov_pars[recov_pars$variable == 'beta',]$mean,
            recov_pars[recov_pars$variable == 'beta',]$sd),
    sprintf("\nGamma sim = %.4f vs recovered %.4f, sd=%.4f",
            data_df$gamma, 
            recov_pars[recov_pars$variable == 'gamma',]$mean,
            recov_pars[recov_pars$variable == 'gamma',]$sd),
    sprintf("\nDeaths sim = %.4f vs recovered %.4f, sd=%.4f",
            data_df$death_prob, 
            recov_pars[recov_pars$variable == 'deathRate',]$mean,
            recov_pars[recov_pars$variable == 'deathRate',]$sd),
    sprintf("\nLambda Twitter sim = %.4f vs recovered %.4f, sd=%.4f",
            data_df$tweet_rate, 
            recov_pars[recov_pars$variable == 'lambda_twitter',]$mean,
            recov_pars[recov_pars$variable == 'lambda_twitter',]$sd),
    "\n"))
}

#' Generates the starting template for the dataframe that runs the experiments
#' @param seed The random seed for random processes in R
#' @param n_pop Population size, should be constant across runs
#' @param n_days Number of days to run, assumed to be constant across runs
#' @return dataframe appropriate for use in runEval.R
setup_run_df <- function(seed, n_pop, n_days) {
  # Set up our template, all runs need this info
  # Each row of dataframe is a separate run
  template_df <- data.frame(sim_run_id = c(NA))
  template_df$description <- NA
  template_df$seed <- seed
  template_df$dir_name <- "CoDatMo"

  # any simulation/model run
  template_df$n_pop <- n_pop
  template_df$n_days <- n_days

  # any model setup
  template_df$model_to_run <- 'none'
  template_df$compute_likelihood <- NA
  template_df$use_tweets <- NA
  template_df$truncate_data <- 0

  #setup data columns 
  template_df$s <- list(c())
  template_df$i <- list(c())
  template_df$r <- list(c())
  template_df$t <- list(c())
  template_df$d <- list(c())
  template_df$tweets <- list(c())

  #needed for sims
#  template_df$beta_daily_rate <- NA
  template_df$beta_mean <- NA
  template_df$gamma <- NA
  template_df$death_prob <- NA
  template_df$tweet_rate <- NA
  template_df$days2death <- NA
  
  # template_df$reports <- NA
  
  # setup prediction columns
  # template_df$d_in_interval <- NA_integer_
  # template_df$tweets_in_interval <- NA_integer_
  template_df$fit <- NA
  #set.seed(template_df$seed)
  return(template_df)
}

#' Does deep copy of run configuration and appends directory name with '_' 
#' separator
#' @param run_df data frame for run
#' @param dir_append_text text to append
#' @return deep copy with $dir_name appended as described
copy_run <- function(run_df, dir_append_text) {
  ret_df <- copy(run_df)
  ret_df$dir_name <- paste(ret_df$dir_name, dir_append_text, sep='_')
  return(ret_df)
}


#' Trims top level of data frame values to specified length in characters
#' and returns data frame with those truncated values. 
#' @param df data frame for run
#' @length length number of characters to trim to
#' @return data frame with all values trimed to length 
trim_for_printing <- function(df, length){
  return (df %>% mutate_all(~(strtrim(., length))))
}

#' Plots simulation data and original data with headings indicating the params
#' that generated the simulation
#' @param df_data dataframe with actual data (deaths/tweets)
#' @param df_sim dataframe with simulation data
#' @return ggplot object ready to plot with title
plot_sim <- function(df_data, df_sim) {
  plot <- ggplot(data = NULL, aes(x = day, y = count))
  plot <- graph_observed_data(data_df = df_data, plot = plot)
  plot <- graph_sim_data(data_df = df_sim, plot = plot, hide_s = TRUE)
  plot <- plot + xlim(0, 330) + 
    ggtitle(paste0("beta inf=", df_sim$beta_mean, 
                   ", gamma recov=", df_sim$gamma,
                   "\ndeath =", df_sim$death_prob, 
                   ", patient 0=", df_sim$n_patient_zero)) + 
    theme(legend.position = "none")
  return(plot)
}

#' Plots predictive check for prior or posterior depending on whether 
#' likelihood has been run against the actual data
#' @param df dataframe for actual data
#' @param fit cmdStanR fit object
#' @param n_draws how many draws to display
#' @return ggplot object ready to plot
plot_pred_check <- function(df, fit, n_draws) {
  plot <- ggplot(data = NULL, aes(x = day, y = count))
  plot <- plot_draws(plot = plot, variable = 'pred_deaths', n_draws = n_draws,
                     n_columns = df$n_days, 
                     'blue', fit)
  plot <- plot_draws(plot = plot, variable = 'pred_tweets', n_draws = n_draws,
                     n_columns = df$n_days, 
                     'red', fit)
  plot <- graph_observed_data(data_df = df, plot = plot)
  plot <- plot + xlim(0, 330) +
    theme(legend.position = "none") +
    ggtitle(df$description)
}