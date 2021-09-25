#' Returns SIRD configurations for model 'twitter_sird' that use tweets and 
#' not. ODE solver not specified
#' @param run_df The dataframe that is being copied and built up.
model_stan_baseline <- function(run_df) {

  #model_no_tweets_df <- copy(run_df)
  #model_no_tweets_df$apply_twitter_data <- 0
  #model_no_tweets_df$description <- paste0(model_no_tweets_df$description,
  #                                         " no tweets")
  model_df <- copy(run_df)
  model_df$dir_name <- paste(model_df$dir_name, 'baseline', sep = '_')
  model_df$apply_twitter_data <- 1
  model_df$description <- paste0(model_df$description,
                                      " use tweets")
  model_df$compute_likelihood <- 1
  model_df$ode_solver <- 'block'
  model_df$model_to_run <- 'baseline'
  return(model_df)
}

#' Returns SIRD configurations for model 'twitter_sird' that use tweets and 
#' not with negative binomial likelihood. ODE solver not specified
#' @param run_df The dataframe that is being copied and built up.
model_stan_baseline_negbin <- function(run_df) {
  
  #model_no_tweets_df <- copy(run_df)
  #model_no_tweets_df$apply_twitter_data <- 0
  #model_no_tweets_df$description <- paste0(model_no_tweets_df$description,
  #                                         " no tweets")
  model_df <- copy(run_df)
  model_df$dir_name <- paste(model_df$dir_name, 'baseline_negbin', sep = '_')
  model_df$apply_twitter_data <- 1
  model_df$description <- paste0(model_df$description,
                                 " use tweets")
  model_df$compute_likelihood <- 1
  model_df$ode_solver <- 'block'
  model_df$model_to_run <- 'baseline_negbin'
  return(model_df)
}

#' Returns SIRD configurations for model 'twitter_sird' that use tweets and 
#' not with negative binomial likelihood. ODE solver not specified
#' @param run_df The dataframe that is being copied and built up.
model_stan_linear_reg <- function(run_df) {
  model_df <- copy(run_df)
  model_df$dir_name <- paste(model_df$dir_name, 'linear_reg', sep = '_')
  model_df$apply_twitter_data <- 0
  model_df$description <- paste0(model_df$description,
                                 " linear_reg")
  model_df$compute_likelihood <- 1
  model_df$model_to_run <- 'linear_reg'
  model_df$truncate_data <- 0
  return(model_df)
}


#' Returns SIRD configurations for model 'twitter_sird' that use tweets and 
#' not. ODE solver not specified
#' @param run_df The dataframe that is being copied and built up.
model_stan_UNINOVE_Brazil <- function(run_df) {
  
  model_df <- copy(run_df)
  model_df$dir_name <- paste(model_df$dir_name, 'uninove', sep = '_')
  model_df$compute_likelihood <- 1
  model_df$apply_twitter_data <- 1
  model_df$model_to_run <- 'UNINOVE_Brazil'
  model_df$description <- paste0(model_df$description,
                                        " UNINOVE_Brazil")
  return(model_df)
}