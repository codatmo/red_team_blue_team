library(data.table)

source(here::here("R","util.R"))
source(here::here("R","SIRTDsim.R"))

# Each run is a row in runDf, the rows contain all information necessary to run
# an experiment. The data in the columns can vary based on whether a sim is being
# run, real data etc. 
# Brazil data

#' reads data saved in the format produced by https://blooming-lake-98194.herokuapp.com/
#' Does not add any internal state info, just tweets and deaths (D)
#' @return dataframe for use in run_eval.R

data_web_app <- function(source_df, file_name) {
  data_df <- read.csv(here::here(file_name))
  run_df <- copy(source_df)
  run_df$n_pop <- sum(data[1,])
  run_df$n_days = nrow(data_df)
  run_df$n_patient_zero <- data_df[1,]$I
  run_df$description <- paste0("Web App: ", file_name)
  run_df$sim_run_id <- 0
  run_df$d <- list(data_df$D)
  run_df$tweets <- list(data_df$tweets)
  run_df$reports <- list(c('graph_data'))
  return(run_df)
}


#' 
#' 
#' @return dataframe for use in runEval.R
data_brazil_1 <- function (source_df) {
  brazil_df <- readRDS(here::here("data","brazil_nation_2020.rds"))
  brazil_df <- brazil_df[-(1:20),] #start with first death
  brazil_pop <- 214110287
  tweets = read.csv(here::here("data","tweet_count.csv"))
  colnames(tweets) = c('date_t','predicted_tweets')
  
  tweets_padded = rbind(data.frame(date_t = rep(NA,86), 
                                  predicted_tweets = rep(0,86)), 
                       tweets)
  brazil_df = cbind(brazil_df,tweets_padded[1:291,])

  run_df <- copy(source_df)
  run_df$n_pop <- brazil_pop
  run_df$n_days = nrow(brazil_df)
  run_df$n_patient_zero <- brazil_df[1,]$new_confirmed
  run_df$description <- "Brazil 3-16-2020 - 12-31-2020"
  run_df$sim_run_id <- 0
  run_df$d <- list(brazil_df$last_available_deaths)
  run_df$tweets <- list(brazil_df$predicted_tweets) #predicted by classifier
  run_df$n_patient_zero <- 100 # assumes 1% death rate, 
  return(run_df)
}

# 214,110,287
# run_df <- setup_run_df(seed = 123, n_pop = 214110287, n_days = 365)
# run_df <- data_web_app(run_df, file_name = "data/web_app_1.csv")
# sim_brazil_1(run_df)
