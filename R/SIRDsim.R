
# SIRD_exact
#' @param n_pop Population size
#' @param n_days Number of days to run simulation
#' @param print Boolean to control whether daily summary is printed
#' @param beta_daily_inf_rate Controls number of s that are infected by i to 
#' become i. Should be between 0 and 1
#' @param num_days_infectuous Controls number of i that transition to r or t
#' @param death_prob Probability transitioning from i to t
#' @param tweet_rate_infected If infected, what rate of tweets per day. Can be 
#' > 1
#' @param n_patient_zero Number of patients infected at day 0
#' @param mean_days_to_death_from_t Mean number of days in t before d
#' @return dataframe simulating sirtd+tweets state for number days 

SIRD_exact <- function(n_pop,
                        n_days,
                        print,
                        beta_daily_inf_rate,
                        num_inf_days,
                        death_prob,
                        tweet_rate,
                        n_patient_zero,
                        round = TRUE) {
  i <- n_patient_zero
  r <- 0
  d <- 0
  s <- n_pop - n_patient_zero 
  col_names = c('day', 's', 'i', 'r', 'd', 'tweets')
  df = data.frame(matrix(nrow = 0, ncol=length(col_names)))
  colnames(df) = col_names
  tweets = 0
  for (day in 1:n_days) {
    df[day,] = rep(NA,length(col_names)) 
    df[day,]$s = ifelse(round, round(s), s)
    df[day,]$i = ifelse(round, round(i), i)
    df[day,]$r = ifelse(round, round(r), r)
    df[day,]$d = ifelse(round, round(d), d)
    if (round){
      df[day,]$s = df[day,]$s + (n_pop - sum(df[day,c('s', 'i', 'r', 'd')]))
    }
    df[day,]$day = day
    df[day,]$tweets = ifelse(round, round(tweets), tweets)
    if (print) {
      cat(
        sprintf(
          "\nDay=%d, susceptible=%.2f, infected=%.2f, recovered=%.2f,
           dead=%.2f, tweets=%.2f, R0=%.2f %.2f",
          df[day,]$day, df[day,]$s, df[day,]$i, df[day,]$r,
          df[day,]$d, df[day,]$tweets,
          beta_daily_inf_rate/(1/num_inf_days),
          s + i + r + d))
    }
    
    s_delta <- -beta_daily_inf_rate * s * (i / n_pop)
    i_delta <- beta_daily_inf_rate * s * (i / n_pop) - ((1 / num_inf_days) * i)
    r_delta <- (1 / num_inf_days) * i - death_prob * r
    d_delta <- death_prob * r
    tweets <- i * tweet_rate
    s <- s + s_delta
    i <- i + i_delta
    r <- r + r_delta
    d <- d + d_delta
  
  }
  return(df)
}


