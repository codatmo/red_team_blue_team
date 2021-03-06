data {
  int<lower=1> n_days;
  vector[n_days] tweets;
  vector[n_days] deaths;
  int<lower = 0, upper = 1> compute_likelihood;
  int<lower = 0, upper = 1> use_tweets;
  int<lower = 0, upper = 1> scale;
  int days_held_out;
  int debug;
}

transformed data {
  real deaths_scaling = 1;
  real tweets_scaling = 1; 
  int n_days_train = n_days - days_held_out;
  if (scale == 1) {
    deaths_scaling = sd(deaths);
    if (deaths_scaling == 0) {
      reject("Standard deviation of zero for deaths");
    }
    tweets_scaling = sd(tweets);
    if (tweets_scaling == 0) {
      reject("Standard deviation of zero for tweets");
    }
  }
}

parameters {
  real alpha_n_patient0;
  real beta_death_rate;
  real<lower = 0> sigma_deaths_sd;
  real<lower = 0> tweet_rate;
}

model {
  if (scale == 1) {
    alpha_n_patient0 ~ normal(0, 1);
    beta_death_rate ~ normal(0, 1);
    sigma_deaths_sd ~ exponential(1);
    tweet_rate ~ normal(0,1);
  }
  else { #never do this!! for pedegogical purposes
       alpha_n_patient0 ~ uniform(-10000000, 10000000);
       beta_death_rate ~ uniform(-10000000, 10000000);
       sigma_deaths_sd ~ uniform(0, 10000000);
       tweet_rate ~ uniform(0, 10000000);
  }
  if (compute_likelihood == 1) { 
    for (i in 1:n_days_train) {
      real normalized_deaths = deaths[i] / deaths_scaling;
      real normalized_tweets = tweets[i] / tweets_scaling;
      if (use_tweets != 1) {
	         normalized_tweets = 0;
      }
      real deaths_for_day = alpha_n_patient0 / deaths_scaling + 
                       beta_death_rate * i +
				               tweet_rate * tweets[i] / tweets_scaling;
			real deaths_scaled = deaths[i] * deaths_scaling;
       ~ normal(deaths_for_day, sigma_deaths_sd) ;
    }
  }
}

generated quantities {
  real pred_deaths[n_days];
  for (i in 1:n_days) {
    real normalized_tweets = tweets[i] / tweets_scaling;
    if (use_tweets != 1) {
      normalized_tweets = 0;
    }
    pred_deaths[i] = normal_rng(alpha_n_patient0 +
                                beta_death_rate * i +
                                tweet_rate * normalized_tweets,
                                sigma_deaths_sd) * deaths_scaling;
  }
}
