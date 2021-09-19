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
  real beta_infection_rate;
  real sigma_deaths_sd;
  real<lower = 0> tweet_rate;
}

model {
  if (scale == 1) {
    alpha_n_patient0 ~ normal(0, 1);
    beta_infection_rate ~ normal(0, 1);
    sigma_deaths_sd ~ exponential(1);
    tweet_rate ~ normal(0,1);
  }
  else {
    alpha_n_patient0 ~ uniform(-10000000, 10000000);
    beta_infection_rate ~ uniform(-10000000, 10000000);
    sigma_deaths_sd ~ uniform(-10000000, 10000000);
    tweet_rate ~ uniform(-10000000, 10000000);
  }
  if (compute_likelihood == 1) { 
    for (i in 1:n_days_train) {
      real normalized_deaths = deaths[i] / deaths_scaling;
      real normalized_tweets = tweets[i] / tweets_scaling;
      if (use_tweets != 1) {
	normalized_tweets = 0;
      }
      normalized_deaths ~ normal(alpha_n_patient0 +
			         beta_infection_rate * i +
				 tweet_rate * normalized_tweets,
			         sigma_deaths_sd);
    }
  }
}

generated quantities {
  real pred_deaths[n_days];
  real pred_tweets[n_days];
  for (i in 1:n_days) {
    real normalized_tweets = tweets[i] / tweets_scaling;
    if (use_tweets != 1) {
      normalized_tweets = 0;
    }
    real pred_deaths_normalized = alpha_n_patient0 +
                                  beta_infection_rate * i +
                                  tweet_rate * normalized_tweets;
    pred_deaths[i] = pred_deaths_normalized * deaths_scaling;
  }
}
