/*
Modified from https://mc-stan.org/users/documentation/case-studies/boarding_school_case_study.html
*/

// #include /home/breck/git/codatmo/dataGeneratingProcess1/stan/ode_solvers.stan

functions {
  real[] sird(real t, real[] y, real[] theta, 
             real[] x_r, int[] x_i) {

      real S = y[1];
      real I = y[2];
      real R = y[3];
      real D = y[4];
      real N = x_i[1];

      real beta = theta[1];
      real gamma = theta[2];
      real deathRate = theta[3];
     
      real dS_dt = -beta * I * S / N;
      real dI_dt =  beta * I * S / N - gamma * I ;
      real dR_dt =  gamma * I - deathRate * R;
      real dD_dt =  deathRate * R; 
 
      return {dS_dt, dI_dt, dR_dt, dD_dt};
  }

  real[,] block_sird(int days, real[] y, real[] theta, 
             real[] x_r, int[] x_i, int debug, int I2DandR, int I2D2R) {
    real day_counts[days,4];
    real S = y[1];
    real I = y[2];
    real R = y[3];
    real D = y[4];
    real N = x_i[1];
    real beta = theta[1];
    real gamma = theta[2];
    real deathRate = theta[3];
    if (debug == 1) {
      print("beta, gamma, deathRate=", theta);  
      print("0 SIRD=", y," sum=", sum(y));
    }  
    if (I2DandR + I2D2R != 1) {
      reject("Misconfigured Block solver, I2D2andR xor I2D2R must be 1")
    }
    for (i in 1:days) {
      real dS_dt;
      real dI_dt;
      real dR_dt;
      real dD_dt;
      if (I2DandR == 1) {
        dS_dt = -beta * I * S / N;
        dI_dt =  beta * I * S / N - gamma * I - deathRate * I;
        dR_dt =  gamma * I; // - deathRate * R;
        dD_dt =  deathRate * I; // R;
      }
      if (I2D2R == 1) {
        dS_dt = -beta * I * S / N;
        dI_dt =  beta * I * S / N - gamma * I ;
        dR_dt =  gamma * I - deathRate * R;
        dD_dt =  deathRate * R; 
      }
    if (debug ==1) {  
      print("dS_dt, dI_Dt, dR_dt, dD_dt=", {dS_dt, dI_dt, dR_dt, dD_dt},
            " sum=", sum({dS_dt, dI_dt, dR_dt, dD_dt}));
    }          
      S = dS_dt + S;
      I = dI_dt + I;
      R = dR_dt + R;
      D = dD_dt + D;
      day_counts[i] = {S, I, R, D};
     if (debug == 1) { 
        print(i," SIRD=", day_counts[i], " sum=", sum(day_counts[i]));
     }
    }
    return day_counts;
  }
}
data {
  int<lower=1> n_days;
  real sDay1;
  real iDay1;
  real rDay1;
  real dDay1;
  int NPop;
  vector[n_days] tweets;
  vector[n_days] deaths;
  int<lower = 0, upper = 1> compute_likelihood;
  int<lower = 0, upper = 1> run_twitter;
  int<lower = 0, upper = 1> run_block_ODE;
  int<lower = 0, upper = 1> run_rk45_ODE;
  int<lower = 0, upper = 1> scale;
  int<lower = 0, upper = 1> center;
  real prior_beta_mean;
  real prior_beta_std;
  real prior_gamma_mean;
  real prior_gamma_std;
  real prior_death_prob;
  real prior_death_prob_std;
  real prior_twitter_lambda;
  int days_held_out;
}
transformed data {
  real x_r[0]; //need for ODE function
  int x_i[1] = { NPop }; //need for ODE function
  int debug = 0;
  int debug2 = 0;
  int n_compartments = 4;
  int sCompartment = 1;
  int iCompartment = 2;
  int rCompartment = 3;
  int dCompartment = 4;
  real ts[n_days];
  real meanDeaths = 0;
  real meanTweets = 0;
  real sdDeaths = 1;
  real sdTweets = 1; 
  vector[n_days] deaths_munged = deaths; 
  vector[n_days] tweets_munged = tweets; 
  int n_days_train = n_days - days_held_out;
  if (compute_likelihood == 1){
    if (center == 1) {
      meanDeaths = mean(deaths);
      deaths_munged = deaths_munged - meanDeaths;
      meanTweets = mean(tweets);
      tweets_munged = tweets_munged - meanTweets;
    }
    if (scale == 1) {
      sdDeaths = sd(deaths);
      if (sdDeaths == 0) {
       reject("Standard deviation of zero for deaths");
      }
      deaths_munged = deaths_munged/sdDeaths;
      sdTweets = sd(tweets);
      if (sdTweets == 0) {
        reject("Standard deviation of zero for tweets");
      }
      tweets_munged = tweets_munged/sdTweets;
    }
  }
  ts[1] = 1.0;
  for (i in 2:n_days) {
      ts[i] = ts[i - 1] + 1;
  }
  if (compute_likelihood == 1) {
    print("SD tweets=", sdTweets, "SD deaths=",sdDeaths);
    print("tweets", tweets);
    print("tweets_munged", tweets_munged);
    print("deaths", deaths);
    print("deaths_munged", deaths_munged);
  }
  else {
    print("Not running likelihood");
  }
}

parameters {
  real<lower = 0, upper = 1> gamma;
  real<lower=0, upper = 2> beta;
  real<lower=0, upper = 1> deathRate;
  real<lower=.001> lambda_twitter;
  real<lower=0> normal_tweets_sd;
  real<lower=0> normal_deaths_sd;
}
transformed parameters{
  real compartmentStartValues[4] = {sDay1, iDay1, rDay1, dDay1};  
  real y[n_days, 4];
  matrix[n_days, 4] daily_counts_ODE;
  real theta[3];
  theta[1] = beta;
  theta[2] = gamma;
  theta[3] = deathRate;
  if (run_rk45_ODE == 1 && run_block_ODE == 1) {
    reject("cannot run both rk45 and block ODEs");
  }
  if (run_rk45_ODE == 1 ) {
    y = integrate_ode_rk45(sird, compartmentStartValues , 0.0, ts, theta, x_r, x_i);
  }
  if (run_block_ODE == 1) {
    y = block_sird(n_days, compartmentStartValues, theta, x_r, x_i, debug, 1, 0);
  }
  daily_counts_ODE = to_matrix(y);
}

model {
  beta ~ normal(prior_beta_mean, prior_beta_std);
  gamma ~ normal(prior_gamma_mean, prior_gamma_std);
  deathRate ~ normal(prior_death_prob, prior_death_prob_std);
  lambda_twitter ~ normal(0,1);
  normal_tweets_sd ~ exponential(1);
  normal_deaths_sd ~ exponential(1);
  if (compute_likelihood == 1) { 
    for (i in 1:n_days_train) {
      	deaths_munged[i] ~ normal(daily_counts_ODE[i, dCompartment], 
                                       normal_deaths_sd);
      if (run_twitter == 1) {
        tweets_munged[i] ~ normal(lambda_twitter * 
                                  daily_counts_ODE[i, iCompartment],
                                          normal_tweets_sd);
      }
    }
  }
}

generated quantities {
  real R0 = beta / gamma;
  real recovery_days = 1 / gamma;
  real pred_deaths[n_days];
  real pred_tweets[n_days];
  real sdDeaths_r = sdDeaths;
  real sdTweets_r = sdTweets;
  real lambda_twitter_unscaled = lambda_twitter * sdTweets;
  //  matrix[n_days, n_compartments] ode_states = daily_counts_ODE;
  matrix[n_compartments, n_days] transposed_ode_states = daily_counts_ODE';
  row_vector[n_days] S = transposed_ode_states[sCompartment] * sdDeaths + meanDeaths;
  row_vector[n_days] I = transposed_ode_states[iCompartment] * sdDeaths + meanDeaths;
  row_vector[n_days] R = transposed_ode_states[rCompartment] * sdDeaths + meanDeaths;
  row_vector[n_days] D = transposed_ode_states[dCompartment] * sdDeaths + meanDeaths;
  for (i in 1:n_days) {
      if (run_twitter == 1) {
        pred_tweets[i] = sdTweets * lambda_twitter * 
                         daily_counts_ODE[i, iCompartment] + meanTweets;
      }
      else {
        pred_tweets[i] = 0;
      }
      pred_deaths[i] = sdDeaths * daily_counts_ODE[i, dCompartment] 
                       + meanDeaths;
  }
}
