library(testthat)
library(cmdstanr)

set_cmdstan_path("/home/breck/.cmdstanr/cmdstan-2.27.0")

test_that("Test baseline scaled params", {
    n_pop = 100
    n_days = 10
    n_patient_zero = 10
    stan_data_2 <- list(n_days = n_days,
                        y0 = c(n_pop - n_patient_zero,
                               n_patient_zero, 0, 0, 0), # one more zero here
                        t0 = 0,
                        ts = 1:n_days,
                        compute_likelihood = 0,
                        use_twitter = 1,
                        death_count = rep(0, n_days),
                        symptomaticTweets = rep(0, n_days),
                        prior_beta_mean = 0,
                        prior_beta_std = 10,
                        prior_omega_mean = 0,
                        prior_omega_std = 10,
                        prior_dI_mean = 0,
                        prior_dI_std = 10,
                        prior_dT_mean = 0,
                        prior_dT_std = 10,
                        prior_twitter_lambda = 1
                        )
    model2 <- cmdstan_model(here::here("stan", "tweet_sirtd_negbin_ODE.stan"))
    fit <- model2$sample(data=stan_data_2,
                         parallel_chains = 1,
                         iter_warmup = 100,
                         iter_sampling = 100,
                         chains = 1,
                         seed = 4857)
    summary = fit$summary(variables=c('pred_tweets'))
    expect_equal(is.nan(summary$mean[1]), FALSE)
})
  
