library(testthat)

source(here::here('R','SIRDsim.R'))

test_that("SIRD exact test",{
  simDf = SIRD_exact(n_pop = 101, print = FALSE, n_days = 10,
                    beta_daily_inf_rate = .5,
                    num_inf_days = 5,
                    death_prob = .01,
                    tweet_rate = .5,
                    n_patient_zero = 1,
                    round = FALSE)
  
  expect_equal(nrow(simDf), 10)
  expect_equal(simDf[10,]$tweets, 3.59, tolerance = 1e-2)
  expect_equal(simDf[10,]$d, 0.146, tolerance = 1e-2)
  })

