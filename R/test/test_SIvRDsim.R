library(testthat)

source(here::here('R','SIvRDsim.R'))

test_that("SIRD exact test",{
  simDf = SIvRD(n_pop = 101, 
                print = FALSE, 
                n_days = 10,
                beta_daily_inf_rate = c(.1, .2, .8, .3, .9, .3, .1, .1, .1, .1),
                num_inf_days = 5,
                death_prob = .01,
                tweet_rate = .5,
                n_patient_zero = 1,
                round = FALSE)
  
  expect_equal(nrow(simDf), 10)
  expect_equal(simDf[10,]$tweets, 1.14, tolerance = 1e-2)
  expect_equal(simDf[10,]$d, 0.0965, tolerance = 1e-2)
  })

