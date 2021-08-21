library(testthat)

source(here::here('R','SIRDsim.R'))

test_that("SIRD exact test",{
  simDf = SIRD_exact(n_pop = 10000, print = TRUE, n_days = 100,
                    beta_daily_inf_rate = .3,
                    num_inf_days = 7,
                    death_prob = .02,
                    tweet_rate = .5,
                    n_patient_zero = 10,
                    round = TRUE)
  
  expect_equal(nrow(simDf), 100)
  expect_equal(simDf[50,]$tweets, 877)
  expect_equal(simDf[50,]$d, 748)
  expect_equal(simDf[70,]$tweets, 269)
  expect_equal(simDf[70,]$d, 2573)
  })

