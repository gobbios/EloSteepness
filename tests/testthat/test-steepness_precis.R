data("dommats", package = "EloRating")
data("baboons1", package = "EloRating")
baboons1 <- baboons1[1:70, ]

# set.seed(1)
res1 <- elo_steepness_from_matrix(dommats$elephants, n_rand = 3, iter = 800, chains = 2,
                                  silent = TRUE, refresh = 0, seed = 1)
summary1 <- steepness_precis(res1)

# set.seed(2)
res2 <- elo_steepness_from_matrix(dommats$elephants, n_rand = 1, iter = 800, chains = 2,
                                  silent = TRUE, refresh = 0, seed = 2)
summary2 <- steepness_precis(res2)

# set.seed(3)
res3 <- elo_steepness_from_sequence(winner = baboons1$Winner, loser = baboons1$Loser, iter = 800, chains = 2,
                                    silent = TRUE, refresh = 0, seed = 3)
summary3 <- steepness_precis(res3)



test_that("precis respects number of randomizations", {
  expect_true(!is.na(summary1["mean_cv"]) & !is.na(summary1["median_cv"]))
  expect_true(is.na(summary2["mean_cv"]) & is.na(summary2["median_cv"]))
  expect_true(is.na(summary3["mean_cv"]) & is.na(summary3["median_cv"]))
  expect_true(round(summary1[1], 10) == round(mean(as.numeric(extract(res1$stanfit, pars = "steepness")$steepness)), 10))
  expect_true(round(summary2[1], 10) == round(mean(as.numeric(extract(res2$stanfit, pars = "steepness")$steepness)), 10))
  expect_true(round(summary3[1], 10) == round(mean(as.numeric(extract(res3$stanfit, pars = "steepness")$steepness)), 10))
})

test_that("inappropriate object throws error", {
  expect_error(steepness_precis(rnorm(10)))
})

