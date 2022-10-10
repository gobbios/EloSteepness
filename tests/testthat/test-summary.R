data(dommats, package = "EloRating")
res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 1,
                                 iter = 800, warmup = 400,
                                 refresh = 0, chains = 2, seed = 1)
res1 <- capture.output(summary(res))

res <- davids_steepness(dommats$elephants, 
                                 iter = 800, warmup = 400,
                                 refresh = 0, chains = 2, seed = 1)
res2 <- capture.output(summary(res))


test_that("summaries produce textual output", {
  expect_true(length(res1) == 19 & length(res2) == 14)
  expect_true(grepl("ratings", res1[1]))
  expect_true(grepl("scores", res2[1]))
})
