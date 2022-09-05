data(dommats, package = "EloRating")
res <- repeatability_steepness(dommats$elephants, n_rand = 10)

test_that("repeatability steepness", {
  expect_true(res$steepness <= 1 & res$steepness >= 0)
})
