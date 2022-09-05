data(dommats, package = "EloRating")
res <- upward_steepness(dommats$elephants)

test_that("upward steepness", {
  expect_true(res <= 1 & res >= 0)
})


