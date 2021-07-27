data(dommats, package = "EloRating")
w <- sample(1:3, 1)
res <- davids_steepness(dommats[[w]], refresh = 0)

test_that("multiplication works", {
  expect_error(plot_scores(res$stanfit))
})

plot_scores(res)
plot_scores(res, color = FALSE)
