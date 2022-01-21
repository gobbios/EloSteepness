data(dommats, package = "EloRating")
w <- sample(1:3, 1)
res <- davids_steepness(dommats[[w]], refresh = 0)

test_that("placeholder", {
  expect_error(plot_steepness_regression(res$stanfit))
})

plot_steepness_regression(res)
plot_steepness_regression(res, color = FALSE, width_fac = 2, axis_extend = 0.2)
cols <- adjustcolor(seq_along(res$ids), 0.7)
plot_steepness_regression(res, color = cols)
