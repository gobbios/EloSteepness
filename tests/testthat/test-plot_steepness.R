data(dommats, package = "EloRating")
w <- sample(1:3, 1)
res <- davids_steepness(dommats[[w]], refresh = 0)

test_that("multiplication works", {
  expect_error(plot_steepness(res$stanfit))
})


plot_steepness(res, print_numbers = TRUE)
plot_steepness(res, print_numbers = FALSE)
plot_steepness(res, adjustpar = 0.7)
