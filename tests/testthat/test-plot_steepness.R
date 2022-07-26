data(dommats, package = "EloRating")
w <- sample(1:3, 1)
m <- dommats[[w]]
if (any(rowSums(m) + colSums(m) == 0)) {
  suppressMessages(res <- davids_steepness(m, refresh = 0, chains = 2))
} else {
  res <- davids_steepness(m, refresh = 0, chains = 2)
}

test_that("supplying wrong object results in error", {
  expect_error(plot_steepness(res$stanfit))
})


plot_steepness(res, print_numbers = TRUE)
plot_steepness(res, print_numbers = FALSE)
plot_steepness(res, adjustpar = 0.7)
