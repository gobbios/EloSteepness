data(dommats, package = "EloRating")
w <- sample(1:3, 1)
m <- dommats[[w]]
if (any(rowSums(m) + colSums(m) == 0)) {
  suppressMessages(res <- davids_steepness(m, refresh = 0, chains = 2))
} else {
  res <- davids_steepness(m, refresh = 0, chains = 2)
}


test_that("plotting the wrong object results in error", {
  expect_error(plot_scores(res$stanfit))
})

test_that("submitting a faulty color vector results in error", {
  expect_error(plot_scores(res, color = 1:3))
})

plot_scores(res, color = TRUE)
plot_scores(res, color = FALSE)
cols <- adjustcolor(seq_along(res$ids), 0.7)
plot_scores(res, color = cols)
