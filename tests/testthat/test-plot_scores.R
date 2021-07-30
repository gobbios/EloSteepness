data(dommats, package = "EloRating")
w <- sample(1:3, 1)
res <- davids_steepness(dommats[[w]], refresh = 0)

test_that("multiplication works", {
  expect_error(plot_scores(res$stanfit))
  expect_error(plot_scores(res, color = 1:3))
})

plot_scores(res, color = TRUE)
plot_scores(res, color = FALSE)
cols <- adjustcolor(1:length(res$ids), 0.7)
plot_scores(res, color = cols)
