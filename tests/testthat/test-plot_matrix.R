
n_nint <- 50
x <- simple_steep_gen(n_ind = 5, n_int = n_nint, steep = runif(1, 0, 1))$matrix

z <- plot_matrix(x, prunkcol = "red", greyout = 0)
test_that("plot_matrix behaves reasonably", {
  expect_true(is.list(z))
  expect_true(length(unique(unlist(lapply(z, dim)))) == 1)
  expect_true(sum(apply(z$content, 1, as.numeric), na.rm = TRUE) == n_nint)
})
