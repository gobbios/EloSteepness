
x <- simple_steep_gen(n_ind = 5, n_int = 50, steep = runif(1, 0, 1))$matrix
o <- capture.output(res1 <- elo_steepness_from_matrix(x,
                                                      mode = "fixed_sd",
                                                      n_rand = 2,
                                                      silent = TRUE))

test_that("steepness is between 0 and 1", {
  expect_true(all(res1$steepness < 1))
  expect_true(all(res1$steepness > 0))
})


o <- capture.output(res2 <- elo_steepness_from_matrix(x,
                                                     mode = "original",
                                                     n_rand = 2,
                                                     silent = TRUE))

test_that("steepness is between 0 and 1", {
  expect_true(all(res2$steepness < 1))
  expect_true(all(res2$steepness > 0))
})


# consisten id names

x <- simple_steep_gen(n_ind = 11, n_int = 300, steep = runif(1, 0.8, 1))$matrix
o <- capture.output(res3 <- elo_steepness_from_matrix(x,
                                                      mode = "fixed_sd",
                                                      n_rand = 1,
                                                      silent = TRUE))


y <- sample(colnames(x))
y <- x[y, y]
o <- capture.output(res4 <- elo_steepness_from_matrix(y,
                                                      mode = "fixed_sd",
                                                      n_rand = 1,
                                                      silent = TRUE))

cwp3 <- apply(res3$cumwinprobs, 3, mean)
names(cwp3) <- res3$ids
cwp3 <- cwp3[order(names(cwp3))]

cwp4 <- apply(res4$cumwinprobs, 3, mean)
names(cwp4) <- res4$ids
cwp4 <- cwp4[order(names(cwp4))]

test_that("handling of id codes works", {
  expect_gt(cor(cwp3, cwp4), 0.9)
})

plot(cwp3, cwp4)
