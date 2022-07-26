nids <- sample(5:8, 1)
nobs <- round(nids ^ runif(1, 2.2, 2.3))

mat <- simple_steep_gen(n_ind = nids,
                        n_int = nobs,
                        steep = 1,
                        id_bias = 0,
                        rank_bias = 0)
mat <- mat$matrix

res0 <- davids_steepness(mat, refresh = 0, chains = 2, silent = TRUE)


test_that("david_steepness produces sensible results", {
  x <- res0$norm_ds
  expect_true(min(x) >= 0)
  expect_true(max(x) <= (nids - 1))

  x <- res0$steepness
  expect_true(min(x) >= 0)
  expect_true(max(x) <= 1)
})

m1 <- mat
rem <- sample(nids, 1)
m1[rem, ] <- 0
m1[, rem] <- 0
m2 <- mat[-rem, -rem]
m1
m2


test_that("david_steepness handles non-interacting individuals", {
  set.seed(123)
  expect_message(res1 <- davids_steepness(m1, refresh = 0, chains = 2))
  set.seed(123)
  res2 <- davids_steepness(m2, refresh = 0, chains = 2)

  x1 <- colMeans(res1$norm_ds)
  x2 <- colMeans(res2$norm_ds)
  expect_true(all((x1 - x2) < 0.001))
})


# order of individuals
new_order <- sample(colnames(mat))
mat3 <- mat[new_order, new_order]
res3 <- davids_steepness(mat3, refresh = 0, chains = 2)

x <- colMeans(res0$norm_ds)
names(x) <- res0$ids
x <- x[order(names(x))]

y <- colMeans(res3$norm_ds)
names(y) <- res3$ids
y <- y[order(names(y))]

test_that("david_steepness ignores order of individuals in matrix", {
  expect_true(cor(x, y) > 0.9)
})
