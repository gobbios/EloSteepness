
nids <- sample(5:10, 1)
nobs <- round(nids ^ runif(1, 2.5, 3))


mat <- simple_steep_gen(n_ind = nids, n_int = nobs,
                        steep = 1, id_bias = 0, rank_bias = 0)
xseq <- mat$sequence
mat <- mat$matrix

mat2 <- mat
mat2[, 1] <- 0
mat2[1, ] <- 0

plist <- prep_data_for_rstan(mat)

mat3 <- mat
colnames(mat3) <- NULL
rownames(mat3) <- NULL
plist3 <- prep_data_for_rstan(mat3)

mat4 <- mat
diag(mat4)[3:5] <- c(3, 2, 1)

set.seed(123)
plist5a <- prep_data_for_rstan(mat)
mat5 <- mat
diag(mat5) <- NA
set.seed(123)
plist5b <- prep_data_for_rstan(mat)

test_that("conversion to list for rstan works", {
  expect_true(plist$N == nobs)
  expect_true(length(plist$winner) == nobs)
  expect_true(length(plist$loser) == nrow(xseq))
  expect_true(length(plist$ids) == nids)

  expect_message(plist2 <- prep_data_for_rstan(mat2))
  expect_true(plist2$K == nids - 1)

  expect_true(is.null(plist3$ids))

  expect_message(prep_data_for_rstan(mat4))

  expect_identical(plist5a, plist5b)
})
