
data("dommats", package = "EloRating")

mat <- dommats$elephants
res1 <- scores(davids_steepness(mat, refresh = 0, iter = 1000))
xord <- sample(seq_len(ncol(mat)))
mat <- mat[xord, xord]
res2 <- scores(davids_steepness(mat, refresh = 0, iter = 1000))

res1 <- res1[order(res1$mean), ]
res2 <- res2[order(res2$mean), ]

test_that("order in results is independent of order in input matrix", {
  expect_true(all(res1$id == res2$id))
})

m <- dommats$elephants
res1 <- elo_steepness_from_matrix(m, algo = "fixed_sd", n_rand = 1, refresh = 0, iter = 1000)
res2 <- elo_steepness_from_matrix(m, algo = "fixed_sd", n_rand = 2, refresh = 0, iter = 1000)

res1 <- scores(res1)
res2 <- scores(res2)

test_that("CV is calculated if more than one randomized sequence", {
  expect_true(all(is.na(res1$mean_cv)))
  expect_true(!all(is.na(res2$mean_cv)))
})
