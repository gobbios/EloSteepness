
data("bonobos", package = "EloRating")
data("dommats", package = "EloRating")

mat <- bonobos
res1 <- scores(davids_steepness(mat, refresh = 0))
xord <- sample(seq_len(ncol(mat)))
mat <- mat[xord, xord]
res2 <- scores(davids_steepness(mat, refresh = 0))

res1 <- res1[order(res1$mean), ]
res2 <- res2[order(res2$mean), ]

all(res1$id == res2$id)

test_that("multiplication works", {
  expect_true(all(res1$id == res2$id))
})

m <- dommats$badgers
res1 <- elo_steepness_from_matrix(m, mode = "fixed_sd", n_rand = 1, refresh = 0)
res2 <- elo_steepness_from_matrix(m, mode = "fixed_sd", n_rand = 2, refresh = 0)

res1 <- scores(res1)
res2 <- scores(res2)


test_that("multiplication works", {
  expect_true(all(is.na(res1$mean_cv)))
  expect_true(!all(is.na(res2$mean_cv)))
})

