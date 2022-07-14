
# need to use the same seed for both calls, so that interaction probabilities for dyads are the same
seed <- sample(100000, 1)
set.seed(seed)
r1 <- simple_steep_gen(n_ind = 20, n_int = 2000, steep = 0.9, id_bias = 1,
                       rank_bias = 0.3, sequential = TRUE)
set.seed(seed)
r2 <- simple_steep_gen(n_ind = 20, n_int = 2000, steep = 0.9, id_bias = 1,
                       rank_bias = 0.3, sequential = FALSE)


cor1 <- cor(rowSums(r1$matrix) + colSums(r1$matrix), 
            rowSums(r2$matrix) + colSums(r2$matrix), 
            method = "s")
cor2 <- cor(EloRating::DS(r1$matrix, prop = "Pij")$DS, 
            EloRating::DS(r2$matrix, prop = "Pij")$DS)

test_that("two modes of data generation are equivalent", {
  expect_equal(r1$settings$dyads[, "final"], r2$settings$dyads[, "final"])
  expect_true(cor1 > 0.8)
  expect_true(cor2 > 0.9)
})
