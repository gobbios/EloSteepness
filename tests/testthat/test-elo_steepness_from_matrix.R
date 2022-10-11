

x <- simple_steep_gen(n_ind = 5, n_int = 50, steep = runif(1, 0, 1))$matrix
res1 <- elo_steepness_from_matrix(x,
                                  algo = "fixed_sd",
                                  n_rand = 2,
                                  refresh = 0,
                                  cores = 2,
                                  iter = 800,
                                  silent = TRUE)

test_that("steepness is between 0 and 1", {
  expect_true(all(res1$steepness < 1))
  expect_true(all(res1$steepness > 0))
})


res2 <- elo_steepness_from_matrix(x,
                                  algo = "original",
                                  n_rand = 2,
                                  refresh = 0,
                                  cores = 2,
                                  iter = 800,
                                  silent = TRUE)

test_that("steepness is between 0 and 1", {
  expect_true(all(res2$steepness < 1))
  expect_true(all(res2$steepness > 0))
})


# consistent id names

x <- simple_steep_gen(n_ind = 11, n_int = 300, steep = runif(1, 0.8, 1))$matrix
res3 <- elo_steepness_from_matrix(x,
                                  algo = "fixed_sd",
                                  n_rand = 1,
                                  refresh = 0,
                                  cores = 2,
                                  silent = TRUE)


y <- sample(colnames(x))
y <- x[y, y]
res4 <- elo_steepness_from_matrix(y,
                                  algo = "fixed_sd",
                                  n_rand = 1,
                                  refresh = 0,
                                  cores = 2,
                                  silent = TRUE)

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


# warnings from rstan::sampling are handled appropriately
# strategy to make sure that the sampler runs into issues is to use a small number of iterations
# this should lead to small sample size issues
# and sometimes (but unpredictably so) to divergent transitions and large Rhat values

data(dommats, package = "EloRating")
test_that("warnings are generated or captured", {
  # generate one randomized sequence
  s <- EloRating:::mat2seq(dommats$elephants)
  # create a seed to be used for sampling
  xseed <- sample(10000000, 1)

  # warnings are stored AND returned (silent = FALSE)
  suppressWarnings(expect_warning(res1 <- elo_steepness_from_sequence(winner = s$winner,
                                                     loser = s$loser,
                                                     algo = "original",
                                                     iter = 400,
                                                     warmup = 200,
                                                     cores = 2,
                                                     seed = xseed,
                                                     refresh = 0,
                                                     open_progress = FALSE,
                                                     silent = FALSE)))
  expect_true(res1$diagnostics$has_issues)
  # res1$diagnostics

  # warnings are only stored in function output (silent = TRUE)
  res2 <- elo_steepness_from_sequence(winner = s$winner,
                                      loser = s$loser,
                                      algo = "original",
                                      iter = 400,
                                      warmup = 200,
                                      cores = 2,
                                      seed = xseed,
                                      refresh = 0,
                                      open_progress = FALSE,
                                      silent = TRUE)
  # res2$diagnostics
  expect_identical(res1$diagnostics, res2$diagnostics)
})


# test implementation that uses fixed k
s <- EloRating:::mat2seq(dommats$elephants)
res1 <- elo_steepness_from_sequence(winner = s$winner,
                                    loser = s$loser,
                                    algo = "fixed_k",
                                    iter = 400,
                                    warmup = 200,
                                    cores = 2,
                                    refresh = 0,
                                    open_progress = FALSE,
                                    k = 1.2,
                                    silent = TRUE)

res2 <- elo_steepness_from_matrix(mat = dommats$elephants,
                                  algo = "fixed_k",
                                  iter = 400,
                                  warmup = 200,
                                  chains = 2,
                                  cores = 2,
                                  refresh = 0,
                                  n_rand = 1,
                                  open_progress = FALSE,
                                  k = 1.2,
                                  silent = TRUE)

res3 <- elo_steepness_from_matrix(mat = dommats$elephants,
                                  algo = "fixed_k",
                                  iter = 400,
                                  warmup = 200,
                                  seed = 1,
                                  chains = 2,
                                  cores = 2,
                                  refresh = 0,
                                  n_rand = 4,
                                  open_progress = FALSE,
                                  k = NULL,
                                  silent = TRUE)


test_that("k is not a parameter in the model", {
  expect_false("k" %in% res1$stanfit@model_pars)
  expect_false("k" %in% res2$stanfit@model_pars)
  expect_false("k" %in% res3$stanfit@model_pars)
})
