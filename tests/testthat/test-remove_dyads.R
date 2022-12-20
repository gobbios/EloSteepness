# create some random matrix

n <- sample(10:15, 1)
m <- matrix(rpois(n ^ 2, 1.5), ncol = n)
diag(m) <- 0

rem <- runif(1, 0.4, 0.8)
# round to three digits (issue #5)
rem <- round(rem * 1000) / 1000

r0 <- remove_dyads(m, stop_at = rem)
r1 <- remove_dyads(m, removal_mode = "by_interaction", stop_at = rem)
r2 <- remove_dyads(m, removal_mode = "by_dyad", stop_at = rem)

test_that("removal of dyads stops at specified value", {
  expect_true(length(r0$matrices) == length(r1$matrices))
  expect_true(length(r0$matrices) == length(r2$matrices))
  expect_true(rev(r0$summary$prunk)[1] <= rem)
  # interaction-wise removal should end in more interactions removed
  #   compared to dyad-wise removal
  # but this is not strictly / *always* true, 
  #   so this 'test' is probabilistic rather than deterministic 
  #   and hence not a good *test*
  # expect_true(mean(r1$summary$n_int) < mean(r2$summary$n_int))
})
