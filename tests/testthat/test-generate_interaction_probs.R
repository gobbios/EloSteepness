# test biases:
ntests <- 20
res <- data.frame(run = 1:ntests,
                  idbias = runif(ntests),
                  rankbias = runif(ntests),
                  cv_id = NA,
                  cor_w_rank = NA,
                  x_colsums = FALSE)
for (i in seq_len(nrow(res))) {
  x <- generate_interaction_probs(n_ind = sample(10:20, 1),
                                  id_bias = res$idbias[i],
                                  rank_bias = res$rankbias[i])
  # generate 10000 'interactions'
  s <- sample(seq_len(nrow(x)), 10000, TRUE, prob = x[, "final"])

  xtab <- as.numeric(table(as.numeric(x[s, 1:2])))
  res$cv_id[i] <- sd(xtab) / mean(xtab)

  xtab <- table((x[s, 2] - x[s, 1]))
  rds <- as.numeric(names(xtab))


  rankdiff <- x[, 2] - x[, 1]
  interactprob <- x[, "final"]
  res$cor_w_rank[i] <- cor(rankdiff, interactprob)
  
  if (all(round(colSums(x[, 3:6]), 10) == 1)) {
    res$x_colsums[i] <- TRUE
  }

}

test_that("column-wise probabilities sum to 1", {
  expect_true(all(res$x_colsums))
})

# the two remaining tests are probabilistic, ie. they work in the majority
# of cases, but not strictly always
# since these rare 'failures' can (if they occur) lead to errors during install
# (and hence during R CMD check), they are turned off


# plot(res$idbias, res$cv_id)
# cor(res$idbias, res$cv_id)
# 
# plot(res$rankbias, res$cor_w_rank)
# cor(res$rankbias, res$cor_w_rank)

# test_that("multiplication works", {
#   expect_true(cor(res$idbias, res$cv_id) > 0.3)
#   expect_true(cor(res$rankbias, res$cor_w_rank) < -0.3)
# })
