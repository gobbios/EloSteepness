params <-
list(EVAL = TRUE)

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE, # deal with CRAN
  comment = "#>"
)

library(xtable)
compile_start <- Sys.time()
# store original pars to satisfy CRAN (needs to be restored at the end)
ori_pars <- par(no.readonly = TRUE)

## ---- echo=FALSE--------------------------------------------------------------
suppressPackageStartupMessages(library(EloSteepness))

## ----setup, eval = FALSE------------------------------------------------------
#  library(EloSteepness)

## -----------------------------------------------------------------------------
data("dommats", package = "EloRating")
mat <- dommats$badgers

## ----badgermatrix, fig.retina= 2, echo=FALSE, fig.width=6, fig.height=2.8, out.width="50%", fig.align="center", fig.cap="An example network of seven badgers."----
par(family = "serif", mar = c(0.1, 1.6, 1.1, 0.1))
EloSteepness:::plot_matrix(mat, greyout = 0)

## ---- echo=FALSE--------------------------------------------------------------
set.seed(12345)

## ----elo_steep_01, cache=TRUE-------------------------------------------------
elo_res <- elo_steepness_from_matrix(mat = mat, n_rand = 2, refresh = 0, 
                                     cores = 2, iter = 1000, seed = 1)

## -----------------------------------------------------------------------------
summary(elo_res)

## ----elo01_plot1, fig.retina= 2, echo=2:2, fig.width=7, fig.height=4.7, out.width="70%", fig.align='center'----
par(family = "serif")
plot_steepness(elo_res)

## -----------------------------------------------------------------------------
p <- steepness_precis(elo_res, quantiles = c(0.055, 0.25, 0.75, 0.945))
round(p, 2)

## ---- eval = FALSE------------------------------------------------------------
#  elo_res$steepness

## ---- eval=FALSE--------------------------------------------------------------
#  hist(elo_res$steepness, xlim = c(0, 1))

## ---- eval = FALSE------------------------------------------------------------
#  elo_res$stanfit

## ----elo01_plot2, fig.retina= 2, echo=2:3, fig.width=7, fig.height=4.7, out.width="70%", fig.align='center'----
par(family = "serif")
my_colors <- hcl.colors(n = 7, palette = "zissou1", alpha = 0.7)
plot_scores(elo_res, color = my_colors)

## -----------------------------------------------------------------------------
my_scores <- scores(elo_res, quantiles = c(0.055, 0.945))

## ---- eval=FALSE--------------------------------------------------------------
#  my_scores

## ---- results='asis', echo=FALSE----------------------------------------------
print(xtable(my_scores), comment = FALSE, include.rownames = FALSE)

## ----elo01_plot3, fig.retina= 2, echo=2:2, fig.width=7, fig.height=4.7, out.width="70%", fig.align='center'----
par(family = "serif")
plot_scores(elo_res, color = my_colors, subset_ids = "d")

## ----elo01_plot4, fig.retina= 2, echo=2:2, fig.width=7, fig.height=4.7, out.width="70%", fig.align='center'----
par(family = "serif")
plot_scores(elo_res, color = my_colors, subset_ids = "a")

## ----elo01_plot5, fig.retina= 2, echo=2:2, fig.width=7, fig.height=4.7, out.width="70%", fig.align='center'----
par(family = "serif")
plot_scores(elo_res, color = my_colors, subset_ids = c("b", "c"))

## ----elo01_plot6, fig.retina= 2, echo=2:2, fig.width=7, fig.height=4.7, out.width="70%", fig.align='center'----
par(family = "serif")
plot_steepness_regression(elo_res, color = my_colors, width_fac = 0.5)

## -----------------------------------------------------------------------------
data("dommats", package = "EloRating")
mat <- dommats$badgers

set.seed(123)
mat1 <- mat / 2
und <- mat1 - floor(mat1) != 0
mat1[und] <- round(mat1[und] + runif(sum(und), -0.1, 0.1))
mat1["f", "d"] <- 1 # just make sure that 'd' keeps its one loss to 'f'

## -----------------------------------------------------------------------------
mat2 <- mat * 2

## ----elo_steep_02, cache=TRUE-------------------------------------------------
elo_res_half <- elo_steepness_from_matrix(mat = mat1, n_rand = 2, refresh = 0, 
                                          cores = 2, iter = 1000, seed = 2)
elo_res_doubled <- elo_steepness_from_matrix(mat = mat2, n_rand = 2, refresh = 0, 
                                             cores = 2, iter = 1000, seed = 3)

## ----elo02_plot1, fig.retina= 2, echo=2:10, fig.width=9, fig.height=6.5, out.width="90%", fig.align='center'----
par(mfrow = c(2, 3), family = "serif")
plot_steepness(elo_res_half)
plot_steepness(elo_res)
plot_steepness(elo_res_doubled)

plot_scores(elo_res_half, color = my_colors)
plot_scores(elo_res, color = my_colors)
plot_scores(elo_res_doubled, color = my_colors)

## ----simu_elo_ex_1, cache=TRUE------------------------------------------------
set.seed(123)

# generate matrices
m1 <- simple_steep_gen(n_ind = 6, n_int = 40, steep = 0.9)$matrix
m2 <- m1 * 2
m5 <- m1 * 5

# calculate steepness
r1 <- elo_steepness_from_matrix(mat = m1, n_rand = 2, cores = 2, iter = 1000, 
                                seed = 1, refresh = 0)
r2 <- elo_steepness_from_matrix(mat = m2, n_rand = 2, cores = 2, iter = 1000,
                                seed = 2, refresh = 0)
r5 <- elo_steepness_from_matrix(mat = m5, n_rand = 2, cores = 2, iter = 1000, 
                                seed = 3, refresh = 0)

## ----simu_elo_ex_plot1, fig.retina= 2, echo=2:15, fig.width=9, fig.height=5.8, out.width="90%", fig.align='center'----
par(mfrow = c(2, 3), family = "serif", mar = c(3.5, 2.5, 1, 1))
mycols <- hcl.colors(6, palette = "Dark 2", alpha = 0.7)

plot_steepness(r1)
plot_steepness(r2)
plot_steepness(r5)

plot_scores(r1, color = mycols)
plot_scores(r2, color = mycols)
plot_scores(r5, color = mycols)

## -----------------------------------------------------------------------------
data("baboons1", package = "EloRating")

## ---- echo=FALSE--------------------------------------------------------------
s <- baboons1[1:200, ]

## ----babsteep, echo = 2:10, cache=TRUE----------------------------------------
# babseq <- elo_steepness_from_sequence(winner = s$Winner, loser = s$Loser, refresh = 100, cores = 4)
s <- baboons1[1:200, ]
babseq <- elo_steepness_from_sequence(winner = s$Winner,
                                      loser = s$Loser,
                                      refresh = 0,
                                      cores = 2,
                                      seed = 1,
                                      iter = 1000)

## ----elo_bab_plot1, fig.retina= 2, echo=2:10, fig.width=10, fig.height=3, out.width="100%", fig.align='center'----
par(family = "serif", mfrow = c(1, 3))
plot_steepness(babseq)
plot_scores(babseq)
plot_steepness_regression(babseq, width_fac = 0.2)

## ----elo_bab_plot2, fig.retina= 2, echo=2:100, fig.width=9, fig.height=5, out.width="60%", fig.align='center'----
par(family = "serif")

# extract number of interactions
ints <- table(c(s$Winner, s$Loser))
ints <- ints[order(names(ints))]
# get the scores for all individuals
the_scores <- scores(babseq)
the_scores <- the_scores[order(the_scores$id), ]

plot(as.numeric(ints), the_scores$q955 - the_scores$q045,
     xlab = "interactions", ylab = "width of credible interval", las = 1)

## -----------------------------------------------------------------------------
data("dommats", package = "EloRating")
mat <- dommats$badgers

## ----david_steep_01, cache=TRUE-----------------------------------------------
david_res <- davids_steepness(mat, refresh = 0, seed = 1, iter = 1000, cores = 2)

## ----david_steep_01_plot1, fig.retina= 2, fig.height=4.5, fig.width=7, out.width="70%", fig.align='center', echo=2:2----
par(family = "serif", mar = c(3.5, 2.5, 1, 1))
plot_steepness(david_res)

## -----------------------------------------------------------------------------
summary(david_res)

## -----------------------------------------------------------------------------
round(steepness_precis(david_res), 2)

## ----david_steep_01_plot2, fig.retina= 2, fig.height=4.5, fig.width=7, out.width="70%", fig.align='center', echo=2:2----
par(family = "serif", mar = c(3.5, 2.5, 1, 1))
plot_scores(david_res)

## ---- eval=FALSE--------------------------------------------------------------
#  scores(david_res)

## ---- results='asis', echo=FALSE----------------------------------------------
# knitr::kable(my_scores, digits = 3)
my_scores <- scores(david_res)
print(xtable(my_scores), comment = FALSE, include.rownames = FALSE)

## ----david_steep_01_plot3, fig.retina= 2, fig.height=4.5, fig.width=7, out.width="70%", fig.align='center', echo=2:2----
par(family = "serif", mgp = c(2, 0.8, 0), mar = c(3.5, 3.5, 1, 1), tcl = -0.4)
plot_steepness_regression(david_res, width_fac = 1)

## ---- echo=FALSE, eval = TRUE-------------------------------------------------
# this is forced to be evaluated!
stime <- "unknown"
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  stime <- round(as.numeric(difftime(time2 = compile_start, time1 = Sys.time(), units = "mins")), 1)
}

## ---- eval = TRUE, echo=FALSE, include=FALSE----------------------------------
# this is forced to be evaluated!
# reset original par
par(ori_pars)

