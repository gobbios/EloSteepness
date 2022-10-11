<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/EloSteepness)](https://CRAN.R-project.org/package=EloSteepness)
[![R-CMD-check](https://github.com/gobbios/EloSteepness/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gobbios/EloSteepness/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/gobbios/EloSteepness/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gobbios/EloSteepness?branch=main)

<!-- badges: end -->

`EloSteepness` is a package that allows estimating steepness of dominance hierarchies from interaction networks.
It does so by estimating Bayesian Elo-ratings, from which the steepness metric can be calculated.
The major difference from classic approaches is that we obtain posterior steepness *distributions*, not point estimates.
More details on the theoretical background can be found in the accompanying [preprint/paper](https://doi.org/10.1101/2022.01.28.478016).

Below are the instructions to install `EloSteepness`.

## Installation

In order to get the package up and running you need a fairly recent version of R (I'd recommend at least v4.0).
You also need a working installation of [`rstan`](https://mc-stan.org/).
Since the package is on CRAN now, installation should be easy with a simple call to:

`install.packages("EloSteepness")`

This also should take care of installing all necessary dependencies, including `Stan`.
If you run the following code and it results in a figure, you are good to go.

```
library("EloSteepness")
data(dommats, package = "EloRating")
# using small numbers for iterations etc to speed up running time
set.seed(123)
res <- elo_steepness_from_matrix(dommats$elephants, n_rand = 3, cores = 2,
                                 iter = 1000, warmup = 500, 
                                 refresh = 0, chains = 2, seed = 1)
plot_steepness(res)
summary(res)
```

If this failed, you can try the following:
The easiest way of installing `Stan` is to [install the `brms` package](https://github.com/paul-buerkner/brms#how-do-i-install-brms). (`brms` is not actually required for `EloSteepness` to work, but it handles the installation of `rstan` and friends very conveniently.)
If you don't want to deal with `brms`, you can also try to install `rstan` by itself ([see here for instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)).
If you already have `brms` (or `rstan`) then you are probably good to go.
If not, then execute the following command and if asked for whether you want to install packages *from source* select 'no' (unless you know what you are doing of course).

`install.packages("brms")`

The only other thing you need are two more packages, `EloRating` and `aniDom`, which are easy to install:

`install.packages("EloRating")`

`install.packages("aniDom")`

For good measure, it might be good idea to restart R (or RStudio if you work with it) at this point. 
If you want to be on the safe side, you'd even restart your computer at this point, but this shouldn't be necessary in most cases.

With this done, you can install `EloSteepness`. 
There are two ways of doing that.
Depending on the level of experience with R, I would recommend option 1 only to more advanced users and option 2 to novices.

### 1) directly from GitHub

For this to work you'll need the `devtools` package (which you can get via `install.packages("devtools")`).
Also required is a working package building tool chain.
To check whether this is available run:

```
devtools::has_devel()
```

If that gives positive feedback then you are set.
If not, you need to install more stuff (on MacOS you need the `Xcode` command line tools (e.g. [here](https://mac.install.guide/commandlinetools/4.html)), and on Windows the `Rtools` [here](https://cran.r-project.org/bin/windows/Rtools/rtools40.html)).

I've also seen cases where problems arose because the version of `devtools` was outdated, so even if you have it installed already it might be a good idea to update the package (also with `install.packages("devtools")`).

Next, you need to decide whether or not you want to build the vignettes alongside installing the package.

#### 1a) directly from GitHub - with vignettes

In order to make the tutorial accessible from within R, you need two more things: $\LaTeX$ and `pandoc`.
The easiest way of getting the former done (unless you have it already) is to use:

```
install.packages("tinytex")
tinytex::install_tinytex()
tinytex:::install_yihui_pkgs()
```

Next, install [`pandoc`](https://pandoc.org/installing.html).
If you are using `RStudio` you can skip this latter step (`pandoc` comes with RStudio), and proceed directly to:

```
devtools::install_github("gobbios/EloSteepness", build_vignettes = TRUE, dependencies = TRUE)
```

This will take several minutes. 

To check whether the installation worked, restart R/RStudio and try to open the tutorial that comes with the package.

```
vignette("tutorial", package = "EloSteepness")
```




#### 1b) directly from GitHub - without vignettes

If you are happy with downloading the tutorial manually [here](https://github.com/gobbios/EloSteepness/blob/main/documents/tutorial.pdf), things should be a little simpler:

```
devtools::install_github("gobbios/EloSteepness", build_vignettes = FALSE)
```

This still might take several minutes to complete.


### 2) from local file

Here you download the package as a single file and then install it from there. 
Start by downloading the package file [from here](https://github.com/gobbios/EloSteepness/releases/latest).


If you are on Windows, download the `EloSteepness_0.4.6.zip` file and run (and don't forget to change the path and use the correct file name):

```
install.packages("C:/Users/myname/Downloads/EloSteepness_0.4.6.zip", 
                 dependencies = TRUE, 
                 repos = NULL, type = "win.binary")
```

If you are on MacOS, download the `EloSteepness_0.4.6.tgz` file and run (and don't forget to change the path and use the correct file name):

```
install.packages("~/Downloads/EloSteepness_0.4.6.tgz", 
                 dependencies = TRUE, 
                 repos = NULL, type = "mac.binary")
```

Depending on your hardware, this can take up to several minutes to complete.
After this is done, I would again recommend to restart R.

To check whether the installation worked, try to open the tutorial.

```
vignette("tutorial", package = "EloSteepness")
```


If this didn't work or you feel a bit more adventurous, choose the file `EloSteepness_0.4.6.tar.gz` for download (don't unpack it!), and remember the path you saved it to...
Then depending on what OS you use, adapt to your needs and run one of the following code chunks (don't forget to change the path and use the correct file name):

```
# something like this on MacOS or Linux
install.packages("~/Downloads/EloSteepness_0.4.6.tar.gz",  
                 repos = NULL, type = "source")
```

```
# something like this on Windows
install.packages("C:/Users/myname/Downloads/EloSteepness_0.4.6.tar.gz",  
                 repos = NULL, type = "source")
```




And finally, if you are just interested in the tutorial, [you can find it here](https://github.com/gobbios/EloSteepness/blob/main/documents/tutorial.pdf).


## First steps

Please refer to the tutorial to go through some examples.
Either [download it here](https://github.com/gobbios/EloSteepness/blob/main/documents/tutorial.pdf) or use `vignette("tutorial", package = "EloSteepness")` to launch it directly from R.

