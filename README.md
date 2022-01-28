
`EloSteepness` is a package that allows estimating steepness of dominance hierarchies from interaction networks.
It does so by estimating Bayesian Elo-ratings, from which the steepness metric can be calculated.
The major difference from classic approaches is that we obtain posterior steepness *distributions*, not point estimates.
More details on the theoretical background can be found in the accompanying preprint/paper.

Below are the instructions to install `EloSteepness`.

## Installation

In order to get the package up an running you need a fairly recent version of R (I'd recommend at least v4.0).
You also need a working installation of [`rstan`](https://mc-stan.org/).
This in turn requires `stan` to be installed but this is taken care of during the setup of the `rstan` package.
The easiest way of doing all this is to [install the `brms` package](https://github.com/paul-buerkner/brms#how-do-i-install-brms). (`brms` is not actually required for `EloSteepness` to work, but it handles the installation of `rstan` and friends very conveniently.)
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

Finally, in order to make the tutorial accessible from within R, you need $\LaTeX$.
The easiest way of getting this done (unless you have it already) is to use:

```
install.packages("tinytex")
tinytex::install_tinytex()
```

You can skip this step if you are happy with downloading the tutorial manually [here](https://github.com/gobbios/EloSteepness/blob/main/vignettes/pdf_files/tutorial.pdf).

Then run this (and set `build_vignettes = FALSE` if you don't want to *install* the tutorial):

```
devtools::install_github("gobbios/EloSteepness", build_vignettes = TRUE, ref = "HEAD")
```

This might take several minutes to complete.
To check whether the installation worked, restart R and try to open the tutorial that comes with the package.

```
vignette("tutorial", package = "EloSteepness")
```


### 2) from local file

Here you download the package as a single file and then install it from there. 
Start by downloading the package file [from here](https://github.com/gobbios/EloSteepness/releases/latest).
Choose the file `EloSteepness_0.3.0.tar.gz` for download (don't unpack it!), and remember the path you saved it to...
Then depending on what OS you use, adapt to your needs and run one of the following code chunks (don't forget to change the path and use the correct file name):

```
# something like this on MacOS or Linux
install.packages("~/Downloads/EloSteepness_0.3.0.tar.gz",  
                 repos = NULL, type = "source")
```

```
# something like this on Windows
install.packages("C:/Users/myname/Downloads/EloSteepness_0.3.0.tar.gz",  
                 repos = NULL, type = "source")
```

Depending on your hardware, this can take up to several minutes to complete.
After this is done, I would again recommend to restart R.

To check whether the installation worked, try to open the tutorial that comes with the package.

```
vignette("tutorial", package = "EloSteepness")
```


If you have trouble with this, you might also try one of these options.

If you are on Windows, download the `EloSteepness_0.3.0.zip` file and run (and don't forget to change the path and use the correct file name):

```
install.packages("C:/Users/myname/Downloads/EloSteepness_0.3.0.zip", 
                 repos = NULL, type = "win.binary")
```

If you are on MacOS, download the `EloSteepness_0.3.0.tgz` file and run (and don't forget to change the path and use the correct file name):

```
install.packages("~/Downloads/EloSteepness_0.3.0.tgz", 
                 repos = NULL, type = "mac.binary")
```

And finally, if you are just interested in the tutorial, [you can find it here](https://github.com/gobbios/EloSteepness/blob/main/vignettes/pdf_files/tutorial.pdf).


## First steps

Please refer to the tutorial to go through some examples.
Either download it [here](https://github.com/gobbios/EloSteepness/blob/main/vignettes/pdf_files/tutorial.pdf) or use `vignette("tutorial", package = "EloSteepness")` to launch it directly from R.

