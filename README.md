
## Installation

In order to get the package up an running you need a working installation of [`rstan`](https://mc-stan.org/).
This in turn requires `stan` to be installed but this is taken care of during the setup of the `rstan` package.
The easiest way of doing all this is to [install the `brms` package](https://github.com/paul-buerkner/brms#how-do-i-install-brms). (`brms` is not actually required for `EloSteepness` to work, but it handles the installation of `rstan` and friends very conveniently.)
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

1) directly from GitHub

For this to work you'll need the `devtools` package (which you can get via `install.packages("devtools")`)

`devtools::install_github("gobbios/EloSteepness")`

2) from local file

Here you download the package as a file and then install it from there. 
Which file to choose from the three in the folder depends on your OS and your level of adventurousness. 
Download the one you need (don't unpack it!), and remember the path you saved it to...

