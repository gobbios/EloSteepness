
## Installation

In order to get the package up an running you need a working installation of `rstan`.
This in turn requires `stan` to be installed but this is taken care of during the setup of the `rstan` package.
The easiest way of doing all this is to install the `brms` package. (`brms` is not actually required for `EloSteepness` to work, but it handles the installation of `rstan` and friends very conveniently.)
If you already have `brms` (or `rstan`) then you are probably good to go.
If not, then execute the following command and if asked for whether you want to install packages *from source* select 'no' (unless you know what you are doing of course).

`install.packages("brms")`

