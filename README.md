
<!-- README.md is generated from README.Rmd. Please edit that file -->

# persuasio

<!-- badges: start -->

<!-- badges: end -->

`persuasio` estimates and bounds persuasion effects in instrumental
variable settings with binary outcomes. You provide the outcome, the
treatment, and the instrument, tell `persuasio` which estimand you want
(average or local persuasion rate), and it takes care of the bounds and
inference. Based on Jun and Lee (2023) <https://doi.org/10.1086/724114>.

## Installation

You can install the development version of persuasio from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("xinrui33/persuasio")
```

## Quick Example

``` r
library(persuasio)
## basic example code

# Average persuasion rate (APR): normal inference
persuasio(
  est     = "apr",
  varlist = c("voteddem_all", "readsome", "post"),
  data    = GKB,
  level   = 0.80,
  method  = "normal"
)
#> 
#> Average persuasion rate for binary outcomes, binary treatments and binary instruments
#> 
#> Outcome:    voteddem_all
#> Treatment:  readsome
#> Instrument: post
#> Model:      no_interaction
#> Method:     normal
#> Observations: 701
#> 
#> Estimates:
#>  Lower Bound Upper Bound CI Lower CI Upper
#>       0.0707      0.6343   0.0288   0.6611
#> 
#> Confidence level: 80%

# Local persuasion rate (LPR): bootstrap inference
persuasio(
  est     = "lpr",
  varlist = c("voteddem_all", "readsome", "post"),
  data    = GKB,
  level   = 0.80,
  method  = "bootstrap",
  nboot   = 1000
)
#> 
#> Local persuasion rate for binary outcomes, binary treatments and binary instruments 
#> 
#> Outcome:    voteddem_all
#> Treatment:  readsome
#> Instrument: post
#> Model:      no_interaction
#> Method:     bootstrap
#> Observations: 701
#> 
#> Estimates:
#>     LPR CI Lower CI Upper
#>  0.8067   0.0409   1.8671
#> 
#> Confidence level: 80%
#> Bootstrap replications: 1000
```

## Learn more

See `vignette("persuasio")` for a full walkthrough including covariates,
model specifications, and the relationship between estimands.
