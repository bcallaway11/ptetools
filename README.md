
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Panel Treatment Effects Tools (ptetools) Package <img src="man/figures/logo.png" align="right" alt="ptetools" width="155" />

The `ptetools` package compartmentalizes the steps needed to implement
estimators of group-time average treatment effects (and their
aggregations) in order to make it easier to apply the same sorts of
arguments outside of their “birthplace” in the literature on
difference-in-differences.

This code is lightweight, only works for balanced panels, and has
minimal error checking. That said, it should be useful projects that
build on top of group-time average treatment effects in order to deliver
estimates of causal effects in panel data settings.

The main function is called `pte`. The most important paramters that it
takes in are `subset_fun` and `attgt_fun`. These are functions that the
user should pass to `pte`.

`subset_fun` takes in the overall data, a group, a time period, and
possibly other arguments and returns a `data.frame` containing the
relevant subset of the data, an outcome, and whether or not a unit
should be considered to be in the treated or comparison group for that
group/time. There is one example of a relevant subset function provided
in the package: [the `two_by_two_subset`
function](https://github.com/bcallaway11/ptetools/blob/master/R/subset_functions.R).
This function takes an original dataset, subsets it into pre- and
post-treatment periods and denotes treated and untreated units. This
particular subset is perhaps the most common/important one for thinking
about treatment effects with panel data.

The other main function is `attgt_fun`. This function should be able to
take in the correct subset of data, possibly along with other arguments
to the function, and report an *ATT* for that subset. With minor
modification, this function should be availble for most any sort of
treatment effects application — for example, if you can solve the
baseline 2x2 case in difference in differences, you should use that
function here, and the `ptetools` package will take care of dealing with
the variation in treatment timing.

If `attgt_fun` returns an influence function, then the `ptetools`
package will also conduct inference using the multiplier bootstrap
(which is fast) and produce uniform confidence bands (which adjust for
multiple testing).

The default output of `pte` is an overall treatment effect on the
treated (i.e., across all groups that participate in the treatment in
any time period) and dynamic effects (i.e., event studies). More
aggregations are possible, but these seem to be the leading cases;
aggregations of group-time average treatment effects are discussed at
length in [Callaway and Sant’Anna
(2021)](https://doi.org/10.1016/j.jeconom.2020.12.001).

Here are a few examples:

## Example 1: Difference in differences

The [`did` package](https://bcallaway11.github.io/did/), which is based
on [Callaway and Sant’Anna
(2021)](https://doi.org/10.1016/j.jeconom.2020.12.001), includes
estimates of group-time average treatment effects, *ATT(g,t)*, based on
a difference in differences identification strategy. The following
example demonstrates that it is easy to compute group-time average
treatment effects using difference in differences using the `ptetools`
package. \[*Note:* This is definitely not the recommended way of doing
this as there is very little error handling, etc. here, but it is rather
a proof of concept. You should use the `did` package for this case.\]

This example reproduces DID estimates of the effect of the minimum wage
on employment using data from the `did` package.

``` r
library(did)
data(mpdta)
did_res <- pte(
  yname = "lemp",
  gname = "first.treat",
  tname = "year",
  idname = "countyreal",
  data = mpdta,
  setup_pte_fun = setup_pte,
  subset_fun = two_by_two_subset,
  attgt_fun = did_attgt,
  xformla = ~lpop
)

summary(did_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0146    -0.0609     -0.0036 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0141 -0.0106      0.0645  
#>          -2  -0.0050     0.0148 -0.0444      0.0345  
#>          -1  -0.0229     0.0137 -0.0595      0.0138  
#>           0  -0.0201     0.0115 -0.0508      0.0105  
#>           1  -0.0547     0.0187 -0.1046     -0.0048 *
#>           2  -0.1382     0.0389 -0.2421     -0.0343 *
#>           3  -0.1069     0.0415 -0.2177      0.0039  
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

What’s most interesting here, is that the only “new” code that needs to
be writte is in [the `did_attgt`
function](https://github.com/bcallaway11/ptetools/blob/master/R/attgt_functions.R).
You will see that this is a very small amount of code.

## Example 2: Policy Evaluation during a Pandemic

As a next example, consider trying to estimate effects of Covid-19
related policies during a pandemic. The estimates below are for the
effects of state-leve shelter-in-place orders during the early part of
the pandemic.

[Callaway and Li (2021)](https://arxiv.org/abs/2105.06927) argue that a
particular unconfoundedness-type strategy is more appropriate in this
context than DID-type strategies due to the spread of Covid-19 cases
being highly nonlinear. However, they still deal with the challenge of
variation in treatment timing. Therefore, it is still useful to think
about group-time average treatment effects, but the DID strategy should
be replaced with their particular unconfoundedness type assumption.

The `ptetools` package is particularly useful here.

``` r
# formula for covariates
xformla <- ~ current + I(current^2) + region + totalTestResults
```

``` r
covid_res <- pte(
  yname = "positive",
  gname = "group",
  tname = "time.period",
  idname = "state_id",
  data = covid_data2,
  setup_pte_fun = setup_pte_basic,
  subset_fun = two_by_two_subset,
  attgt_fun = covid_attgt,
  xformla = xformla,
  max_e = 21,
  min_e = -10
)

summary(covid_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.] 
#>  14.8882       72.9644  -128.1195    157.8958 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     3.4720  -13.7679      6.3146 
#>          -9   2.6607     1.3600   -1.2726      6.5940 
#>          -8   0.8290     2.4227   -6.1776      7.8355 
#>          -7   5.2843     2.3046   -1.3807     11.9494 
#>          -6   2.8555     1.6024   -1.7788      7.4899 
#>          -5   1.3589     3.9788  -10.1479     12.8658 
#>          -4   0.3294     3.4291   -9.5876     10.2464 
#>          -3  -4.2227     4.6652  -17.7146      9.2693 
#>          -2  -3.8447     3.0287  -12.6037      4.9143 
#>          -1  -0.2234     3.6485  -10.7751     10.3283 
#>           0 -10.8156     9.1745  -37.3486     15.7174 
#>           1 -13.7998    13.8403  -53.8266     26.2270 
#>           2  -7.8432    11.4608  -40.9882     25.3019 
#>           3  -4.5541     7.8721  -27.3206     18.2123 
#>           4  -3.5368    10.4098  -33.6426     26.5689 
#>           5   8.5221    11.6292  -25.1102     42.1544 
#>           6   1.1140    16.7373  -47.2910     49.5190 
#>           7   6.6384    18.6669  -47.3471     60.6240 
#>           8   7.1288    24.3691  -63.3478     77.6054 
#>           9  10.8758    22.1689  -53.2377     74.9893 
#>          10  17.5057    31.9608  -74.9265    109.9379 
#>          11  40.8318    44.0803  -86.6506    168.3141 
#>          12  48.6134    45.2290  -82.1910    179.4178 
#>          13  52.4228    66.3735 -139.5323    244.3779 
#>          14  50.2000    52.6136 -101.9610    202.3610 
#>          15  68.2960    65.5323 -121.2266    257.8186 
#>          16  44.7305    76.4396 -176.3363    265.7973 
#>          17  61.4670    83.4622 -179.9097    302.8436 
#>          18  50.4635   116.1918 -285.5687    386.4957 
#>          19  47.3392   108.3272 -265.9482    360.6266 
#>          20  28.6326   148.0358 -399.4937    456.7589 
#>          21   4.3445   142.9397 -409.0438    417.7328 
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(covid_res) + ylim(c(-1000, 1000))
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

What’s most interesting is just how little code needs to be written
here. The only new code required is the `ppe::covid_attgt` function
which is [available
here](https://github.com/bcallaway11/ppe/blob/master/R/covid_attgt.R),
and, as you can see, this is very simple.

## Example 3: Empirical Bootstrap

The code above used the multiplier bootstrap. The great thing about the
multiplier bootstrap is that it’s fast. But in order to use it, you have
to work out the influence function for the estimator of *ATT(g,t)*.
Although I pretty much always end up doing this, it can be tedious, and
it can be nice to get a working version of the code for a project going
before working out the details on the influence function.

The `ptetools` package can be used with the empirical bootstrap. There
are a few limitations. First, it’s going to be substantially slower.
Second, this code just reports pointwise confidence intervals. However,
this basically is set up to fit into my typical workflow, and I see this
as a way to get preliminary results.

Let’s demonstrate it. To do this, consider the same setup as in Example
1, but where no influence function is returned. Let’s write the code for
this:

``` r
# did with no influence function
did_attgt_noif <- function(gt_data, xformla, ...) {
  # call original function
  did_gt <- did_attgt(gt_data, xformla, ...)

  # remove influence function
  did_gt$inf_func <- NULL

  did_gt
}
```

Now, we can show the same sorts of results as above

``` r
did_res_noif <- pte(
  yname = "lemp",
  gname = "first.treat",
  tname = "year",
  idname = "countyreal",
  data = mpdta,
  setup_pte_fun = setup_pte,
  subset_fun = two_by_two_subset,
  attgt_fun = did_attgt_noif, # this is only diff.
  xformla = ~lpop
)

summary(did_res_noif)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0112    -0.0542     -0.0103 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Pointwise  Conf. Band]  
#>          -3   0.0269     0.0158         -0.0040      0.0578  
#>          -2  -0.0050     0.0116         -0.0277      0.0178  
#>          -1  -0.0229     0.0131         -0.0485      0.0028  
#>           0  -0.0201     0.0112         -0.0421      0.0018  
#>           1  -0.0547     0.0161         -0.0864     -0.0231 *
#>           2  -0.1382     0.0364         -0.2095     -0.0669 *
#>           3  -0.1069     0.0346         -0.1747     -0.0391 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res_noif)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

What’s exciting about this is just how little new code needs to be
written.
