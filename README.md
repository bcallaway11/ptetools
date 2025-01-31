
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
#>  -0.0305        0.0123    -0.0546     -0.0063 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0298     0.0158 -0.0111      0.0706  
#>          -2  -0.0024     0.0127 -0.0351      0.0302  
#>          -1  -0.0243     0.0147 -0.0621      0.0136  
#>           0  -0.0189     0.0124 -0.0508      0.0130  
#>           1  -0.0536     0.0181 -0.1001     -0.0070 *
#>           2  -0.1363     0.0424 -0.2456     -0.0269 *
#>           3  -0.1008     0.0351 -0.1914     -0.0103 *
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
#>  14.8882       70.9966  -124.2626    154.0389 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     3.1136  -12.1092      4.6560 
#>          -9   2.6607     1.7120   -1.9485      7.2699 
#>          -8   0.8290     2.4142   -5.6706      7.3286 
#>          -7   5.2843     2.4728   -1.3732     11.9419 
#>          -6   2.8555     1.6468   -1.5782      7.2892 
#>          -5   1.3589     3.7156   -8.6445     11.3623 
#>          -4   0.3294     4.4875  -11.7521     12.4109 
#>          -3  -4.2227     3.5722  -13.8400      5.3947 
#>          -2  -3.8447     3.0971  -12.1830      4.4935 
#>          -1  -0.2234     3.5584   -9.8037      9.3569 
#>           0 -10.8156     8.7835  -34.4632     12.8320 
#>           1 -13.7998    11.9623  -46.0057     18.4061 
#>           2  -7.8432    10.5984  -36.3770     20.6907 
#>           3  -4.5541    10.1508  -31.8830     22.7747 
#>           4  -3.5368    10.8100  -32.6404     25.5668 
#>           5   8.5221    11.9253  -23.5841     40.6283 
#>           6   1.1140    16.3982  -43.0345     45.2625 
#>           7   6.6384    19.3860  -45.5542     58.8311 
#>           8   7.1288    25.5462  -61.6487     75.9063 
#>           9  10.8758    28.1514  -64.9156     86.6673 
#>          10  17.5057    32.5023  -69.9996    105.0110 
#>          11  40.8318    43.1400  -75.3131    156.9767 
#>          12  48.6134    41.9574  -64.3475    161.5743 
#>          13  52.4228    50.3243  -83.0642    187.9098 
#>          14  50.2000    53.9236  -94.9772    195.3773 
#>          15  68.2960    72.5556 -127.0440    263.6360 
#>          16  44.7305    62.2640 -122.9016    212.3626 
#>          17  61.4670    90.7224 -182.7830    305.7169 
#>          18  50.4635   102.9626 -226.7404    327.6674 
#>          19  47.3392   103.1212 -230.2918    324.9701 
#>          20  28.6326   127.6975 -315.1646    372.4298 
#>          21   4.3445   138.8343 -369.4360    378.1250 
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
#>  -0.0323        0.0124    -0.0566      -0.008 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Pointwise  Conf. Band]  
#>          -3   0.0269     0.0157         -0.0039      0.0577  
#>          -2  -0.0050     0.0137         -0.0319      0.0220  
#>          -1  -0.0229     0.0136         -0.0495      0.0038  
#>           0  -0.0201     0.0118         -0.0433      0.0030  
#>           1  -0.0547     0.0167         -0.0875     -0.0220 *
#>           2  -0.1382     0.0367         -0.2102     -0.0662 *
#>           3  -0.1069     0.0315         -0.1686     -0.0452 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res_noif)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

What’s exciting about this is just how little new code needs to be
written.
