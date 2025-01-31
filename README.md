
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
#> Warning in pte(yname = "lemp", gname = "first.treat", tname = "year", idname = "countyreal", : 'pte' is deprecated.
#> Use 'pte2' instead.
#> See help("Deprecated")
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal

summary(did_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0135    -0.0587     -0.0058 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0112 -0.0053      0.0591  
#>          -2  -0.0050     0.0126 -0.0413      0.0313  
#>          -1  -0.0229     0.0124 -0.0584      0.0127  
#>           0  -0.0201     0.0139 -0.0602      0.0199  
#>           1  -0.0547     0.0173 -0.1046     -0.0049 *
#>           2  -0.1382     0.0337 -0.2350     -0.0414 *
#>           3  -0.1069     0.0340 -0.2046     -0.0092 *
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
#> Warning in pte(yname = "positive", gname = "group", tname = "time.period", : 'pte' is deprecated.
#> Use 'pte2' instead.
#> See help("Deprecated")
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal
#> Warning in mboot2(thisinffunc, biters = biters, alp = alp): critical value for uniform confidence band is somehow smaller than
#>             critical value for pointwise confidence interval...using pointwise
#>             confidence interal

summary(covid_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.] 
#>  14.8882       89.2491  -160.0369    189.8132 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band]  
#>         -10  -3.7266     4.0893  -13.5376      6.0843  
#>          -9   2.6607     1.4131   -0.7295      6.0508  
#>          -8   0.8290     2.1182   -4.2528      5.9107  
#>          -7   5.2843     2.0558    0.3523     10.2164 *
#>          -6   2.8555     2.3030   -2.6698      8.3808  
#>          -5   1.3589     4.0842   -8.4397     11.1575  
#>          -4   0.3294     3.4489   -7.9451      8.6039  
#>          -3  -4.2227     4.6681  -15.4221      6.9768  
#>          -2  -3.8447     2.9548  -10.9336      3.2441  
#>          -1  -0.2234     3.4588   -8.5214      8.0747  
#>           0 -10.8156     8.4755  -31.1497      9.5184  
#>           1 -13.7998    12.9474  -44.8626     17.2630  
#>           2  -7.8432     9.9894  -31.8092     16.1228  
#>           3  -4.5541     8.6319  -25.2634     16.1552  
#>           4  -3.5368    11.3025  -30.6532     23.5795  
#>           5   8.5221    12.1537  -20.6363     37.6805  
#>           6   1.1140    17.0145  -39.7064     41.9343  
#>           7   6.6384    15.9018  -31.5122     44.7891  
#>           8   7.1288    25.9489  -55.1264     69.3840  
#>           9  10.8758    33.4554  -69.3886     91.1403  
#>          10  17.5057    30.2446  -55.0556     90.0670  
#>          11  40.8318    35.3309  -43.9323    125.5959  
#>          12  48.6134    45.3943  -60.2943    157.5210  
#>          13  52.4228    59.6057  -90.5799    195.4255  
#>          14  50.2000    55.9394  -84.0068    184.4068  
#>          15  68.2960    67.1875  -92.8965    229.4885  
#>          16  44.7305    67.4749 -117.1516    206.6126  
#>          17  61.4670    82.0294 -135.3336    258.2675  
#>          18  50.4635   104.0203 -199.0964    300.0234  
#>          19  47.3392   129.1065 -262.4062    357.0845  
#>          20  28.6326   109.0253 -232.9351    290.2003  
#>          21   4.3445   129.9741 -307.4822    316.1713  
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
#> Warning in pte(yname = "lemp", gname = "first.treat", tname = "year", idname = "countyreal", : 'pte' is deprecated.
#> Use 'pte2' instead.
#> See help("Deprecated")

summary(did_res_noif)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0125    -0.0568     -0.0077 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Pointwise  Conf. Band]  
#>          -3   0.0269     0.0140         -0.0005      0.0544  
#>          -2  -0.0050     0.0131         -0.0307      0.0208  
#>          -1  -0.0229     0.0155         -0.0531      0.0074  
#>           0  -0.0201     0.0123         -0.0442      0.0039  
#>           1  -0.0547     0.0170         -0.0881     -0.0214 *
#>           2  -0.1382     0.0335         -0.2039     -0.0725 *
#>           3  -0.1069     0.0344         -0.1744     -0.0394 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res_noif)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

What’s exciting about this is just how little new code needs to be
written.
