
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Panel Treatment Effects (pte) Package <img src="man/figures/logo.png" align="right" alt="pte" width="155" />

The `pte` package compartmentalizes the steps needed to implement
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
function](https://github.com/bcallaway11/pte/blob/master/R/subset_functions.R).
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
function here, and the `pte` package will take care of dealing with the
variation in treatment timing.

If `attgt_fun` returns an influence function, then the `pte` package
will also conduct inference using the multiplier bootstrap (which is
fast) and produce uniform confidence bands (which adjust for multiple
testing).

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
treatment effects using difference in differences using the `pte`
package. \[*Note:* This is definitely not the recommended way of doing
this as there is very little error handling, etc. here, but it is rather
a proof of concept. You should use the `did` package for this case.\]

This example reproduces DID estimates of the effect of the minimum wage
on employment using data from the `did` package.

``` r
library(did)
data(mpdta)
did_res <- pte(yname="lemp",
               gname="first.treat",
               tname="year",
               idname="countyreal",
               data=mpdta,
               setup_pte_fun=setup_pte,
               subset_fun=two_by_two_subset,
               attgt_fun=did_attgt,
               xformla=~lpop) 
#> Warning in pte(yname = "lemp", gname = "first.treat", tname = "year", idname = "countyreal", : 'pte' is deprecated.
#> Use 'pte2' instead.
#> See help("Deprecated")

summary(did_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0126     -0.057     -0.0075 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0154 -0.0131      0.0670  
#>          -2  -0.0050     0.0129 -0.0385      0.0286  
#>          -1  -0.0229     0.0144 -0.0603      0.0145  
#>           0  -0.0201     0.0117 -0.0506      0.0103  
#>           1  -0.0547     0.0165 -0.0976     -0.0118 *
#>           2  -0.1382     0.0396 -0.2410     -0.0354 *
#>           3  -0.1069     0.0341 -0.1954     -0.0184 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

What’s most interesting here, is that the only “new” code that needs to
be writte is in [the `did_attgt`
function](https://github.com/bcallaway11/pte/blob/master/R/attgt_functions.R).
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

The `pte` package is particularly useful here.

``` r
# formula for covariates
xformla <- ~ current + I(current^2) + region + totalTestResults
```

``` r
covid_res <- pte(yname="positive",
                 gname="group",
                 tname="time.period",
                 idname="state_id",
                 data=covid_data2,
                 setup_pte_fun=setup_pte_basic,
                 subset_fun=two_by_two_subset,
                 attgt_fun=covid_attgt,
                 xformla=xformla,
                 max_e=21,
                 min_e=-10) 
#> Warning in pte(yname = "positive", gname = "group", tname = "time.period", : 'pte' is deprecated.
#> Use 'pte2' instead.
#> See help("Deprecated")
#> Warning in compute.aggte(MP = MP, type = type, balance_e = balance_e, min_e =
#> min_e, : Simultaneous conf. band is somehow smaller than pointwise one using
#> normal approximation. Since this is unusual, we are reporting pointwise
#> confidence intervals

summary(covid_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.] 
#>  14.8882       65.6876  -113.8573    143.6336 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     3.2311  -12.3090      4.8557 
#>          -9   2.6607     1.5006   -1.3252      6.6465 
#>          -8   0.8290     2.2907   -5.2556      6.9136 
#>          -7   5.2843     2.2970   -0.8168     11.3855 
#>          -6   2.8555     1.6468   -1.5186      7.2296 
#>          -5   1.3589     4.6708  -11.0476     13.7654 
#>          -4   0.3294     3.4672   -8.8801      9.5388 
#>          -3  -4.2227     4.1922  -15.3579      6.9125 
#>          -2  -3.8447     2.9210  -11.6033      3.9138 
#>          -1  -0.2234     3.4245   -9.3195      8.8727 
#>           0 -10.8156     9.3885  -35.7531     14.1218 
#>           1 -13.7998    15.7824  -55.7205     28.1209 
#>           2  -7.8432    11.4501  -38.2566     22.5703 
#>           3  -4.5541    12.2315  -37.0430     27.9347 
#>           4  -3.5368    12.3862  -36.4367     29.3631 
#>           5   8.5221    11.9115  -23.1167     40.1609 
#>           6   1.1140    16.5244  -42.7776     45.0056 
#>           7   6.6384    19.8568  -46.1044     59.3813 
#>           8   7.1288    28.8276  -69.4422     83.6998 
#>           9  10.8758    25.2019  -56.0647     77.8163 
#>          10  17.5057    32.3052  -68.3023    103.3137 
#>          11  40.8318    41.9009  -70.4639    152.1275 
#>          12  48.6134    41.2350  -60.9137    158.1404 
#>          13  52.4228    57.2359  -99.6052    204.4508 
#>          14  50.2000    71.5414 -139.8257    240.2257 
#>          15  68.2960    58.4186  -86.8735    223.4655 
#>          16  44.7305    77.2877 -160.5585    250.0195 
#>          17  61.4670    97.5688 -197.6919    320.6258 
#>          18  50.4635    96.0637 -204.6976    305.6246 
#>          19  47.3392   117.3176 -264.2759    358.9542 
#>          20  28.6326   159.2878 -394.4622    451.7275 
#>          21   4.3445   120.6413 -316.0987    324.7878 
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(covid_res) + ylim(c(-1000,1000))
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

The `pte` package can be used with the empirical bootstrap. There are a
few limitations. First, it’s going to be substantially slower. Second,
this code just reports pointwise confidence intervals. However, this
basically is set up to fit into my typical workflow, and I see this as a
way to get preliminary results.

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
did_res_noif <- pte(yname="lemp",
                    gname="first.treat",
                    tname="year",
                    idname="countyreal",
                    data=mpdta,
                    setup_pte_fun=setup_pte,
                    subset_fun=two_by_two_subset,
                    attgt_fun=did_attgt_noif, #this is only diff.
                    xformla=~lpop) 
#> Warning in pte(yname = "lemp", gname = "first.treat", tname = "year", idname = "countyreal", : 'pte' is deprecated.
#> Use 'pte2' instead.
#> See help("Deprecated")

summary(did_res_noif)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323         0.012    -0.0558     -0.0087 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Pointwise  Conf. Band]  
#>          -3   0.0269     0.0140         -0.0005      0.0544  
#>          -2  -0.0050     0.0134         -0.0312      0.0213  
#>          -1  -0.0229     0.0125         -0.0473      0.0016  
#>           0  -0.0201     0.0121         -0.0439      0.0036  
#>           1  -0.0547     0.0170         -0.0881     -0.0213 *
#>           2  -0.1382     0.0332         -0.2032     -0.0732 *
#>           3  -0.1069     0.0324         -0.1704     -0.0434 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res_noif)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

What’s exciting about this is just how little new code needs to be
written.
