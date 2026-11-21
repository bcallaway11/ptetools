

<!-- README.md is generated from README.qmd. Please edit that file -->

# Panel Treatment Effects Tools (ptetools) Package <img src="man/figures/logo.png" align="right" alt="ptetools" width="155" />

## Installation

You can install the released version of `ptetools` from CRAN with:

``` r
install.packages("ptetools")
```

or the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("bcallaway11/ptetools")
```

## Structure of panel data causal inference problems

The `ptetools` package compartmentalizes the steps needed to implement
estimators of group-time average treatment effects (and their
aggregations) in order to make it easier to apply the same sorts of
arguments outside of their “birthplace” in the literature on
difference-in-differences.

Essentially, the idea is that many panel data causal inference problems
involve steps such as:

1.  Defining an identification strategy (e.g.,
    difference-in-differences)

2.  Defining a notion of a group (e.g., based on treatment timing)

3.  Looping over groups and time periods

4.  Organizing the data so that the correct data show up for each group
    and time period

5.  Computing group-time average treatment effects (or other parameters
    that are local to a group and a time period)

6.  Aggregating group-time average treatment effect parameters (e.g.,
    into an event study or an overall average treatment effect
    parameter)

Many of these steps are common across different panel data causal
inference settings. For example, you could implement a
difference-in-differences identification strategy or a change-in-changes
identification strategy with all of the same steps as above except for
replacing step 1.

The idea of the `ptetools` package is to re-use as much
code/infrastructure as possible when developing new approaches to panel
data causal inference. For example, `ptetools` sits as the “backend” for
several other packages including: `ife`, `contdid`, `badcontrols`, and
parts of `qte`.

## How ptetools works

The main function is called `pte`. The most important parameters that it
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
about treatment effects with panel data, and this function can be reused
across applications.

The other main function is `attgt_fun`. This function should be able to
take in the correct subset of data, possibly along with other arguments
to the function, and report an *ATT* for that subset. With minor
modification, this function should be available for most any sort of
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
any time period) and and event study. More aggregations are possible,
but these seem to be the leading cases; aggregations of group-time
average treatment effects are discussed at length in [Callaway and
Sant’Anna (2021)](https://doi.org/10.1016/j.jeconom.2020.12.001).

Estimators built on `ptetools` also inherit several features without any
extra code:

- Sampling weights, through the `weightsname` argument.

- Clustered standard errors, through the `clustervars` argument (e.g.,
  clustering counties by state). Clustering applies to analytical
  standard errors, the multiplier bootstrap, and the empirical
  bootstrap.

- Overlap diagnostics for each group-time cell, through
  `check_overlap()`, and warnings that are collected and reported once
  per call (rather than once per group-time cell) when, for example, a
  covariate has to be dropped in some cells.

Below are several examples of how the `ptetools` package can be used to
implement an identification strategy with a very small amount of new
code.

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
#>  -0.0305        0.0155    -0.0608      -1e-04 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Simult.  Conf. Band]  
#>          -3   0.0298     0.0153       -0.0142      0.0738  
#>          -2  -0.0024     0.0119       -0.0367      0.0318  
#>          -1  -0.0243     0.0148       -0.0668      0.0183  
#>           0  -0.0189     0.0121       -0.0538      0.0159  
#>           1  -0.0536     0.0181       -0.1056     -0.0015 *
#>           2  -0.1363     0.0376       -0.2442     -0.0283 *
#>           3  -0.1008     0.0369       -0.2068      0.0052  
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggplot2::autoplot(did_res)
```

<img src="man/figures/README-unnamed-chunk-5-1.png"
data-fig-alt="Event-study plot of ATT estimates by event time, with uniform confidence bands, for the minimum wage example." />

What’s most interesting here, is that the only “new” code that needs to
be written is in [the `did_attgt`
function](https://github.com/bcallaway11/ptetools/blob/master/R/attgt_functions.R).
You will see that this is a very small amount of code.

## Example 2: Policy Evaluation during a Pandemic

As a next example, consider trying to estimate effects of COVID-19
related policies during a pandemic. The estimates below are for the
effects of state-level shelter-in-place orders during the early part of
the pandemic.

The data for this example is included in the `ptetools` package:

``` r
data(covid_data)
```

[Callaway and Li (2021)](https://arxiv.org/abs/2105.06927) argue that a
particular unconfoundedness-type strategy is more appropriate in this
context than DID-type strategies due to the spread of COVID-19 cases
being highly nonlinear. However, they still deal with the challenge of
variation in treatment timing. Therefore, it is still useful to think
about group-time average treatment effects, but the DID strategy should
be replaced with their particular unconfoundedness type assumption.

The `ptetools` package handles this through `pte_default` with
`lagged_outcome_cov = TRUE`.

``` r
# formula for covariates
xformla <- ~ current + I(current^2) + region + totalTestResults
```

``` r
covid_res <- pte_default(
  yname = "positive",
  gname = "group",
  tname = "time.period",
  idname = "state_id",
  data = covid_data2,
  xformula = xformla,
  d_outcome = FALSE,
  lagged_outcome_cov = TRUE,
  max_e = 21,
  min_e = -10
)

summary(covid_res)
#> 
#> Overall ATT:  
#>       ATT    Std. Error     [ 95%  Conf. Int.] 
#>  -13.2547       78.2922  -166.7046    140.1952 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Simult.  Conf. Band] 
#>         -10  -6.3170     5.9091      -22.0136      9.3796 
#>          -9   2.1087     1.1906       -1.0538      5.2712 
#>          -8  -2.7721     1.4866       -6.7210      1.1768 
#>          -7   3.8965     1.9004       -1.1516      8.9446 
#>          -6  -1.0588     2.6696       -8.1501      6.0325 
#>          -5   2.4337     6.1332      -13.8582     18.7256 
#>          -4   0.2436     1.3210       -3.2654      3.7526 
#>          -3  -1.3052     4.8644      -14.2266     11.6163 
#>          -2   0.4594     2.0423       -4.9655      5.8844 
#>          -1   4.0376     2.1559       -1.6891      9.7643 
#>           0  -3.2518     3.7357      -13.1749      6.6714 
#>           1  -7.4231    11.4175      -37.7518     22.9056 
#>           2  -0.3148     7.9784      -21.5080     20.8783 
#>           3   2.7880     8.7694      -20.5064     26.0824 
#>           4   1.3625    11.1558      -28.2710     30.9960 
#>           5  -2.9563    20.5049      -57.4242     51.5115 
#>           6 -17.2610    31.3154     -100.4450     65.9230 
#>           7 -19.2258    35.4103     -113.2873     74.8356 
#>           8 -21.5150    37.0396     -119.9042     76.8743 
#>           9 -25.2793    48.3840     -153.8031    103.2444 
#>          10 -23.0541    45.4677     -143.8314     97.7232 
#>          11  -3.8346    58.9140     -160.3295    152.6602 
#>          12  -5.6789    73.5857     -201.1467    189.7889 
#>          13 -12.4977    70.4054     -199.5177    174.5223 
#>          14 -19.6199    90.4435     -259.8677    220.6279 
#>          15 -11.6638   122.1818     -336.2190    312.8913 
#>          16 -15.0522    80.1959     -228.0789    197.9745 
#>          17  -3.3234   101.4282     -272.7500    266.1032 
#>          18 -14.3328    89.8926     -253.1172    224.4515 
#>          19 -26.1852   149.5505     -423.4406    371.0702 
#>          20 -40.7039   123.9819     -370.0406    288.6329 
#>          21 -58.7364   161.4025     -487.4746    370.0018 
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggplot2::autoplot(covid_res) + ylim(c(-1000, 1000))
```

<img src="man/figures/README-unnamed-chunk-9-1.png"
data-fig-alt="Event-study plot of ATT estimates by event time, with uniform confidence bands, for the COVID-19 shelter-in-place example." />

What’s most interesting is just how little code needs to be written
here. The `pte_default` function handles the unconfoundedness-type
identification strategy directly, with no custom `attgt_fun` required.

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
#>  -0.0323        0.0113    -0.0542     -0.0103 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Simult.  Conf. Band]  
#>          -3   0.0269     0.0132        0.0011      0.0528 *
#>          -2  -0.0050     0.0130       -0.0304      0.0204  
#>          -1  -0.0229     0.0141       -0.0506      0.0049  
#>           0  -0.0201     0.0111       -0.0418      0.0015  
#>           1  -0.0547     0.0165       -0.0871     -0.0224 *
#>           2  -0.1382     0.0360       -0.2088     -0.0676 *
#>           3  -0.1069     0.0335       -0.1726     -0.0412 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggplot2::autoplot(did_res_noif)
```

<img src="man/figures/README-unnamed-chunk-11-1.png"
data-fig-alt="Event-study plot of ATT estimates by event time, with pointwise confidence intervals, from the empirical bootstrap version of the minimum wage example." />

What’s exciting about this is just how little new code needs to be
written.
