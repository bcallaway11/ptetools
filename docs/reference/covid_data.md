# State-level Covid-19 Data

A panel dataset containing Covid-19 related data for 46 states. This
data comes from Callaway and Li (2021). See the paper for additional
descriptions.

## Usage

``` r
covid_data
```

## Format

A data frame with 1656 rows and 9 variables:

- positive:

  The cumulative number of cases per million individuals in a particular
  state by a particular time period.

- time.period:

  Time period

- group:

  The group that a state belongs to. It is based on the time period when
  they enacted the shelter-in-place order.

- state:

  State abbreviation

- totalTestResults:

  The total Covid-19 number of tests run per million individuals in a
  particular state by a particular time period.

- state_id:

  Numeric state identifier

- region:

  Census region for particular state

- retail_and_recreation_percent_change_from_baseline:

  The percentage change in retail and recreational travel from pre-Covid
  baseline. This is from Google's Mobility report (see paper for
  details).

- current:

  The current number of cases per million individuals in a particular
  state by a particular time period. This variable is constructed from
  `positive` (see paper for details).

## Source

Callaway and Li (2021)
