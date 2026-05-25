# Package index

## Generic functions for panel data causal inference

- [`attgt_pte_aggregations()`](https://github.com/bcallaway11/ptetools/reference/attgt_pte_aggregations.md)
  : Aggregate Group-Time Average Treatment Effects
- [`compute.pte()`](https://github.com/bcallaway11/ptetools/reference/compute.pte.md)
  : Heavy-Lifting for pte Function
- [`gt_data_frame()`](https://github.com/bcallaway11/ptetools/reference/gt_data_frame.md)
  : Convert Data to Usable Format
- [`overall_weights()`](https://github.com/bcallaway11/ptetools/reference/overall_weights.md)
  : Weights for Overall Aggregation
- [`panel_empirical_bootstrap()`](https://github.com/bcallaway11/ptetools/reference/panel_empirical_bootstrap.md)
  : Panel Empirical Bootstrap
- [`process_att_gt()`](https://github.com/bcallaway11/ptetools/reference/process_att_gt.md)
  : Process ATT(g,t) Results
- [`pte()`](https://github.com/bcallaway11/ptetools/reference/pte.md) :
  Panel Treatment Effects
- [`pte_aggte()`](https://github.com/bcallaway11/ptetools/reference/pte_aggte.md)
  : Aggregates (g,t)-Specific Results
- [`pte_attgt()`](https://github.com/bcallaway11/ptetools/reference/pte_attgt.md)
  : General ATT(g,t)
- [`setup_pte()`](https://github.com/bcallaway11/ptetools/reference/setup_pte.md)
  : Generic Setup Function
- [`setup_pte_basic()`](https://github.com/bcallaway11/ptetools/reference/setup_pte_basic.md)
  : Basic Setup Function

## Group-time average treatment effects with a binary treatment

- [`covid_attgt()`](https://github.com/bcallaway11/ptetools/reference/covid_attgt.md)
  : Covid ATT(g,t) Estimator
- [`did_attgt()`](https://github.com/bcallaway11/ptetools/reference/did_attgt.md)
  : Difference-in-differences for ATT(g,t)
- [`did_rcs_attgt()`](https://github.com/bcallaway11/ptetools/reference/did_rcs_attgt.md)
  : Repeated Cross Sections Difference-in-Differences for ATT(g,t)
- [`pte_default()`](https://github.com/bcallaway11/ptetools/reference/pte_default.md)
  : Default, General Function for Computing Treatment Effects with Panel
  Data
- [`two_by_two_rcs_subset()`](https://github.com/bcallaway11/ptetools/reference/two_by_two_rcs_subset.md)
  : Two Period Two Group Repeated Cross Sections Subset

## Functions for dealing with a continuous treatment

- [`pte_dose_results()`](https://github.com/bcallaway11/ptetools/reference/pte_dose_results.md)
  : Class for Continuous Treatment Results
- [`process_dose_gt()`](https://github.com/bcallaway11/ptetools/reference/process_dose_gt.md)
  : Process Results with a Continuous Treatment

## Functions for dealing with distributional treatment effects

- [`qott_pte_aggregations()`](https://github.com/bcallaway11/ptetools/reference/qott_pte_aggregations.md)
  : Aggregate Group-Time Quantile of the Treatment Effect
- [`qtt_empirical_bootstrap()`](https://github.com/bcallaway11/ptetools/reference/qtt_empirical_bootstrap.md)
  : Empirical Bootstrap for QTT Curves
- [`qtt_pte_aggregations()`](https://github.com/bcallaway11/ptetools/reference/qtt_pte_aggregations.md)
  : Aggregate Group-Time Quantile Treatment Effects

## Functions for group-time specific subsets of data

- [`keep_all_pretreatment_subset()`](https://github.com/bcallaway11/ptetools/reference/keep_all_pretreatment_subset.md)
  : Keep All Pre-Treatment Subset
- [`keep_all_untreated_subset()`](https://github.com/bcallaway11/ptetools/reference/keep_all_untreated_subset.md)
  : Keep All Untreated Subset
- [`two_by_two_subset()`](https://github.com/bcallaway11/ptetools/reference/two_by_two_subset.md)
  : Two Period Two Group Subset

## Plotting functions

- [`autoplot(`*`<dose_obj>`*`)`](https://github.com/bcallaway11/ptetools/reference/autoplot.dose_obj.md)
  : autoplot.dose_obj
- [`autoplot(`*`<pte_emp_boot>`*`)`](https://github.com/bcallaway11/ptetools/reference/autoplot.pte_emp_boot.md)
  : autoplot.pte_emp_boot
- [`autoplot(`*`<pte_qtt>`*`)`](https://github.com/bcallaway11/ptetools/reference/autoplot.pte_qtt.md)
  : autoplot.pte_qtt
- [`autoplot(`*`<pte_results>`*`)`](https://github.com/bcallaway11/ptetools/reference/autoplot.pte_results.md)
  : autoplot.pte_results
- [`ggpte()`](https://github.com/bcallaway11/ptetools/reference/ggpte.md)
  : ggpte
- [`ggpte_cont()`](https://github.com/bcallaway11/ptetools/reference/ggpte_cont.md)
  : ggpte_cont
- [`plot(`*`<dose_obj>`*`)`](https://github.com/bcallaway11/ptetools/reference/plot.dose_obj.md)
  : plot.dose_obj
- [`plot(`*`<pte_emp_boot>`*`)`](https://github.com/bcallaway11/ptetools/reference/plot.pte_emp_boot.md)
  : plot.pte_emp_boot
- [`plot(`*`<pte_qtt>`*`)`](https://github.com/bcallaway11/ptetools/reference/plot.pte_qtt.md)
  : plot.pte_qtt
- [`plot(`*`<pte_results>`*`)`](https://github.com/bcallaway11/ptetools/reference/plot.pte_results.md)
  : plot.pte_results

## Helper functions

- [`crit_val_checks()`](https://github.com/bcallaway11/ptetools/reference/crit_val_checks.md)
  : Sanity Checks on Critical Values
- [`mboot2()`](https://github.com/bcallaway11/ptetools/reference/mboot2.md)
  : Multiplier Bootstrap

## Classes

- [`aggte_obj()`](https://github.com/bcallaway11/ptetools/reference/aggte_obj.md)
  : Aggregated Treatment Effects Class
- [`attgt_if()`](https://github.com/bcallaway11/ptetools/reference/attgt_if.md)
  : Class for (g,t)-Specific Results with Influence Function
- [`attgt_noif()`](https://github.com/bcallaway11/ptetools/reference/attgt_noif.md)
  : Class for (g,t)-Specific Results without Influence Function
- [`dose_obj()`](https://github.com/bcallaway11/ptetools/reference/dose_obj.md)
  : Class for Continuous Treatments
- [`group_time_att()`](https://github.com/bcallaway11/ptetools/reference/group_time_att.md)
  : Class for Estimates across Groups and Time
- [`pte_emp_boot()`](https://github.com/bcallaway11/ptetools/reference/pte_emp_boot.md)
  : Class for Empirical Bootstrap Results
- [`pte_params()`](https://github.com/bcallaway11/ptetools/reference/pte_params.md)
  : PTE Parameters Class
- [`pte_qtt()`](https://github.com/bcallaway11/ptetools/reference/pte_qtt.md)
  : Class for QTT Curve Results
- [`pte_results()`](https://github.com/bcallaway11/ptetools/reference/pte_results.md)
  : Class for PTE Results

## Data

- [`covid_data`](https://github.com/bcallaway11/ptetools/reference/covid_data.md)
  : State-level Covid-19 Data
