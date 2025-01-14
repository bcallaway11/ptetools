process_dose_gt <- function(gt_results, ptep, ...) {
    browser()

    o_weights <- overall_weights(gt_results, ...)

    # extract ATT(g,t) and influence functions
    attgt.list <- gt_results$attgt.list
    inffunc <- gt_results$inffunc
}
