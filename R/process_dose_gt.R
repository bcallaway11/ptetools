process_dose_gt <- function(gt_results, ptep, ...) {
    browser()

    # make the call to att, to get same format of results
    att_gt <- process_att_gt(gt_results, ptep)
    o_weights <- overall_weights(att_gt, ...)

    1 + 1
}
