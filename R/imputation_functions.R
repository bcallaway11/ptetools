# =============================================================================
# Title: Imputation Functions (Stub)
# Description: Placeholder for TWFE imputation estimator. Not yet implemented
#   or used; excluded from the package build via .Rbuildignore.
# Author: Brant Callaway
# Last update: 2026-05-25
# Date created: 2021-05-19
# =============================================================================

# --- twfe_imputation (stub) --------------------------------------------------
# Not yet implemented. Intended to estimate untreated potential outcomes via
# TWFE imputation (Borusyak, Jaravel, Spiess 2024). Requires fixest, which is
# not in DESCRIPTION. Do not use or export until implementation is complete.
#' @keywords internal
twfe_imputation <- function(data, ptep) {
  stop("twfe_imputation is not yet implemented")
  yname <- ptep$yname
  idname <- ptep$idname
  tname <- ptep$tname
  gname <- ptep$gname
  
  formla <- paste0(yname, "~", 0)
  formla <- paste0(formla, " | ", idname, " + ", tname)
  formla <- as.formula(formla)

  pre_data <- data[ ( data[,tname] < data[,gname] ) | data[,gname]==0, ]

  twfe_est <- fixest::feols(formla, data=pre_data)

  y0 <- predict(twfe_est, newdata=data)

  y0
}
