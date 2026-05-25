# =============================================================================
# Title: Run revdeplite
# Description: Run lightweight reverse dependency checks for ptetools.
# Author: Brant Callaway
# Last update: 2026-05-25
# Date created: 2026-05-25
# =============================================================================

revdeplite::revdeplite(
  github_deps = c(
    "bcallaway11/contdid",
    "bcallaway11/qte",
    "bcallaway11/ife"
  ),
  check_dir = ".revdeplite"
)
