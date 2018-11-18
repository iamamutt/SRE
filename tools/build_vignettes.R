build_vignettes <- function(...) {
  devtools::load_all(".", export_all=FALSE)
  load_required_sre_packages()
  build_all_supplementary(...)
}

old_opt <- options(warn=2) # default=0, 2 = stop on warn
build_vignettes(doc="all")
options(old_opt)
