devtools::load_all(".")
load_required_sre_packages()
sre_models(refit = TRUE, save_dir = "inst/extdata", debug = FALSE)
