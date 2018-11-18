set_pkg_opts <- function(..., opt_list=NULL) {
  pkg_opts <- c(pairlist(...), as.pairlist(opt_list))

  if (is.null(pkg_opts)) {
    return(invisible())
  }

  opts <- names(options())
  pkg_opt_names <- names(pkg_opts)
  new_opts <- nzchar(pkg_opt_names) & !pkg_opt_names %in% opts

  if (any(new_opts)) {
    options(pkg_opts[pkg_opt_names[new_opts]])
  } else {
    invisible()
  }
}

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("mejr", quietly=TRUE)) {
    set_pkg_opts(
      SRE.mejr_pkg=TRUE, SRE.mejr_theme=TRUE, SRE.font_family="Oswald"
    )
    extrafont::loadfonts(quiet=TRUE)
  } else {
    set_pkg_opts(SRE.mejr_pkg=FALSE, SRE.mejr_theme=FALSE, SRE.font_family="serif")
  }
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("SRE package successfully attached.")
}
