#' Self-Recognition Experiment (SRE)
#'
#' Descrip
#'
#' See `help(package = "SRE")` for a list of functions.
#'
#' View the vignette with `browseVignettes(package ="SRE")`.
#'
#' @name SRE
#' @docType package
#' @import data.table
#' @import rstanarm
#' @import ggplot2
#' @import rbaes
#' @importFrom ggdistribute geom_posterior
NULL

#' Pipe operator
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @export
rbaes::merge_data_and_posterior

#' Self-Recognition Experiment (SRE) dataset
#'
#' @format Dataset is an object of class [data.frame].
#' @section Variables:
#' - `recog_acc`: self-recognition accuracy
#' @seealso [import_sre()]
#' @return `sre` returns an object of class [data.frame].
#' @examples
#' # get data
#' library(SRE)
#' sre_copy <- sre
"sre"
