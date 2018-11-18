#' Self-Recognition Experiment (SRE)
#'
#' The `SRE` package (Self-recognition Experiment) provides
#' supplementary analyses and data for the paper
#' *The Impact of Autistic Traits on Self-Recognition of Body Movements*.
#'
#' See `help(package = "SRE")` for a list of functions.
#'
#' View the vignette with `browseVignettes(package ="SRE")`.
#'
#' @name SRE-package
#' @docType package
#' @import data.table
#' @import rstanarm
#' @import ggplot2
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

#' Self-Recognition Experiment (SRE) dataset
#'
#' @format Dataset is an object of class [data.frame].
#' @section Variables:
#' - `recog_acc`: self-recognition accuracy
#' @seealso [import_sre()]
#' @return `SRE` returns an object of class [data.frame].
#' @examples
#' # get data
#' library(SRE)
#' sre_copy <- SRE
"SRE"
