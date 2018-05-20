# export ------------------------------------------------------------------

#' Import SRE dataset
#'
#' @param from_csv Load from CSV or RData. `Logical`.
#' @param raw Import data without refactoring or standardizing.
#' @return [data.frame]
#' @export
#' @examples
#' import_sre()         # load from compressed RData
#' import_sre(TRUE)     # reload from package CSV data
#' import_sre(raw=TRUE) # load without refactoring and scaling.
import_sre <- function(from_csv = FALSE, raw = FALSE) {
  if (!from_csv && !raw) {
    data(sre, package = "SRE", envir = environment())
    return(sre)
  }

  # read in pre-sanitized data
  sre_raw <- data.table::fread(sre_file_path())

  if (raw) {
    return(as.data.frame(sre_raw))
  }

  # change the following variables to factors
  to_fac <- c(
    "id", "sub_sex", "aq_category", "action_category",
    "action", "recog_corr_box", "recog_resp_box")

  # rescale numerics
  to_scale <- c("sub_age", "aq_score", "recog_rt", "trial")

  # clean up for model
  sre_raw %>%
    .[, eval(to_fac) := lapply(.SD, as.factor), .SDcols = to_fac] %>%
    .[, aq_category := relevel(aq_category, "high symptoms")] %>%
    .[, sub_sex := relevel(sub_sex, "f")] %>%
    .[, angle := factor(angle, levels = c(180L, 0L, 135L, 247L))] %>%
    .[, eval(paste0(to_scale, "_scaled")) := lapply(
      .SD, rbaes::unit_scale
    ), .SDcols = to_scale] %>%
    .[, aq_score_standard := aq_standardize(aq_score)] %>%
    as.data.frame()
}

#' Rescale AQ score given specific mean and variance
#'
#' Use mean and standard deviation from the non-clinical population.
#'
#' Resulting scale will have a mean of 0.5 with most of the mass between 0 and
#' 1.
#' @param x Original AQ score
#' @param mu Population mean
#' @param sigma Population standard deviation
#'
#' @return [numeric] vector with attributes to convert back to original
#' @export
#' @seealso
#' [Ruzich, E. et. al. (2014)](https://molecularautism.biomedcentral.com/articles/10.1186/2040-2392-6-2)
#' @examples
#' aq_standardize(25)
aq_standardize <- function(x, mu = 16.94, sigma = 5.59) {
  sigma_dbl <- 2 * sigma
  adj <- 0.5
  structure(adj + ((x - mu) / sigma_dbl),
    center = mu,
    scale = sigma_dbl, adj = adj)
}

# helpers -----------------------------------------------------------------

#' @export
sre_extdata <- function() {
  system.file("extdata", package = "SRE", mustWork = TRUE)
}

#' @export
sre_file_path <- function() {
  file.path(sre_extdata(), .sre_csv_basename)
}

#' @export
print_data_sample <- function(x, n = 5, i = seq_len(n), ...) {
  nx <- nrow(x)
  cont <- ifelse(n < nx, "...", NA_character_)
  invisible(sapply(
    names(x),
    function(nm) {
      vals <- x[[nm]]
      val_sample <- prettyNum(vals[i], ...)
      if (n > 1) {
        val_sample <- paste0(
          "[", paste(na.omit(c(val_sample, cont)),
            collapse = ", "), "]"
        )
      }
      cat(sprintf(
        "\n- `%s` (*%s*): %s", nm,
        class(vals), val_sample))
    }))
  cat("\n")
}

#' @export
copy_dt <- function(x = NULL, shorten_names = FALSE) {
  if (is.null(x)) {
    x <- import_sre()
  }

  x <- copy(as.data.table(x))

  if (shorten_names) {
    n <- names(x)

    if ("aq_category" %in% n) {
      x <- dt_fac_rename(
        x, "aq_category",
        c("high symptoms", "low symptoms", "all"),
        c("High", "Low", "."), ".")
      setnames(x, "aq_category", "AQ")
    }

    if ("action_category" %in% n) {
      x <- dt_fac_rename(
        x, "action_category",
        c("simple actions", "complex actions", "all"),
        c("Simple", "Complex", "."), ".")
      setnames(x, "action_category", "Actions")
    }

    if ("id" %in% n) {
      x[, id := as.integer(id)]
    }

    if ("sub_sex" %in% n) {
      x <- dt_fac_rename(
        x, "sub_sex", c("f", "m", "all"),
        c("F", "M", "."), ".")
      setnames(x, "sub_sex", "Sex")
    }
  }
  invisible(x)
}

#' @export
panda_print <- function(x, caption = "SRE Table.", digits = 5,
                        justify = "left", ...) {
  pander::pandoc.table(x,
    caption = caption, digits = digits,
    justify = justify, ...)
}
