# general -----------------------------------------------------------------

`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}

dt_fac_rename <- function(dt, fac, lvls, lbls, na_lvl = NULL) {
  if (!is.null(na_lvl)) {
    lvls <- c(lvls, NA)
    lbls <- c(lbls, na_lvl)
    exclude <- NULL
  } else {
    exclude <- NA
  }
  dt[, eval(fac) := factor(get(fac), lvls, lbls, exclude = exclude)]
  invisible(dt)
}

attach_required_packages <- function() {
  suppressPackageStartupMessages({
    library(pander)
    library(data.table)
    library(rbaes)
    library(ggdistribute)
  })
}

save_as <- function(..., file, list = NULL) {
  envr <- c(list, list(...))
  arg_list <- list(
    list = names(envr), file = file, precheck = FALSE,
    envir = list2env(envr), compress = "bzip2")
  do.call(save, arg_list)
  invisible()
}

cap_mid <- function(x, prefix = "", suffix = "") {
  get_names <- names(post_int_sre(1:5))
  setnames(
    x, get_names[1],
    paste0(prefix, chartr("m", "M", get_names[1]), suffix))
  invisible(x)
}

make_dir <- function(target, root = NULL, recursive = TRUE, warn = TRUE) {
  if (is.null(root)) {
    paths <- target
  } else {
    if (length(root) > 1) {
      stop("`root` argument must be of length 1")
    }
    if (!dir.exists(root)) {
      stop("Directory specified in `root` does not exist: ", root)
    }
    paths <- file.path(root, target)
  }

  success <- lapply(
    paths,
    function(p) {
      if (dir.exists(p)) {
        return(p)
      } else {
        if (dir.create(p,
          showWarnings = warn,
          recursive = recursive)) {
          return(p)
        } else {
          return(character())
        }
      }
    })

  unlist(success)
}

build_all_supplementary <- function(doc = c(
                                      "all", "pdf_document",
                                      "html_document")) {
  rmd <- "vignettes/supplementary.Rmd"

  if (!file.exists(rmd)) {
    warning("RMD file not found at ", rmd, " from ", getwd(), call. = FALSE)
    return()
  }

  output <- make_dir("inst/doc")

  if (length(output) < 1) {
    warning("Output directory not found.", call. = FALSE)
    return()
  }

  basefile <- tools::file_path_sans_ext(rmd)
  r_file <- paste0(basefile, ".R")
  pdf_file <- paste0(basefile, ".pdf")
  html_file <- paste0(basefile, ".html")
  # tex_file <- paste0(basefile, ".tex")
  on.exit(unlink(c(pdf_file, html_file, r_file)))
  rmarkdown::render(rmd, output_format = match.arg(doc), clean = TRUE)
  knitr::purl(rmd, r_file, documentation = 0)
  file.copy(c(pdf_file, html_file, r_file), to = output, overwrite = TRUE)
  invisible()
}

# package -----------------------------------------------------------------

.sre_csv_basename <- "sre.csv"

save_pkg_data <- function() {
  sre <- import_sre(from_csv = TRUE, raw = FALSE)
  devtools::use_data(sre, overwrite = TRUE, compress = "xz")
}


# analyses ----------------------------------------------------------------

summary_tbl <- function(x, na.rm = TRUE) {
  if (is.null(x)) {
    x <- NA_real_
    na.rm <- FALSE
  }
  data.table(
    N = length(x), mean = mean(x, na.rm = na.rm),
    sd = sd(x), get_range(x, na.rm))
}

get_range <- function(x, na.rm = TRUE) {
  data.table(
    min = as.numeric(min(x, na.rm = na.rm)),
    max = as.numeric(max(x, na.rm = na.rm)))
}


data_cond_means <- function(cond_grps = c("aq_category", "action_category"),
                            add_by = NULL, omit = NULL) {
  by <- c(cond_grps, add_by)
  dt <- copy_dt() %>% .[, summary_tbl(recog_acc), by = by]

  if (!is.null(add_by)) {
    dt[, eval(c("cond_min", "cond_max")) := get_range(mean), cond_grps]
  }

  if (!is.null(omit)) {
    dt[, eval(omit) := NULL]
  }
  dt[order(get(cond_grps), -mean)]
}

group_stat_subset <- function(x, grps, fun = mean) {
  copy_dt(x) %>%
    .[, .(.y = fun(.y)), by = grps] %>%
    .[, c(grps, ".y"), with = FALSE]
}

effect_refactor <- function(x) {
  factor(x,
    levels = rev(c(
      "smpl_low", "smpl_hig", "cplx_low", "cplx_hig",
      "action_main", "aq_main", "intx", "act_diff.hig",
      "act_diff.low", "aq_diff.smpl", "aq_diff.cplx")),
    labels = rev(c(
      "simple low", "simple high", "complex low",
      "complex high", "complex actions - simple actions",
      "low AQ - high AQ", "action type x AQ",
      "complex actions - simple (high AQ)",
      "complex actions - simple (low AQ)",
      "low AQ - high (simple actions)",
      "low AQ - high (complex actions)")))
}

post_int_sre <- function(x, rope = NULL, warn = FALSE, w = 0.9) {
  int <- as.data.table(
    ggdistribute:::post_int(na.omit(x),
      mid = "median", int = "hdi",
      widths = w, rope = rope, warn = warn)
  )
  rename_new <- c(int$central, "SD.lower", "SD.upper", "CI.lower", "CI.upper")
  rename_old <- c("c", "l.sd", "r.sd", "l.wide", "r.wide")
  setnames(int, rename_old, rename_new)
  int[, central := NULL]
  int[, c(rename_new, names(int)[!names(int) %in% rename_new]), with = FALSE]
}

prob_diff_from_null <- function(x, null = 0.0) {
  p <- sum(x < null) / length(x)
  ifelse(p > 0.5, 1 - p, p)
}

# difference of rotated viewpoints from recorded viewpoint
multivariate_viewpoint <- function(ppd, fun = mean) {
  grps <- c(".sample", "angle")
  group_stat_subset(ppd, grps, fun = fun) %>%
    dcast(... ~ angle, value.var = ".y")
}

multivariate_gender <- function(ppd, fun = mean) {
  grps <- c(".sample", "sub_sex", "aq_category")
  group_stat_subset(ppd, grps, fun = fun) %>%
    dcast(... ~ sub_sex, value.var = ".y")
}

multivariate_aq_action <- function(ppd, fun = mean) {
  grps <- c(".sample", "action_category", "aq_category")
  group_stat_subset(ppd, grps, fun = fun) %>%
    dcast(... ~ action_category + aq_category, value.var = ".y") %>%
    setnames(
      c(
        "complex actions_high symptoms",
        "complex actions_low symptoms",
        "simple actions_high symptoms",
        "simple actions_low symptoms"),
      c("cplx_hig", "cplx_low", "smpl_hig", "smpl_low")) %>%
    .[]
}

posterior_aq_action_effects <- function(ppd) {
  multivariate_aq_action(ppd) %>%
    .[
      ,
      .(
        .sample = 1:.N, smpl_low, smpl_hig, cplx_low, cplx_hig,
        act_diff.hig = cplx_hig - smpl_hig, act_diff.low = cplx_low -
          smpl_low,
        aq_diff.cplx = cplx_low - cplx_hig, aq_diff.smpl = smpl_low -
          smpl_hig,
        action_main = apply_contrast(
          c(-0.5, -0.5, 0.5, 0.5), smpl_low,
          smpl_hig, cplx_low, cplx_hig),
        aq_main = apply_contrast(
          c(0.5, -0.5, 0.5, -0.5), smpl_low,
          smpl_hig, cplx_low, cplx_hig),
        intx = apply_contrast(
          c(-1, -1, 1, 1) * c(-1, 1, -1, 1),
          smpl_low, smpl_hig, cplx_low, cplx_hig))
    ]
}

rsquared_data <- function(DT, yhat = ".y") {
  if (ncol(DT) > 2) {
    stop("DT must only contain yhat and y")
  }
  yhat_var <- var(DT[[yhat]])
  err_var <- var(-1 * Reduce(`-`, DT))
  yhat_var / (yhat_var + err_var)
}

ppd_rsquared <- function(ppd, yhat, y) {
  if (is.null(ppd)) {
    return(data.table())
  }
  ppd[, .(r_sq = rsquared_data(.SD, yhat)), .sample, .SDcols = c(yhat, y)]
}
