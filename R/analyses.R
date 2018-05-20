#' @export
demographics_summary <- function(value, grps = "aq_category",
                                 print = TRUE, ...) {
  demo_data <- copy_dt() %>%
    .[
      , .(trials = .N, months = days_elapsed / 30.42),
      .(id, sub_sex, aq_category, sub_age, aq_score, days_elapsed)
    ]

  grp_stats <- demo_data[, summary_tbl(get(value)), by = grps]
  all_stats <- demo_data[, summary_tbl(get(value))]

  dt <- data.table::rbindlist(list(grp_stats, all_stats), fill = TRUE)
  dt <- dt[order(get(grps), -mean)]

  if (print) {
    panda_print(copy_dt(dt, TRUE), ...)
  }

  invisible(dt)
}

#' @export
time_delay_summary <- function(print = TRUE, ...) {
  time_data <- copy_dt() %>%
    .[, .N, .(id, days_elapsed, aq_category, action_category)] %>%
    .[, months := days_elapsed / 30.42]

  dt1 <- dt[, .(e = mean(days_elapsed)), .(aq_category, id)] %>%
    .[
      , .(
        month_avg = mean(e) / 30.42, mean_days = mean(e),
        sd_days = sd(e), range = paste(range(e), collapse = "-")),
      .(aq_category)
    ]

  dt2 <- dt[, .(e = mean(days_elapsed)), .(id)] %>%
    .[, .(
      aq_category = "all", month_avg = mean(e) / 30.42,
      mean_days = mean(e), sd_days = sd(e),
      range = paste(range(e), collapse = "-"))]

  dt <- rbind(dt1, dt2)
  if (print) {
    panda_print(dt, ...)
  }

  invisible(dt)
}


#' @export
acc_summary_actions <- function(print = TRUE, ...) {
  dt <- data_cond_means(c("action_category", "action"),
    omit = c("min", "max"))

  if (print) {
    x <- copy_dt(dt, TRUE)
    setnames(x, c("Actions", "action"), c(" ", "Actions"))
    panda_print(x, ...)
  }

  invisible(dt)
}


#' @export
acc_summary_subject <- function(print = TRUE, ...) {
  dt <- data_cond_means(c("aq_category"), "id", omit = c("min", "max"))

  if (print) {
    x <- copy_dt(dt, TRUE)
    setnames(x, c("cond_min", "cond_max"), c("min", "max"))
    panda_print(x[AQ == "High"], ...)
    panda_print(x[AQ == "Low"], ...)
  }

  invisible(dt)
}

#' @export
acc_summary_aq <- function(print = TRUE, ...) {
  dt <- data_cond_means(c("aq_category"), omit = c("min", "max"))

  if (print) {
    panda_print(copy_dt(dt, TRUE), ...)
  }

  invisible(dt)
}

#' @export
acc_summary_sex <- function(print = TRUE, ...) {
  dt_g <- data_cond_means(c("sub_sex"), "aq_category", omit = c("min", "max"))
  dt <- data_cond_means(c("sub_sex"), omit = c("min", "max"))
  dt <- rbindlist(list(dt_g, dt), fill = TRUE)

  if (print) {
    x <- copy_dt(dt, TRUE)
    setnames(x, c("cond_min", "cond_max"), c("min", "max"))
    panda_print(x, missing = "", ...)
  }

  invisible(dt)
}

#' @export
posterior_viewpoint_diff <- function(ppd, print = TRUE, ...) {
  test_angles <- c("0", "135", "247")

  # difference of rotated viewpoints from recorded viewpoint
  rot_acc_diff <- multivariate_viewpoint(ppd) %>%
    .[, lapply(.SD, `-`, `180`), .SDcols = test_angles] %>%
    melt(
      measure.vars = test_angles, value.name = ".y",
      variable.name = "Rotation")

  # posterior intervals and p vals
  results <- rot_acc_diff[, post_int_sre(.y), Rotation] %>%
    merge(rot_acc_diff[
      , .(`$p(D = 0)$` = prob_diff_from_null(.y)),
      Rotation
    ], by = "Rotation") %>%
    cap_mid(suffix = " Diff.")

  if (print) {
    panda_print(results, ...)
  }

  invisible(results)
}

#' @export
posterior_gender_diff <- function(ppd, print = TRUE, ...) {
  gender_acc <- multivariate_gender(ppd)

  results <- gender_acc[, post_int_sre(m - f), .(aq_category)]

  if (print) {
    panda_print(results, ...)
  }

  invisible(results)
}

#' @export
posterior_action_acc_subj <- function(ppd, print = TRUE, ...) {
  acc_means <- ppd[, .(.y = mean(.y)), .(.sample, aq_category, id)] %>%
  .[ , post_int_sre(.y), .(aq_category, id)] %>%
    .[order(aq_category, -median)] %>%
    .[, id := NULL] %>% .[]

  if (print) {
    x <- copy_dt(acc_means, TRUE)
    panda_print(x[AQ == "High"], ...)
    panda_print(x[AQ == "Low"], ...)
  }

  invisible(acc_means)
}

#' @export
posterior_action_acc_means <- function(ppd) {
  acc_means <- ppd[
    , .(.y = mean(.y), y = mean(recog_acc)),
    .(.sample, action, action_category)
  ]

  ordered_names <- acc_means[, mean(y), action] %>% .[order(V1), action]

  acc_means[, action := factor(action, levels = ordered_names)]

  invisible(acc_means)
}

#' @export
posterior_acc_aq_intervals <- function(ppd, print = TRUE, ...) {
  # mean acc per sample
  acc_means <- ppd[, .(m = mean(.y)), .(.sample, id, aq_category)] %>%
    .[, .(m = mean(m)), .(.sample, aq_category)]

  acc_summary <- acc_means[, post_int_sre(m), aq_category] %>%
    rbind(acc_means[, cbind(aq_category = "all", post_int_sre(m))])

  if (print) {
    panda_print(acc_summary, ...)
  }

  invisible(acc_summary)
}


#' Univariate form with renamed variables
#' @export
posterior_aq_action_contrasts <- function(ppd) {
  posterior_aq_action_effects(ppd) %>%
    melt(id.vars = ".sample", variable.name = "effect") %>%
    .[, contrast := NA_character_] %>%
    .[
      effect %in% c("smpl_low", "smpl_hig"),
      contrast := "simple acc"
    ] %>%
    .[
      effect %in% c("cplx_low", "cplx_hig"),
      contrast := "complex acc"
    ] %>%
    .[
      effect %in% c("action_main", "aq_main"),
      contrast := "Simple\ncontrasts"
    ] %>%
    .[effect == "intx", contrast := "Interaction\ncontrast"] %>%
    .[
      effect %in%
        c("act_diff.hig", "act_diff.low", "aq_diff.smpl", "aq_diff.cplx"),
      contrast := "Simple\neffects"
    ] %>%
    .[, effect := effect_refactor(effect)] %>%
    .[]
}

#' @export
posterior_aq_action_intervals <- function(fx, print = TRUE, ...) {
  results <- fx[, post_int_sre(value), .(contrast, effect)] %>%
    merge(fx[
      , .(`Bayes *p*` = prob_diff_from_null(value)),
      .(contrast, effect)
    ] %>%
      .[, c("", " ") := as.data.table(
        rbaes::pval_format(`Bayes *p*`)
      )],
    by = c("contrast", "effect"))

  if (print) {
    panda_print(results, ...)
  }

  invisible(results)
}

#' @export
contrast_effect_sizes <- function(ppd, print = TRUE, ...) {
  fx <- posterior_aq_action_effects(ppd)
  fx_sd <- multivariate_aq_action(ppd, fun = sd)
  fx_n <- multivariate_aq_action(ppd, fun = length)

  col_names <- c("smpl_low", "smpl_hig", "cplx_low", "cplx_hig")
  sd_g <- pooled_sd(
    as.matrix(fx_sd[, col_names, with = FALSE]),
    as.matrix(fx_n[, col_names, with = FALSE]))

  fx_size_cols <- c(
    "act_diff.hig", "act_diff.low", "aq_diff.cplx",
    "aq_diff.smpl", "action_main", "aq_main", "intx")

  fx_size_tbl <- fx[, lapply(.SD, `/`, sd_g), .SDcols = fx_size_cols] %>%
    melt(
      measure.vars = fx_size_cols, value.name = "d",
      variable.name = "Effect size") %>%
    .[, post_int_sre(d), `Effect size`] %>%
    .[, `Effect size` := effect_refactor(`Effect size`)] %>%
    .[]

  if (print) {
    panda_print(fx_size_tbl, ...)
  }

  invisible(fx_size_tbl)
}
