# export ------------------------------------------------------------------

#' Load SRE models into global workspace
#'
#' @param refit If `TRUE`, then refit package models and save to `root_dir`. If
#'   `FALSE`, then load models from `root_dir`.
#' @param root_dir Root directory of subfolders containing saved models.
#'   Defaults to external package data directory.
#' @param debug Runs a small number of iterations for debugging purposes.
#'
#' @return [NULL]
#' @export
#' @examples
#' sre_models()
sre_models <- function(refit = FALSE, root_dir = NULL, debug = FALSE) {
  if (refit) {
    if (is.null(root_dir)) {
      root_dir <- "~/sre_fitted_models"
    }
    sre <- import_sre()
    fit_full_model(sre, root_dir, debug)
    fit_alt_model(sre, root_dir, debug)
    fit_null_model(sre, root_dir, debug)
  } else {
    if (is.null(root_dir)) {
      root_dir <- sre_extdata()
    }

    for (sub_dir in c("sre_mod_full", "sre_mod_alt", "sre_mod_null")) {
      load(file.path(root_dir, sub_dir, "stanreg.RData"), .GlobalEnv)
    }
  }
  invisible()
}

#' @export
print_stanreg_results <- function(...) {
  stanregs <- list(...)
  mod_names <- names(stanregs)
  if (is.null(mod_names)) {
    mod_names <- paste("stanreg", seq_along(stanregs))
  }

  rule <- paste(rep("=", 75), collapse = "")
  cat("\n\n```\n")

  Map(
    function(stanreg, n) {
      cat(rule, paste0(n, " model:"), rule, sep = "\n")
      print(stanreg, digits = 3)
      cat("\n")
      print(prior_summary(stanreg, digits = 3))
    },
    stanregs,
    mod_names)


  cat("\n```\n\n")
  invisible()
}

#' @export
print_loo_results <- function(..., stat = "elpd_loo") {
  stanregs <- list(...)
  panda_print(loo_table(stanregs, stat = stat),
    "PSIS-LOO-CV model comparisons",
    digits = 4)
  invisible()
}

#' @export
run_rstanarm_binom <- function(formula, data, debug = FALSE, seed = 20130110) {
  if (debug) {
    adapt_delta <- 0.8
    cores <- 1
    chains <- 1
    warmup <- 50
    thin <- 1
    n_final_samples <- 100
  } else {
    adapt_delta <- 0.975
    cores <- parallel::detectCores()
    chains <- cores
    warmup <- 50000
    thin <- 1
    n_final_samples <- 10000
  }

  iter <- ceiling(n_final_samples * thin / chains + warmup)

  t_prior_int <- rstanarm::student_t(
    df = 4, location = 0, scale = 10, autoscale = TRUE
  )

  t_prior_beta <- rstanarm::student_t(
    df = 4, location = 0, scale = 2.5, autoscale = TRUE
  )

  # not used if binomial or poisson
  t_prior_sigma <- rstanarm::student_t(
    df = 4, location = 0, scale = 5, autoscale = TRUE
  )

  cov_prior <- rstanarm::decov(
    regularization = 2, concentration = 2,
    shape = 1, scale = 1)

  rstanarm::stan_glmer(
    formula = formula, data = data, iter = iter, warmup = warmup,
    thin = thin, chains = chains, cores = cores, seed = seed,
    adapt_delta = adapt_delta, family = binomial("logit"),
    prior_intercept = t_prior_int, prior = t_prior_beta,
    prior_aux = t_prior_sigma, prior_covariance = cov_prior
  )
}

# separate model fitting --------------------------------------------------

fit_x_model <- function(name, save_dir, ...) {
  fit_list <- structure(list(NULL, NULL),
    .Names = paste0(c("stanreg_", "loo_"), name))

  message("... Fitting model: ", name)
  fit_list[[1]] <- run_rstanarm_binom(...)

  message("... Running approximate leave-one-out cross validation (PSIS-LOO)")
  fit_list[[2]] <- rstanarm::loo(fit_list[[1]])

  if (!is.null(save_dir)) {
    save_dir <- file.path(save_dir, paste0("sre_mod_", name))
    save_dir <- make_dir(save_dir)
    rd_file <- file.path(save_dir, "stanreg.RData")

    message("... Saving data to: ", rd_file)
    save_as(list = fit_list, file = rd_file)

    opts <- options(width = 500, max.print = 1e4)
    on.exit(options(opts))

    capture.output(print(fit_list[[1]]$stanfit, digits = 4),
      print(fit_list[[2]], digits = 4),
      file = file.path(save_dir, "stanfit.txt"))
  }

  message("\nCompleted")

  fit_list
}

fit_full_model <- function(sre_data, save_dir = NULL, debug = FALSE) {
  mod_form <- formula(recog_acc ~
  1 + aq_category * action_category +
    aq_score_standard + sub_sex + sub_age_scaled +
    (1 + action_category | id) + (1 + angle | action))

  invisible(fit_x_model(
    name = "full", save_dir = save_dir,
    formula = mod_form, data = sre_data, debug = debug))
}

fit_alt_model <- function(sre_data, save_dir = NULL, debug = FALSE) {
  mod_form <- formula(recog_acc ~
  1 + aq_category + action_category +
    aq_score_standard + sub_sex + sub_age_scaled +
    (1 | id) + (1 + angle | action))

  invisible(fit_x_model(
    name = "alt", save_dir = save_dir,
    formula = mod_form, data = sre_data, debug = debug))
}

fit_null_model <- function(sre_data, save_dir = NULL, debug = FALSE) {
  mod_form <- formula(recog_acc ~ 1 + sub_sex + sub_age_scaled +
    (1 | id) + (1 + angle | action))

  invisible(fit_x_model(
    name = "null", save_dir = save_dir,
    formula = mod_form, data = sre_data, debug = debug))
}

# helpers -----------------------------------------------------------------

n_coef <- function(stanreg) {
  beta <- fixef(stanreg)
  gamma <- ranef(stanreg)
  gamma <- unlist(lapply(gamma, ncol))
  names(gamma) <- paste0("random:", names(gamma))
  tbl <- as.data.table(t(c(fixed = length(beta), gamma)))
  cat("\nNumber of coefficients for:\n")
  cat("*", gsub("\\s{2, }", " ", Reduce(paste, deparse(stanreg$formula))), "*", sep = "")
  cat("\n")
  print_data_sample(tbl, 1)
}
