# export ------------------------------------------------------------------

#' Load SRE models into global workspace
#'
#' @param refit If `TRUE`, then refit package models and save to `save_dir`. If
#' `FALSE`, then load models from `save_dir`.
#' @param save_dir Root directory of subfolders containing saved models.
#' Defaults to external package data directory.
#' @param debug Runs a small number of iterations for debugging purposes.
#'
#' @return [NULL]
#' @export
#' @examples
#' sre_models()
sre_models <- function(refit=FALSE, save_dir=NULL, debug=FALSE) {
  if (refit) {
    if (is.null(save_dir)) {
      save_dir <- ".junk/temp_fit"
    }
    sre_data <- import_sre()
    fit_full_model(sre_data, save_dir=save_dir, debug=debug)
    fit_alt_model(sre_data, save_dir=save_dir, debug=debug)
    fit_null_model(sre_data, save_dir=save_dir, debug=debug)
    fit_rt_model(sre_data, save_dir=save_dir, debug=debug)
  } else {
    if (is.null(save_dir)) {
      save_dir <- sre_extdata()
    }

    for (sub_dir in c("sre_mod_full", "sre_mod_alt", "sre_mod_null", "sre_mod_rt")) {
      load(file.path(save_dir, sub_dir, "stanreg.RData"), .GlobalEnv)
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

  rule <- paste(rep("=", 75), collapse="")
  cat("\n\n```\n")

  Map(function(stanreg, n) {
    cat(rule, paste0(n, " model:"), rule, sep="\n")
    print(stanreg, digits=3)
    cat("\n")
    print(prior_summary(stanreg, digits=3))
  }, stanregs, mod_names)


  cat("\n```\n\n")
  invisible()
}

#' @export
print_loo_results <- function(..., stat="elpd_loo") {
  stanregs <- list(...)
  panda_print(loo_table(stanregs, stat=stat), "PSIS-LOO-CV model comparisons", digits=4)
  invisible()
}

#' @export
run_rstanarm_model <- function(formula, data, debug=FALSE, seed=20130110,
                               family=binomial("logit")) {
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

  t_prior_int <- rstanarm::student_t(df=4, location=0, scale=10, autoscale=TRUE)

  t_prior_beta <- rstanarm::student_t(df=4, location=0, scale=2.5, autoscale=TRUE)

  # not used if binomial or poisson
  t_prior_sigma <- rstanarm::student_t(df=4, location=0, scale=5, autoscale=TRUE)

  cov_prior <- rstanarm::decov(regularization=2, concentration=2, shape=1, scale=1)

  rstanarm::stan_glmer(
    formula=formula, data=data, iter=iter, warmup=warmup, thin=thin,
    chains=chains, cores=cores, seed=seed, adapt_delta=adapt_delta,
    family=family, prior_intercept=t_prior_int, prior=t_prior_beta,
    prior_aux=t_prior_sigma, prior_covariance=cov_prior)
}

# separate model fitting --------------------------------------------------

fit_x_model <- function(name, save_dir, ...) {
  fit_list <- structure(list(NULL, NULL), .Names=paste0(c("stanreg_", "loo_"), name))

  message("... Fitting model: ", name)
  fit_list[[1]] <- run_rstanarm_model(...)

  message("... Running approximate leave-one-out cross validation (PSIS-LOO)")
  fit_list[[2]] <- rstanarm::loo(fit_list[[1]])

  if (!is.null(save_dir)) {
    save_dir <- file.path(save_dir, paste0("sre_mod_", name))
    save_dir <- make_dir(save_dir)
    rd_file <- file.path(save_dir, "stanreg.RData")

    message("... Saving data to: ", rd_file)
    save_as(list=fit_list, file=rd_file)

    opts <- options(width=500, max.print=1e4)
    on.exit(options(opts))

    capture.output(print(fit_list[[1]]$stanfit, digits=4), print(
      fit_list[[2]],
      digits=4
    ),
    file=file.path(save_dir, "stanfit.txt"))
  }

  message("\nCompleted")

  fit_list
}

fit_full_model <- function(sre_data, save_dir=NULL, debug=FALSE) {
  mod_form <- formula(recog_acc ~ 1 +
    aq_category * action_category + aq_score_standard + sub_sex + sub_age_scaled +
    (1 + action_category | id) + (1 + angle | action))

  invisible(fit_x_model(
    name="full", save_dir=save_dir, formula=mod_form, data=sre_data,
    debug=debug))
}

fit_alt_model <- function(sre_data, save_dir=NULL, debug=FALSE) {
  mod_form <- formula(recog_acc ~
  1 + aq_category + action_category + aq_score_standard +
    sub_sex + sub_age_scaled + (1 | id) + (1 + angle | action))

  invisible(fit_x_model(
    name="alt", save_dir=save_dir, formula=mod_form, data=sre_data,
    debug=debug))
}

fit_null_model <- function(sre_data, save_dir=NULL, debug=FALSE) {
  mod_form <- formula(recog_acc ~
  1 + sub_sex + sub_age_scaled + (1 | id) + (1 + angle | action))

  invisible(fit_x_model(
    name="null", save_dir=save_dir, formula=mod_form, data=sre_data,
    debug=debug))
}

fit_rt_model <- function(sre_data, save_dir=NULL, debug=FALSE) {
  sre_data <- copy_dt(sre_data)
  sre_data <- sre_data[recog_rt > 0]
  sre_data[, recog_rt := unit_scale(log(recog_rt))]
  mod_form <- formula(recog_acc ~ 1 +
    aq_category * action_category + aq_score_standard + sub_sex + sub_age_scaled +
    (1 + action_category | id) + (1 + angle | action))

  invisible(fit_x_model(
    name="rt", save_dir=save_dir, formula=mod_form,
    data=as.data.frame(sre_data), debug=debug, family=gaussian()))
}

# rbaes functions -----------------------------------------------------------------

#' @export
n_coef <- function(stanreg) {
  beta <- fixef(stanreg)
  gamma <- ranef(stanreg)
  gamma <- unlist(lapply(gamma, ncol))
  names(gamma) <- paste0("random:", names(gamma))
  tbl <- as.data.table(t(c(fixed=length(beta), gamma)))
  cat("\nNumber of coefficients for:\n")
  cat("*", gsub("\\s{2, }", " ", Reduce(paste, deparse(stanreg$formula))), "*", sep="")
  cat("\n")
  print_data_sample(tbl, 1)
}




#' Merge posterior predictions with data
#'
#' @param stanreg x
#' @param newdata data to use for prediction
#' @param post_fun x
#' @param var.sample x
#' @param var.rows x
#' @param var.yhat x
#' @param ... options passed to rstanarm::posterior_predict
#'
#' @return [data.table::data.table]
#' @export
#'
#' @examples
#' stanreg <- example_stanreg(500, 1)
#' merge_data_and_posterior(stanreg)
merge_data_and_posterior <- function(stanreg, newdata=NULL,
                                     post_fun=rstanarm::posterior_predict,
                                     var.sample=".sample", var.rows=".obs",
                                     var.yhat=".y", ...) {
  pred <- post_fun(stanreg, newdata=newdata, ...) %>%
    as.data.table() %>%
    .[, .__mcmc_sample := 1:.N] %>%
    melt(
      id.vars=".__mcmc_sample", variable.name=".__data_obs",
      value.name=".__post_pred") %>%
    .[, .__data_obs := as.integer(.__data_obs)] %>%
    .[, .__mcmc_sample := as.integer(.__mcmc_sample)] %>%
    .[]

  if (is.null(newdata)) {
    newdata <- stanreg_dtbl(stanreg, model_frame=FALSE, get_y=TRUE)
  } else {
    if (is.data.table(newdata)) {
      newdata <- copy(newdata)
    } else {
      newdata <- as.data.table(newdata)
    }
  }

  newdata[, .__data_obs := 1:.N]
  pred <- merge(newdata, pred, by=".__data_obs", all.y=TRUE)
  default_vars <- c(".__mcmc_sample", ".__data_obs", ".__post_pred")
  rename_vars <- c(var.sample, var.rows, var.yhat)
  which_exist <- rename_vars %in% names(pred)

  if (any(which_exist)) {
    these_exist <- paste0(rename_vars[which_exist], collapse=", ")
    warning("New names: ", these_exist, " already exist. ", "Keeping default names.")
  }

  if (!all(which_exist)) {
    setnames(pred, default_vars[!which_exist], rename_vars[!which_exist])
  }

  set_col_order(pred, rename_vars)
  pred
}

#' Scale a numeric vector
#'
#' Vectors are scaled to have a mean of 0.5 and standard deviation of 0.5.
#'
#' The function works similar to scale, in that attributes of the original mean and standard
#' deviation are saved in the object called \code{center} and \code{scale}. The adjustment is also
#' provided in the \code{adj} attribute.
#'
#' @param x numeric vector
#' @param y alternative numeric vector that contains the attributes to use to rescale x
#' @param na.rm remove NAs from the vector before finding stats
#'
#' @return scaled vector
#'
#' @examples
#' x <- rnorm(1000, 50, 5)
#' hist(x, br=50)
#'
#' y <- unit_scale(x)
#' hist(y, br=50)
#' mean(y) # <- 0.5
#' sd(y) # <- 0.5
unit_scale <- function(x, y=NULL, na.rm=TRUE) {
  if (is.null(y)) {
    x_mean <- mean(x, na.rm=na.rm)
    s_std <- 2 * sd(x, na.rm=na.rm)
    x_adj <- 0.5
  } else {
    y_attr <- get_unit_attr(y)
    x_mean <- y_attr$m
    s_std <- y_attr$s
    x_adj <- y_attr$adj
  }

  z <- x_adj + ((x - x_mean) / s_std)

  attr(z, "center") <- x_mean
  attr(z, "scale") <- s_std
  attr(z, "adj") <- x_adj
  return(z)
}

#' Reverse unit scaling
#'
#' @param x object to be scaled back to original scale
#' @param y optional object that has center, scale, and adj attributes to use on x. If y is not
#' used, these attributes will be taken from x.
#'
#' @return numeric vector in original scale
#'
#' @examples
#' z <- rnorm(10000, 100, 15)
#' y <- unit_scale(z)
#' rev_unit_scale(y)
rev_unit_scale <- function(x, y=NULL, na.rm=FALSE) {
  if (is.null(y)) {
    stats <- get_unit_attr(x)
  } else {
    stats <- get_unit_attr(y)
  }

  if (na.rm) {
    x <- na.omit(x)
  }

  with(stats, as.numeric((x - adj) * s + m))
}

get_unit_attr <- function(x) {
  adj <- attr(x, "adj")
  m <- attr(x, "center")
  s <- attr(x, "scale")

  if (any(is.null(c(adj, m, s)))) {
    stop("could not find attributes from a unit_scale object")
  }

  list(adj=adj, m=m, s=s)
}

#' Get model data from stanreg
#'
#' @param stanreg object of class stanreg
#' @param model_frame If `TRUE`, then extract from `$glmod$fr`, else use
#' `$data`.
#' @param get_y logical value. Include the output columns?
#'
#' @return data.table
#' @examples
#' stanreg <- example_stanreg()
#' model_data <- stanreg_dtbl(stanreg, TRUE, TRUE)
stanreg_dtbl <- function(stanreg, model_frame=FALSE, get_y=FALSE) {
  if (model_frame) {
    dt <- copy(as.data.table(stanreg$glmod$fr))
  } else {
    dt <- copy(as.data.table(stanreg$data))
  }

  if (!get_y) {
    y_names <- names(stanreg_get_y(stanreg))
    y_names <- y_names[y_names %in% names(dt)]
    dt[, eval(y_names) := NULL]
  }

  dt
}

set_col_order <- function(x, first_names) {
  data.table::setcolorder(x, c(first_names, names(x)[!names(x) %chin% first_names]))
  invisible(x)
}

stanreg_get_y <- function(stanreg) {
  model_data <- as.data.table(stanreg$glmod$fr)
  txt <- paste0("as.data.table(", sub("~", "", deparse(stanreg$formula[1:2])), ")")
  model_data[, (eval(parse(text=txt), envir=environment()))]
}


#' string format p-value cutoffs
#'
#' @param p a p value between 0 and 1
#'
#' @return character matrix
#'
#' @examples
#' pval_format(.055)
#' pval_format(.05)
#' pval_format(.049)
#' pval_format(.01)
#' pval_format(.001)
#' pval_format(.0001)
#'
#' p <- seq(0, 0.06, .01)
#' data.frame(p, pval_format(p))
pval_format <- function(p) {
  row_mat <- function(s, t) {
    matrix(c(t, s), ncol=2)
  }
  ptab <- do.call(rbind, lapply(p, function(i) {
    if (i > 0.05) {
      return(row_mat("", "n.s."))
    }
    if (i < 0.05 & i > 0.01) {
      return(row_mat("*", "p < .05"))
    }
    if (i < 0.01 & i > 0.001) {
      return(row_mat("**", "p < .01"))
    }
    if (i < 0.001 & i >= 0) {
      return(row_mat("***", "p < .001"))
    }
    if (i == 0.05) {
      return(row_mat("*", "p = .05"))
    }
    if (i == 0.01) {
      return(row_mat("**", "p = .01"))
    }
    if (i == 0.001) {
      return(row_mat("***", "p = .001"))
    }
    stop("invalid p value")
  }))
  colnames(ptab) <- c("Pr cutoff", "Pr significance")
  return(ptab)
}


is.atomic_vec <- function(x) {
  is.vector(x) && is.atomic(x)
}

vec_to_mat <- function(x, row_vec=FALSE) {
  if (is.matrix(x)) {
    return(x)
  }

  if (!is.atomic_vec(x)) {
    stop("x is not a vector.")
  }

  n <- length(x)

  if (n < 1) {
    return(as.matrix(get(typeof(x))()))
  }

  if (row_vec) {
    dim(x) <- c(1L, n)
  } else {
    dim(x) <- c(n, 1L)
  }
  x
}

rep_mat <- function(x, n, dim) {
  if (dim == 1) {
    matrix(rep(x, each=n), nrow=n)
  } else {
    matrix(rep(x, each=n), ncol=n)
  }
}

#' Hedges G
#' @param n_vec sample sizes corresponding to the standard deviations
#'
#' @return numeric value
#'
#' @examples
#' pooled_sd(c(1, 5), c(100, 8))
pooled_sd <- function(sd, n) {
  sd_mat <- vec_to_mat(sd, row_vec=TRUE)
  n_mat <- vec_to_mat(n, row_vec=TRUE)

  cols_sd <- ncol(sd_mat)
  cols_n <- ncol(n_mat)

  if (cols_sd != cols_n) {
    if (cols_n == 1) {
      n_mat <- rep_mat(n_mat, cols_sd, 2)
      cols_n <- ncol(n_mat)
    } else {
      stop("SD vec must equal length of N vec")
    }
  }

  if (nrow(n_mat) == 1 & nrow(sd_mat) > 1) {
    n_mat <- rep_mat(n_mat, nrow(sd_mat), 1)
  }

  numer <- rowSums((n_mat - 1) * sd_mat^2)
  denom <- rowSums(n_mat) - cols_n
  sqrt(numer / denom)
}

apply_contrast <- function(contrast_coef, ...) {
  if (dot_dot_len(...) != length(contrast_coef)) {
    stop("Num. coefficients must equal num vectors.")
  }
  if (sum(contrast_coef) != 0) {
    warning("Sum of coefficients not equal to zero.")
  }
  as.vector(cbind(...) %*% contrast_coef)
}

dot_dot_len <- function(...) {
  length(match.call(expand.dots=TRUE)) - 1L
}

#' table of LOO comparisons
#'
#' @param loo_list a list of objects of the type \code{loo}
#' @param stat
#'
#' @return data.table of loo comparisons
#'
#' @examples
#' stanreg1 <- example_stanreg()
#' stanreg2 <- update(stanreg1, . ~ . - 1)
#' stanreg3 <- update(stanreg1, . ~ . + floor:log_uranium)
#' loo_list <- lapply(list(M1 = stanreg1, M2 = stanreg2, M3 = stanreg3), rstanarm::loo)
#' loo_table(loo_list)
#' loo_table(loo_list, stat="elpd_loo")
loo_table <- function(loo_list, stat=c("elpd_loo", "looic", "p_loo")) {
  stat <- match.arg(stat)
  n_fit <- length(loo_list)
  n_compare <- pairwise(n_fit)
  lnames <- names(loo_list)

  if (is.null(lnames)) {
    lnames <- paste0("loo", 1:n_fit)
  } else {
    no_names <- !nzchar(lnames)
    if (any(no_names)) {
      lnames[no_names] <- paste0("loo", which(no_names))
    }
  }

  ldata <- lapply(loo_list, function(i) {
    lp <- i$pointwise[, stat]
    list(lp=lp, sum_lp=sum(lp))
  })

  loo_comp <- list()

  p_alpha <- 0.95

  tbl_names <- c(
    toupper(paste(stat, "L")), toupper(paste(stat, "R")),
    toupper(paste(stat, "Diff.")), "SE DIFF.",
    paste0("CI ", names(loo_ci(1, 1, 2, p_alpha))))

  elpd_order <- ifelse(stat == "elpd_loo", TRUE, FALSE)

  for (p in 1:nrow(n_compare)) {
    p1 <- n_compare[p, 1]
    p2 <- n_compare[p, 2]
    mod_names <- lnames[c(p1, p2)]
    lp_data <- list(ldata[[p1]]$lp, ldata[[p2]]$lp)
    n_obs <- unlist(lapply(lp_data, length))

    has_nas <- any(unlist(lapply(lp_data, is.na)))
    has_diff_n <- n_obs[1] != n_obs[2]

    if (has_nas | has_diff_n) {
      loo_comp[[p]] <- NA_real_
    } else {
      sum_data <- c(ldata[[p1]]$sum_lp, ldata[[p2]]$sum_lp)
      m_order <- order(sum_data, decreasing=elpd_order)
      loo_diff_data <- lp_data[[m_order[2]]] - lp_data[[m_order[1]]]

      loo_diff <- sum(loo_diff_data)
      loo_se <- sqrt(var(loo_diff_data) * length(loo_diff_data))
      loo_cint <- loo_ci(loo_diff, loo_se, length(loo_diff_data), p_alpha)

      sig_ast <- ifelse(all(loo_cint < 0) || all(loo_cint > 0), "* ", "  ")
      tbl <- data.table::data.table(
        C=c(
          better=sum_data[m_order[1]], worse=sum_data[m_order[2]], diff=loo_diff,
          se=loo_se, cil=loo_cint[1], ciu=loo_cint[2])
      )
      names(tbl) <- paste0(mod_names[m_order[1]], sig_ast, mod_names[m_order[2]])

      loo_comp[[p]] <- tbl
    }
  }

  out_table <- cbind(` `=tbl_names, do.call(cbind, loo_comp))
  return(out_table)
}

#' All pairwise combinations
#'
#' @param n set size (integer)
#'
#' @return n pairs by 2 matrix
#'
#' @examples
#' pairwise(3)
pairwise <- function(n) {
  if (n < 2) {
    return(NULL)
  }
  t(utils::combn(n, 2))
}

dec2pct <- function(x, digits=1) {
  sprintf(paste0("%-", digits + 4, ".", digits, "f%%"), x * 100)
}

loo_ci <- function(x, se, N, p=0.95) {
  if (N <= 1) {
    stop("n must be a sample size greater than 1")
  }

  if (p < 0.5 | p >= 1) {
    stop("p must be greater or equal to than 0.5 and less than 1")
  }

  one_mp <- (1 - p) / 2
  one_pp <- (1 - one_mp)
  q_scaled <- qt(one_pp, N - 1) * se
  ci <- c(x - q_scaled, x + q_scaled)
  dec2pct(one_mp)
  names(ci) <- c(dec2pct(one_mp), dec2pct(one_pp))
  return(ci)
}
