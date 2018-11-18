knitr::opts_chunk$set(
  collapse = TRUE,
  eval = TRUE,
  echo = FALSE,
  comment = "#>",
  strip.white = FALSE,
  fig.pos = 'H',
  tidy = FALSE,
  fig.align = "center",
  warning = FALSE
)
## library(SRE)
sre_print <- copy_dt(SRE)
sre_print[, id := as.integer(id)]
sre_print <- sre_print[order(exp_date)]
print_data_sample(sre_print, n=3, i = seq(1, nrow(sre_print), 73)[1:5], digits = 2)
demographics_summary("sub_age", "aq_category",
                     caption="Participant age by AQ groups.", digits=3)
demographics_summary("days_elapsed",
                     caption = "Delay between recording and test (days).")
demographics_summary("aq_score", "aq_category",
                     caption = "AQ score by AQ groups.", digits = 3)
demographics_summary("aq_score", "sub_sex",
                     caption = "AQ score by sex.", digits = 3)
demographics_summary("aq_score", c("sub_sex", "aq_category"),
                     caption = "AQ score by AQ groups and sex.", digits = 3)
acc_summary_subject(caption="Accuracy by subject.", digits=2)
acc_summary_actions(caption="Accuracy by action.", digits=2)
acc_summary_sex(caption="Accuracy by sex.", digits=2)
panda_print(
  dbinom(structure(0:4, .Names=0:4), 4, .25, log=TRUE),
  caption=paste("Log-likelihoods of getting none to all",
                "four responses correct for a specific action,",
                "given that there are four options to choose for each trial."))
sre_models(refit = FALSE, save_dir = NULL)
seed <- 19850519
mcmc_checks(stanreg_full)
mcmc_checks(stanreg_alt)
mcmc_checks(stanreg_null)
## sre_models(refit = FALSE, save_dir = NULL)
## seed <- 19850519
ppd.full <- merge_data_and_posterior(stanreg_full,
                                     post_fun = rstanarm::posterior_linpred,
                                     transform = TRUE,
                                     seed = seed)

ppd.full.binary <- merge_data_and_posterior(stanreg_full, seed = seed)
posterior_action_acc_subj(
  ppd.full,
  caption = "Each participants' posteriror median accuracy, separated by AQ group intervals.")
posterior_acc_aq_intervals(ppd.full,
                           caption = "AQ group, posterior intervals.")
posterior_gender_diff(ppd.full,
                      caption = "Male-Female acc. diff., posterior intervals.")
aq_act_contrasts <- posterior_aq_action_contrasts(ppd.full)
posterior_aq_action_intervals(aq_act_contrasts,
                              digits = 2,
                              caption = "Posterior contrast intervals.")
contrast_effect_sizes(ppd.full.binary,
                      digits = 2,
                      caption = "Posterior contrast effect size intervals.")
sre_rt <- copy_dt(SRE)
no_resp <- sre_rt[, recog_rt <= 0]
rt_remove <- sre_rt[(no_resp), 
                    .(nSub=length(unique(id)), total=.N), 
                    .(sub_sex, aq_category)]

panda_print(rt_remove, caption="Trials removed for no response after 40 seconds.")
ppd.rt <- merge_data_and_posterior(stanreg_rt, seed = seed)
rt_aq_act_contrasts <- posterior_aq_action_contrasts(ppd.rt)
rt_aq_act_contrasts_results <- posterior_aq_action_intervals(rt_aq_act_contrasts, 
                                                             print=FALSE)
panda_print(rt_aq_act_contrasts_results,
  digits = 2,
  caption = "Posterior contrast intervals for response times.")
rm(ppd.rt, rt_aq_act_contrasts)
recog_rt_scaled <- sre_rt[(!no_resp), SRE:::unit_scale(log(recog_rt))]

panda_print(rt_aq_act_contrasts_results[
                , .(RT_effect=exp(SRE:::rev_unit_scale(median, recog_rt_scaled))),
                .(contrast, effect, ` `)], 
            digits=2, 
            caption="RT contrast effects in original scale (seconds)")
posterior_viewpoint_diff(ppd.full,
                         caption = "Viewpoint, posterior intervals.",
                         digits=3)
n_coef(stanreg_full)
print_stanreg_results(`M1 (Full)` = stanreg_full)
print(loo_full, digits=3)
n_coef(stanreg_alt)
print_stanreg_results(`M2 (Alternative)` = stanreg_alt)
print(loo_alt, digits=3)
n_coef(stanreg_null)
print_stanreg_results(`M3 (Null)` = stanreg_null)
print(loo_null, digits=3)
print_loo_results(`Ful(M1)` = loo_full,
                  `Alt(M2)` = loo_alt,
                  `Nul(M3)` = loo_null)
ppd.alt <- merge_data_and_posterior(stanreg_alt,
                                     post_fun = rstanarm::posterior_linpred,
                                     transform = TRUE,
                                     seed = seed)

ppd.null <- merge_data_and_posterior(stanreg_null,
                                     post_fun = rstanarm::posterior_linpred,
                                     transform = TRUE,
                                     seed = seed)
compare_rsquared(ppd.full, ppd.alt, ppd.null)
model_vs_data(ppd.full, "M1 (Full)", 4.75, 500, seed = seed)
model_vs_data(ppd.alt, "M2 (Alt)", 4.75, 500, seed = seed)
model_vs_data(ppd.null, "M3 (Null)", 4.75, 500, seed = seed)
if (getOption("SRE.mejr_pkg")) {
  old_opt <- options(SRE.mejr_theme = TRUE)
  knit_path_adj <- ifelse("NAMESPACE" %in% list.files(), ".", "..")
  fig_dir <- file.path(knit_path_adj, "inst", "extdata", "figures")
  fig_dir <- SRE:::make_dir(fig_dir)

  mejr::save_plot(
    turtles_aq_act_contrasts(aq_act_contrasts),
    file = file.path(fig_dir, "cint_fig"),
    format = "all",
    width = 5,
    height = 3,
    res=600,
    font = getOption("SRE.font_family")
  )

  mejr::save_plot(
    boxplot_action_acc(),
    file = file.path(fig_dir, "action_fig"),
    format = "all",
    width = 3.75,
    height = 3.25,
    res=600,
    font = getOption("SRE.font_family")
  )

  mejr::save_plot(
    boxplot_aq_action_acc(),
    file = file.path(fig_dir, "aq_act_data_fig"),
    format = "all",
    width = 2.333,
    height = 2.8,
    res=600,
    font = getOption("SRE.font_family")
  )

  options(old_opt)
}
interaction_plot_bar()
hist_plot_acc()
plot_action_post_acc(posterior_action_acc_means(ppd.full.binary))
interaction_scatter()
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
## library(SRE)
## SRE
## SRE <- import_sre(raw=TRUE)
## sre_models(refit = TRUE, save_dir = NULL)
## model_refit_dir <- "./sre_models_example"
## sre_models(refit = TRUE, save_dir = model_refit_dir, debug = FALSE)
## sre_models(refit = FALSE, save_dir = NULL)
