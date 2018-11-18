#' @export
turtles_aq_act_contrasts <- function(fx) {
  fx <- fx[!contrast %in% c("simple acc", "complex acc")] %>%
    .[order(contrast, effect, .sample)]

  opts <- plot_setup()

  ggplot(fx) +
    aes(y=effect) + facet_grid(contrast ~ ., scales="free_y", space="free_y") +
    geom_vline(color=opts$gry, size=opts$lwd, linetype=1, xintercept=0) +
    ggdistribute::geom_posterior(
      aes(x=value),
      size=0.15, ci_width=0.90, fill="#D7E0C5",
      brighten=c(3, 0, 1.333), midline=opts$blk, interval_type="ci",
      center_stat="mean", mirror=FALSE, adjust=1.0, n=1024, trim=.005, cut=1
    ) +
    scale_x_continuous(breaks=seq(-1, 1, .05)) +
    labs(x="Difference in accuracy (posterior predictions)") +
    theme(
      panel.border=element_rect(colour=opts$gry, size=0.67),
      axis.title.y=element_blank(),
      strip.text.y=element_text(
        angle=0, margin=margin(t=0, r=0, b=0, l=0, unit="pt"), hjust=0.5
      )
    )
}

#' @export
boxplot_action_acc <- function() {
  dt <- copy_dt() %>%
    .[, .(acc=mean(recog_acc)), .(id, action, aq_category, action_category)] %>%
    .[, mu := mean(acc), action] %>%
    .[order(action_category, -mu), ] %>%
    .[, action := factor(action, levels=as.character(unique(action)))] %>%
    copy_dt(TRUE)

  opts <- plot_setup()

  outliers <- dt[, .(acc=boxplot.stats(acc)$out), .(action, Actions)]

  ggplot(dt) + aes(x=action, y=acc) +
    geom_boxplot(aes(fill=Actions, colour=Actions),
      size=opts$lwd, width=0.5,
      outlier.alpha=0, outlier.size=1, notch=FALSE, show.legend=FALSE) +
    geom_point(
      data=outliers, aes(fill=Actions, colour=Actions), alpha=0.5, size=0.5,
      show.legend=FALSE, position=position_jitter(0.25, 0.02)
    ) +
    scale_fill_manual(values=opts$c_pal[c(1, 5)]) +
    scale_colour_manual(values=opts$c_pal[c(2, 4)]) +
    coord_flip(ylim=c(0, 1)) + facet_grid(Actions ~ ., space="free", scales="free") +
    labs(y="Self-recognition accuracy", x="Action")
}

#' @export
boxplot_aq_action_acc <- function() {
  dt <- copy_dt() %>%
    .[, .(acc=mean(recog_acc)), .(id, aq_category, action_category)] %>%
    .[order(aq_category, action_category, -acc), ] %>%
    copy_dt(TRUE)

  opts <- plot_setup()

  ggplot(dt) + aes(x=AQ, y=acc) +
    geom_boxplot(
      aes(fill=Actions, colour=Actions),
      size=opts$lwd, width=0.333,
      position=position_dodge(width=0.5), outlier.size=0.7, outlier.stroke=0.5,
      outlier.shape=21, notch=FALSE, show.legend=TRUE
    ) +
    geom_point(
      aes(fill=Actions),
      shape=20, alpha=0.5, colour=opts$blk, show.legend=FALSE,
      position=position_jitterdodge(
        jitter.width=0.275, jitter.height=0, dodge.width=0.5
      )
    ) +
    scale_fill_manual(values=opts$c_pal[c(1, 5)]) +
    scale_colour_manual(values=opts$c_pal[c(2, 4)]) +
    scale_y_continuous(breaks=seq(0, 1, .1)) + coord_cartesian(ylim=c(0, 1)) +
    labs(y="Self-recognition accuracy", x="AQ score group") +
    theme(
      legend.position=c(1, 0), legend.justification=c("right", "bottom"),
      legend.box.background=element_blank(), legend.title=element_blank()
    )
}

#' @export
plot_action_post_acc <- function(fx) {
  acc_dta <- fx[, .(.y=mean(y)), .(action, action_category)]
  opts <- plot_setup()

  ggplot(fx) + facet_wrap(~action_category, scales="free") +
    geom_vline(color=opts$gry, size=opts$lwd, linetype=1, xintercept=0.25) +
    ggdistribute::geom_posterior(
      aes(x=.y, y=action),
      size=0.15, ci_width=0.9, interval_type="ci",
      center_stat="mean", mirror=FALSE, adjust=1.1, n=1024, trim=.003, cut=1
    ) +
    geom_point(
      data=acc_dta, aes(x=.y, y=action), color=opts$blk,
      position=position_nudge(y=-0.333), shape=21, size=1, fill="#FFFFFF"
    ) +
    scale_x_continuous(breaks=seq(-1, 1, .1)) +
    coord_cartesian(xlim=c(.25, 1)) + labs(x="Self-recognition accuracy") +
    theme(
      panel.border=element_rect(fill=NA, colour=opts$gry, size=0.67),
      axis.title.y=element_blank(), strip.background=element_blank(),
      panel.spacing.y=unit(12, "pt"),
      strip.text.y=element_text(
        angle=0, margin=margin(t=0, r=0, b=0, l=0, unit="pt"), hjust=0.5
      )
    )
}

#' @export
hist_plot_acc <- function() {
  opts <- plot_setup()

  plt_sum <- copy_dt(NULL) %>%
    .[, .(.y=mean(recog_acc)), .(aq_category, action_category, id, action)] %>%
    .[, count := .N, .(aq_category, action_category)] %>%
    .[, .(n_freq=.N / count), .(aq_category, action_category, .y, count)] %>%
    .[, .__aq := sub(" symptoms", "", aq_category)] %>%
    .[, .__act := sub(" actions", "", action_category)] %>%
    .[, boxes := paste("AQ score: ", .__aq, "\nActions: ", .__act, sep="")] %>%
    .[, boxes_txt := c(boxes[1], rep("", .N - 1)), .(.__aq, .__act)] %>%
    .[, v := c(sum(.y * n_freq), rep(-1, .N - 1)), boxes]

  acc_range <- plt_sum[v >= 0, range(v)]

  ggplot(plt_sum) + aes(x=.y, y=n_freq) +
    geom_rect(
      aes(xmin=acc_range[1], xmax=acc_range[2], ymin=0, ymax=1),
      fill=grey(0.96)
    ) +
    geom_vline(aes(xintercept=v), linetype=2, size=opts$lwd, color=opts$blk) +
    geom_bar(aes(color=aq_category, fill=aq_category),
      width=0.1, size=opts$lwd,
      stat="identity") +
    geom_text(aes(x=0, y=0.5, label=boxes_txt),
      hjust=0, vjust=1,
      family=getOption("SRE.font_family")) +
    facet_wrap("boxes", ncol=2, scales="free_x") +
    scale_y_continuous(breaks=seq(0, 1, .1), labels=function(x) {
      sprintf("%-3.0f%%", x * 100)
    }) +
    scale_x_continuous(breaks=seq(0, 1, length.out=5)) +
    scale_fill_manual(values=c(opts$c_pal[5], opts$c_pal[4])) +
    scale_color_manual(values=c("#000000", opts$blk)) +
    coord_cartesian(
      xlim=c(-0.1, 1.1), ylim=c(0, max(plt_sum$n_freq) + .05), expand=FALSE
    ) +
    labs(x="Self-recognition accuracy", y="Percentage of total responses") +
    guides(fill=guide_legend(title="AQ group")) +
    theme(
      legend.position="none", strip.background=element_blank(),
      strip.text=element_blank(), panel.spacing.x=unit(12, "pt"),
      panel.spacing.y=unit(12, "pt")
    )
}

#' @export
interaction_plot_bar <- function() {
  SRE <- copy_dt(NULL)
  nsub <- length(unique(SRE$id))

  plt_summary_dat <- SRE %>%
    .[, ran_mean := mean(recog_acc), id] %>%
    .[, fix_mean := mean(recog_acc), aq_category] %>%
    .[
      , subj_err := mean(recog_acc) - ran_mean + fix_mean,
      .(id, action_category, angle)
    ] %>%
    .[, sem := sd(subj_err) / sqrt(nsub), .(aq_category)] %>%
    .[, .(y=mean(recog_acc), sem=mean(sem)),
      by=list(aq_category, action_category)
    ] %>%
    .[, `:=`(
      aq_category=factor(aq_category,
        levels=c(
          "low symptoms", "high symptoms"
        ),
        labels=c("low AQ", "high AQ")),
      action_category=factor(
        action_category,
        levels=c("complex actions", "simple actions"),
        labels=c("complex", "simple")
      )
    )] %>%
    .[]

  opts <- plot_setup()

  ggplot(plt_summary_dat) + aes(action_category, y) +
    geom_hline(aes(yintercept=.25), color=opts$gry, size=0.333, linetype=1) +
    geom_bar(
      stat="identity", position=position_dodge(width=opts$dodge_w), size=1,
      color=NA, width=0.67, aes(fill=aq_category)
    ) +
    geom_linerange(
      stat="identity", position=position_dodge(width=opts$dodge_w), size=0.8,
      color="white", aes(group=aq_category, ymin=y - sem, ymax=y)
    ) +
    geom_linerange(
      stat="identity", position=position_dodge(width=opts$dodge_w), size=0.8,
      color=opts$blk, aes(group=aq_category, ymin=y, ymax=y + sem)
    ) +
    scale_fill_manual(values=with(opts, c(blk, gry))) +
    scale_y_continuous(breaks=seq(0, 1, .1)) +
    labs(y="Self-recognition accuracy", x="Action type") +
    coord_cartesian(ylim=c(0, 1)) +
    theme(
      panel.border=element_blank(), legend.position=c(1, 1),
      legend.justification=c("right", "top"), legend.direction="vertical",
      legend.title=element_blank(), legend.text=element_text(size=rel(0.7)),
      legend.background=element_blank(), legend.box.background=element_blank(),
      legend.key.width=unit(16, "pt"), legend.key.height=unit(16, "pt"),
      legend.text.align=1
    )
}

#' @export
interaction_scatter <- function() {
  sre_plot_dta1 <- copy_dt() %>%
    .[, .(p=mean(recog_acc)), .(id, aq_category, aq_score, action_category, sub_sex)]
  sre_plot_dta1 <- copy_dt(sre_plot_dta1, TRUE)
  sre_sub1_means <- sre_plot_dta1[
    , .(p=mean(p), aql=min(aq_score), aqu=max(aq_score)), .(Actions, AQ)
  ]


  opts <- plot_setup()

  ggplot(sre_plot_dta1) + aes(x=aq_score, y=p, group=interaction(Actions, AQ)) +
    geom_vline(xintercept=0, size=0.25) + geom_hline(yintercept=0.25, size=0.25) +
    geom_segment(
      data=sre_sub1_means, aes(
        x=aql, y=p, yend=p, xend=aqu,
        color=Actions
      ), linetype=2, size=0.3,
      show.legend=FALSE
    ) +
    stat_smooth(
      aes(color=Actions),
      se=FALSE, method="lm", fill=rgb(0.94, 0.94, 0.94, 0.5)
    ) +
    geom_point(aes(color=Actions, shape=Sex), alpha=0.7) +
    scale_color_manual(values=c(opts$c_pal[2], opts$c_pal[4])) +
    scale_shape_manual(values=c(3, 6)) + scale_x_continuous(breaks=seq(0, 50, 2)) +
    scale_y_continuous(breaks=seq(0, 1, 0.125)) + labs(x="AQ score", y="Accuracy") +
    guides(
      color=guide_legend(title="Actions:", override.aes=list(shape=NA)),
      shape=guide_legend(title="Sex:", override.aes=list(color="black", alpha=1))
    ) +
    theme(
      panel.border=element_blank(), legend.position=c(0, 0.03),
      legend.direction="horizontal", legend.justification=c("left", "bottom"),
      legend.title=element_text(size=rel(0.5), hjust=0),
      legend.text=element_text(size=rel(0.5)), legend.spacing=grid::unit(0, "pt"),
      legend.background=element_blank(), legend.box.background=element_blank()
    )
}

# model checks ------------------------------------------------------------


#' @export
mcmc_checks <- function(stanreg) {
  opts <- plot_setup(8)

  plot((plot(stanreg, "trace", pars="(Intercept)") %>%
    gg_overrides() %>%
    change_geom_aes(1, alpha=.5)) + labs(title="Target distribution coverage"))

  plot((plot(stanreg, "rhat_hist", binwidth=5e-5) %>%
    gg_overrides() %>%
    change_geom_aes(1, colour="white", size=opts$lwd)) + guides(colour=FALSE) +
    labs(title="Chain consistency (Rhat)") + theme(axis.title.x=element_blank()))

  plot((plot(stanreg, "neff") %>%
    gg_overrides() %>%
    change_geom_aes(1, alpha=.5)) +
    labs(title="Effective sample size ratio"))
}


#' @export
compare_rsquared <- function(ppd.full, ppd.alt=NULL, ppd.null=NULL) {
  r2_list <- lapply(
    list(`M1 (full)`=ppd.full, `M2 (alt)`=ppd.alt, `M3 (null)`=ppd.null),
    ppd_rsquared,
    yhat=".y", y="recog_acc"
  )

  r2_data <- rbindlist(r2_list, idcol="model")
  opts <- plot_setup()
  ggplot(r2_data) +
    aes(x=r_sq, y=model) + ggdistribute::geom_posterior(ci_width=0.9, mirror=FALSE) +
    labs(x="R-squared value", y="Model")
}

#' @export
model_vs_data <- function(ppd_linpred, mname="", ymax=NULL, n=500, seed=NULL) {
  y_obs <- ppd_linpred[, .(y=mean(recog_acc)), .(id, aq_category)]

  y_rep <- ppd_linpred %>%
    .[, .(.y=mean(.y)), .(.sample, id)] %>%
    dcast(.sample ~ id, value.var=".y") %>%
    .[, .sample := NULL] %>%
    as.matrix()

  y_rep <- y_rep[, chmatch(as.character(y_obs$id), colnames(y_rep))]
  opts <- plot_setup(8)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  s <- nrow(y_rep)
  n <- min(n, s)
  r <- sample(s, n)
  p <- bayesplot::ppc_dens_overlay(
    y_obs$y, y_rep[r, ],
    n_dens=512, adjust=0.8, alpha=0.15
  )

  if (is.null(ymax)) {
    ymax <- max(density(y_obs$y, n=256, adjust=0.8)$y) * 1.5
  }

  (p %>%
    gg_overrides() %>%
    change_geom_aes(1, colour=opts$c_pal[4])) +
    coord_cartesian(ylim=c(0, ymax)) +
    theme(legend.position=c(0, 1), legend.justification=c("left", "top")) +
    scale_x_continuous(breaks=seq(0, 1, .1)) +
    labs(title=mname) + guides(colour=guide_legend(override.aes=list(alpha=1)))
}

# helpers -----------------------------------------------------------------


plot_setup <- function(font_size=NULL) {
  if (getOption("SRE.mejr_theme")) {
    theme_set(ggdistribute::theme_mejr(
      base_size=font_size %||% 10, madj=1,
      font_family=getOption("SRE.font_family")
    ))
  } else {
    theme_set(theme_classic(base_size=font_size %||% 12, base_family="serif"))
  }

  theme_update(
    plot.margin=margin(t=2, r=4, b=2, l=2, unit="pt"),
    panel.border=element_blank(), axis.title.y=element_text(hjust=0.5),
    legend.box.background=element_blank(), legend.background=element_blank()
  )

  list(
    blk=gray(0.4), gry=gray(0.84), c_pal=c(
      "#EDFADC", "#9EAD87", "#FDFFFA", "#A376AD",
      "#F4DCFA"
    ), dodge_w=0.75, lwd=1 / 3
  )
}

gg_overrides <- function(gg, n_colors=5) {
  suppressMessages(gg + scale_color_manual(values=viridisLite::viridis(n_colors)) +
    scale_fill_manual(values=viridisLite::viridis(n_colors)) +
    guides(alpha=FALSE, fill=FALSE) +
    theme(
      legend.position="bottom", plot.title=element_text(
        hjust=0.5, size=rel(0.9)
      ),
      legend.title=element_blank()
    ))
}

change_geom_aes <- function(p, layer, ...) {
  keyvals <- list(...)
  keys <- names(keyvals)
  if (is.null(keys)) {
    return(p)
  }
  keyvals <- keyvals[keys[nzchar(keys)]]
  p$layers[[layer]]$aes_params[names(keyvals)] <- keyvals
  p
}
