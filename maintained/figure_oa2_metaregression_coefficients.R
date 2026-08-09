# hewitt_etal_2024/maintained/figure_oa2_metaregression_coefficients.R
# Output: output/figure_oa2_favorability_primary.pdf, .png and five siblings,
#   output/figure_oa2_metaregression_coefficients.csv
# Depends on: helpers.R, metaregressions.R output
# Description: Figures OA2 through OA7, the coefficient plots behind Figure 3's
#   t-statistics. Six panels, one per hypothesis group and outcome, each showing
#   the coefficient in each context, the precision-weighted pooled coefficient,
#   and the three differences between contexts. Every value is printed on the
#   face of the plot, so the CSV beside them carries all six figures at once.
#
#   One script for six figures because they are one object: the same data frame
#   drawn six times, and splitting it would duplicate the whole construction.

source(here::here("maintained", "helpers.R"))

estimates <- read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE)
contrasts <- read_csv(out("metaregression_contrasts.csv"), show_col_types = FALSE)

estimand_levels <- c("2018", "2020 Downballot", "2020 Presidential", "Combined",
                     "2020D-2018", "2020P-2018", "2020P-2020D")

# The overall mean effect sits at the top of every panel, labelled ATE.
overall <-
  estimates |>
  filter(metareg_group == "overall", specification == "no_moderators",
         weighting == "unweighted", term == "intercept",
         outcome %in% c("favorability", "votechoice")) |>
  transmute(metareg_group = "overall", metareg_name = "Overall", term,
            term_name = "ATE", outcome, outcome_name, estimand = dataset,
            estimate, std.error, conf.low, conf.high)

overall_contrasts <-
  contrasts |>
  filter(metareg_group == "overall", specification == "no_moderators",
         term == "intercept", outcome %in% c("favorability", "votechoice")) |>
  transmute(metareg_group, metareg_name, term, term_name = "ATE", outcome,
            outcome_name, estimand = contrast, estimate, std.error,
            conf.low = NA_real_, conf.high = NA_real_)

hypotheses <-
  estimates |>
  filter(metareg_group %in% c("primary", "secondary", "new"),
         specification == "standard", weighting == "unweighted",
         outcome %in% c("favorability", "votechoice"), tested, !variance_term) |>
  transmute(metareg_group, metareg_name, term, term_name, outcome, outcome_name,
            estimand = dataset, estimate, std.error, conf.low, conf.high)

hypothesis_contrasts <-
  contrasts |>
  filter(metareg_group %in% c("primary", "secondary", "new"),
         specification == "standard", tested,
         outcome %in% c("favorability", "votechoice")) |>
  transmute(metareg_group, metareg_name, term, term_name, outcome, outcome_name,
            estimand = contrast, estimate, std.error, conf.low = NA_real_,
            conf.high = NA_real_)

plotted <-
  bind_rows(overall, overall_contrasts, hypotheses, hypothesis_contrasts) |>
  mutate(
    estimand = factor(estimand, levels = estimand_levels),
    panel = if_else(str_detect(as.character(estimand), "-"),
                    "Difference-in-coefficients", "Metaregression coefficient"),
    lower = if_else(is.na(conf.low), estimate - 2 * std.error, conf.low),
    upper = if_else(is.na(conf.high), estimate + 2 * std.error, conf.high),
    printed = sprintf("%.1f", estimate)
  ) |>
  arrange(metareg_group, term_name, outcome, estimand, .locale = "en")

write_csv(
  plotted |>
    select(metareg_group, metareg_name, term, term_name, outcome, outcome_name,
           estimand, panel, estimate, std.error, lower, upper, printed),
  out("figure_oa2_metaregression_coefficients.csv")
)

figure_names <- tribble(
  ~metareg_group, ~outcome, ~stem,
  "primary", "favorability", "figure_oa2_favorability_primary",
  "primary", "votechoice", "figure_oa3_votechoice_primary",
  "secondary", "favorability", "figure_oa4_favorability_secondary",
  "secondary", "votechoice", "figure_oa5_votechoice_secondary",
  "new", "favorability", "figure_oa6_favorability_new",
  "new", "votechoice", "figure_oa7_votechoice_new"
)

draw_panel <- function(metareg_group_, outcome_, stem) {
  d <- plotted |>
    filter(outcome == outcome_,
           metareg_group %in% c("overall", metareg_group_)) |>
    mutate(term_name = fct_rev(fct_relevel(factor(term_name), "ATE")))

  dodge <- position_dodge(width = 0.65)

  g <- d |>
    ggplot(aes(x = estimate, y = term_name, color = estimand, shape = estimand)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_linerange(aes(xmin = lower, xmax = upper), position = dodge) +
    geom_point(position = dodge) +
    geom_text(aes(x = Inf, label = paste0(printed, " ")), hjust = 1,
              position = dodge, show.legend = FALSE, size = 3) +
    facet_grid(term_name ~ panel, scales = "free_y", space = "free_y") +
    scale_color_manual(values = c(
      "2018" = "#F8766D", "2020 Downballot" = "#B79F00",
      "2020 Presidential" = "#00BA38", "Combined" = "black",
      "2020D-2018" = "#00BFC4", "2020P-2018" = "#619CFF",
      "2020P-2020D" = "#F564E3"
    )) +
    scale_shape_manual(values = c("2018" = 15, "2020 Downballot" = 16,
                                  "2020 Presidential" = 17, "Combined" = 18,
                                  "2020D-2018" = 15, "2020P-2018" = 16,
                                  "2020P-2020D" = 17)) +
    coord_cartesian(xlim = c(-5, 5)) +
    labs(x = str_glue("ATE on {outcome_} (pp)"), y = "") +
    theme_hewitt() +
    theme(panel.grid.major.y = element_blank(), strip.text.y = element_blank(),
          legend.position = "none")

  save_figure(g, stem, width = 10, height = 8.5)
}

pwalk(figure_names, function(metareg_group, outcome, stem) {
  draw_panel(metareg_group, outcome, stem)
})

print(plotted |> count(metareg_group, outcome, panel))
print(str_glue("{nrow(plotted)} values printed across Figures OA2 to OA7."))
