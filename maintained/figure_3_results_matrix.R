# hewitt_etal_2024/maintained/figure_3_results_matrix.R
# Output: output/figure_3_results_matrix.pdf, .png, output/figure_3_t_statistics.csv
# Depends on: helpers.R, metaregressions.R output
# Description: Figure 3. One cell per hypothesis, outcome and electoral context,
#   carrying the t-statistic on that hypothesis's coefficient. The cells CSV this
#   writes is also what Figures OA10 and OA12 are drawn from.

source(here::here("maintained", "helpers.R"))

t_statistics <-
  read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE) |>
  filter(
    metareg_group %in% c("primary", "secondary", "new"),
    specification == "standard",
    weighting == "unweighted",
    outcome %in% c("favorability", "votechoice"),
    tested, !variance_term
  ) |>
  select(metareg_group, metareg_name, outcome, outcome_name, dataset, term,
         term_name, estimate, std.error, statistic, p.value) |>
  mutate(
    dataset = factor(dataset, levels = dataset_levels),
    group_name = recode(metareg_group,
                        primary = "2018 Primary hypotheses",
                        secondary = "2018 Secondary hypotheses",
                        new = "2020 New hypotheses") |>
      factor(levels = metareg_group_levels)
  ) |>
  arrange(group_name, term_name, outcome, dataset, .locale = "en")

write_csv(t_statistics, out("figure_3_t_statistics.csv"))

# A cell is starred at the 0.05 and 0.01 levels, which is what makes the figure
# readable as a hypothesis test rather than only as a heat map.
star_label <- function(statistic, p_value) {
  stars <- case_when(p_value < 0.01 ~ "**", p_value < 0.05 ~ "*", .default = "")
  paste0(sprintf("%.2f", statistic), stars)
}

matrix_plot <- function(d, x_var, facet_var) {
  d |>
    mutate(label = star_label(statistic, p.value)) |>
    ggplot(aes(x = .data[[x_var]], y = fct_rev(factor(term_name)))) +
    facet_grid(rows = vars(.data[[facet_var]]), cols = vars(outcome_name),
               space = "free_y", scales = "free_y", switch = "y") +
    geom_tile(aes(fill = statistic)) +
    geom_text(aes(label = label)) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                         midpoint = 0, limits = c(-5, 5)) +
    scale_x_discrete(position = "top") +
    coord_cartesian(expand = FALSE) +
    labs(x = "", y = "") +
    theme_bw() +
    theme(legend.position = "none",
          strip.placement = "outside",
          strip.background = element_rect(fill = "white", colour = "white"),
          strip.text = element_text(face = "bold", size = 10.5),
          axis.text = element_text(color = "black"),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white"))
}

save_figure(matrix_plot(t_statistics, "dataset", "group_name"),
            "figure_3_results_matrix", width = 10, height = 12)

print(t_statistics |> count(group_name, outcome_name))
print(str_glue("{nrow(t_statistics)} published t-statistics across ",
               "{n_distinct(t_statistics$term_name)} hypotheses."))
