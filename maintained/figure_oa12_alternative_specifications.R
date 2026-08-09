# hewitt_etal_2024/maintained/figure_oa12_alternative_specifications.R
# Output: output/figure_oa12_alternative_specifications.pdf, .png,
#   output/figure_oa12_t_statistics.csv
# Depends on: helpers.R, metaregressions.R output
# Description: Figure OA12. The 2018 hypothesis tests under four specifications:
#   the main one, inverse-probability weighting for attrition, study random
#   effects and study fixed effects. It is the attrition robustness check for
#   Figure 3's leftmost column.

source(here::here("maintained", "helpers.R"))

specification_levels <- c("Standard", "IPW", "Study random\neffects",
                          "Study fixed\neffects")

t_statistics <-
  read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE) |>
  filter(
    metareg_group %in% c("primary", "secondary", "new"),
    dataset == "2018",
    outcome %in% c("favorability", "votechoice"),
    tested, !variance_term
  ) |>
  mutate(
    specification_name = case_when(
      weighting == "ipw" ~ "IPW",
      specification == "standard" ~ "Standard",
      specification == "study_random_effects" ~ "Study random\neffects",
      specification == "study_fixed_effects" ~ "Study fixed\neffects"
    ) |>
      factor(levels = specification_levels),
    group_name = recode(metareg_group,
                        primary = "2018 Primary hypotheses",
                        secondary = "2018 Secondary hypotheses",
                        new = "2020 New hypotheses") |>
      factor(levels = metareg_group_levels)
  ) |>
  select(metareg_group, group_name, metareg_name, outcome, outcome_name,
         specification, weighting, specification_name, term, term_name,
         estimate, std.error, statistic, p.value) |>
  arrange(group_name, term_name, outcome, specification_name, .locale = "en")

write_csv(t_statistics, out("figure_oa12_t_statistics.csv"))

star_label <- function(statistic, p_value) {
  stars <- case_when(p_value < 0.01 ~ "**", p_value < 0.05 ~ "*", .default = "")
  paste0(sprintf("%.2f", statistic), stars)
}

g <-
  t_statistics |>
  mutate(label = star_label(statistic, p.value)) |>
  ggplot(aes(x = specification_name, y = fct_rev(factor(term_name)))) +
  facet_grid(rows = vars(group_name), cols = vars(outcome_name),
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

save_figure(g, "figure_oa12_alternative_specifications", width = 10, height = 10)

print(t_statistics |> count(group_name, specification_name))
