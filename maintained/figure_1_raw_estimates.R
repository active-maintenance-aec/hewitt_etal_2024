# hewitt_etal_2024/maintained/figure_1_raw_estimates.R
# Output: output/figure_1_raw_estimates.pdf, .png,
#   output/figure_1_points.csv, output/figure_1_meta_means.csv
# Depends on: helpers.R, estimate_ates.R and metaregressions.R output
# Description: Figure 1. Every advertisement's treatment effect estimate against
#   the number of days between the study and its election, with the
#   meta-analytic mean of each panel in a narrow strip on the right.

source(here::here("maintained", "helpers.R"))

studies <- read_deposit_rds("studies.RDS") |>
  select(study_id, dataset, days_until_election)

use_outcomes <- c("favorability", "votechoice")

points <-
  read_csv(out("ate_estimates.csv"), show_col_types = FALSE) |>
  filter(outcome %in% use_outcomes) |>
  left_join(studies, by = "study_id") |>
  mutate(outcome = fct_relevel(outcome, use_outcomes),
         dataset = factor(dataset, levels = dataset_levels))

write_csv(points |> select(study_id, content_id, outcome, dataset,
                           days_until_election, estimate, conf.low, conf.high),
          out("figure_1_points.csv"))

meta_means <-
  read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE) |>
  filter(metareg_group == "overall", specification == "no_moderators",
         weighting == "unweighted", term == "intercept",
         outcome %in% use_outcomes) |>
  select(outcome, dataset, estimate, std.error, conf.low, conf.high) |>
  mutate(outcome = fct_relevel(outcome, use_outcomes),
         dataset = factor(dataset, levels = dataset_levels))

write_csv(meta_means, out("figure_1_meta_means.csv"))

# The panel strip is a coloured label rather than a legend, which is why the
# facet text is markdown.
outcome_strip <- function(outcome) {
  recode(as.character(outcome),
         votechoice = "<span style='color:blue4'>Vote choice</span>",
         favorability = "<span style='color:purple3'>Favorability</span>")
}

# The horizontal jitter separates advertisements tested on the same day. It is
# the one random element in this figure, so the seed is set here.
set.seed(42)

scatter <-
  points |>
  mutate(days_until_election = exp(jitter(log(days_until_election), amount = 0.05))) |>
  ggplot(aes(x = days_until_election, y = estimate, color = outcome)) +
  facet_grid(dataset ~ fct_reorder(outcome_strip(outcome), as.integer(outcome))) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "gray", alpha = 0.2) +
  geom_point(alpha = 0.5, stroke = 0, size = 1.5) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = outcome_colors, guide = "none") +
  scale_x_continuous(transform = reverselog_trans()) +
  coord_cartesian(ylim = c(-10, 20)) +
  labs(x = "Days until election (log scale)", y = "Average Treatment Effect (pp)") +
  theme_hewitt() +
  theme(panel.grid.major.x = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_markdown())

strip <-
  meta_means |>
  ggplot(aes(x = outcome, y = estimate, color = outcome,
             ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_linerange() +
  geom_point(alpha = 1, stroke = 0, size = 1.5) +
  facet_grid(dataset ~ "Meta-analytic mean") +
  geom_text(
    data = meta_means |>
      filter(dataset == "2018") |>
      mutate(label = recode(as.character(outcome), favorability = "Favorability",
                            votechoice = "Vote choice")),
    aes(label = label), vjust = -1.5, hjust = 0.2, angle = 45, size = 3
  ) +
  geom_text(data = meta_means, parse = TRUE, size = 3, y = 0, vjust = 1.3,
            aes(label = str_glue("hat(mu) == '{sprintf('%.1f', estimate)}'"))) +
  geom_text(data = meta_means, parse = TRUE, size = 3, y = 0, vjust = 2.8,
            aes(label = str_glue("('{sprintf('%.1f', std.error)}')"))) +
  scale_color_manual(values = outcome_colors, guide = "none") +
  coord_cartesian(ylim = c(-10, 20)) +
  theme_hewitt() +
  theme(panel.grid.major.x = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), strip.text.x = element_markdown())

save_figure(scatter + strip + plot_layout(widths = c(5, 1)),
            "figure_1_raw_estimates", width = 9, height = 6)

print(meta_means)
print(str_glue("{nrow(points)} advertisement-level estimates plotted."))
