# hewitt_etal_2024/maintained/figure_oa10_results_by_reliability.R
# Output: output/figure_oa10_results_by_reliability.pdf, .png,
#   output/figure_oa10_reliability_groups.csv
# Depends on: helpers.R, figure_3_results_matrix.R output,
#   original_extracted/.../icc_2020.RDS
# Description: Figure OA10. The same t-statistics as Figure 3, regrouped by how
#   consistently the 2020 raters coded the feature, so that a reader can see
#   whether the inconsistency across contexts is concentrated in the items the
#   coders agreed least about.

source(here::here("maintained", "helpers.R"))

t_statistics <- read_csv(out("figure_3_t_statistics.csv"), show_col_types = FALSE)

# ICC(C, k) is the reliability of the average of the k ratings a video received,
# which is the quantity the analysis actually uses.
icc_2020 <-
  read_deposit_rds("icc_2020.RDS") |>
  filter(spec == "ICC(C,k)") |>
  select(term = var, icc = est)

reliability_groups <-
  t_statistics |>
  left_join(icc_2020, by = "term") |>
  mutate(
    reliability = case_when(
      icc < 0.5 ~ "Low reliability",
      icc < 0.75 ~ "Medium reliability",
      icc >= 0.75 ~ "High reliability"
    ) |>
      factor(levels = c("High reliability", "Medium reliability", "Low reliability")),
    dataset = factor(dataset, levels = dataset_levels)
  )

stopifnot(!any(is.na(reliability_groups$reliability)))

write_csv(
  reliability_groups |>
    distinct(term, term_name, icc, reliability) |>
    arrange(reliability, term_name, .locale = "en"),
  out("figure_oa10_reliability_groups.csv")
)

star_label <- function(statistic, p_value) {
  stars <- case_when(p_value < 0.01 ~ "**", p_value < 0.05 ~ "*", .default = "")
  paste0(sprintf("%.2f", statistic), stars)
}

g <-
  reliability_groups |>
  mutate(label = star_label(statistic, p.value)) |>
  ggplot(aes(x = dataset, y = fct_rev(factor(term_name)))) +
  facet_grid(rows = vars(reliability), cols = vars(outcome_name),
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

save_figure(g, "figure_oa10_results_by_reliability", width = 10, height = 12)

print(reliability_groups |> distinct(term_name, icc, reliability) |>
        count(reliability))
