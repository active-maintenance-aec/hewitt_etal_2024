# hewitt_etal_2024/maintained/figure_oa9_reliability.R
# Output: output/figure_oa9_reliability.pdf, .png, output/table_da35_reliability.csv
# Depends on: helpers.R, original_extracted/.../icc_2018.RDS, icc_2020.RDS
# Description: Figure OA9 and Dataverse Appendix Table DA35. How consistently the
#   research assistants coded each advertisement feature, measured as the
#   reliability of the average of the ratings a video received.

source(here::here("maintained", "helpers.R"))

# The metaregression definitions name which hypothesis group each coded feature
# belongs to, and the reliability figure is grouped the same way.
feature_groups <-
  read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE) |>
  filter(metareg_group %in% c("primary", "secondary", "new"), tested,
         !variance_term) |>
  distinct(term, term_name, metareg_group)

reliability <-
  bind_rows(
    read_deposit_rds("icc_2018.RDS") |> mutate(year = 2018),
    read_deposit_rds("icc_2020.RDS") |> mutate(year = 2020)
  ) |>
  select(term = var, spec, estimate = est, conf.low = lwr, conf.high = upr,
         p.value = pval, year)

# ICC(C, k) is the reliability of the k-rater average, with k = 3 in 2018 and
# k = 2 in 2020. ICC(1) is the single-rater figure the article quotes in prose.
average_rater <-
  reliability |>
  filter(spec == "ICC(C,k)") |>
  inner_join(feature_groups, by = "term") |>
  mutate(metareg_group = factor(metareg_group,
                                levels = c("primary", "secondary", "new")))

write_csv(
  average_rater |>
    select(term, term_name, metareg_group, year, estimate, conf.low, conf.high) |>
    arrange(metareg_group, term_name, year, .locale = "en"),
  out("table_da35_reliability.csv")
)

write_csv(
  reliability |>
    inner_join(feature_groups, by = "term") |>
    select(term, term_name, metareg_group, year, spec, estimate, conf.low,
           conf.high) |>
    arrange(term_name, year, spec, .locale = "en"),
  out("text_reliability_all_specs.csv")
)

g <-
  average_rater |>
  mutate(term_name = fct_reorder(factor(term_name), estimate)) |>
  ggplot(aes(y = term_name, x = estimate, xmin = conf.low, xmax = conf.high,
             color = metareg_group)) +
  facet_grid(metareg_group ~ year, space = "free_y", scales = "free_y") +
  geom_point() +
  geom_linerange(alpha = 0.5) +
  labs(x = "Reliability", y = "") +
  theme_hewitt() +
  theme(panel.grid.major.y = element_blank(), legend.position = "none")

save_figure(g, "figure_oa9_reliability", width = 8, height = 8)

print(average_rater |> count(metareg_group, year))
print(str_glue("{nrow(average_rater)} published reliability estimates."))
