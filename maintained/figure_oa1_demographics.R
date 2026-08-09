# hewitt_etal_2024/maintained/figure_oa1_demographics.R
# Output: output/figure_oa1_demographics.pdf, .png,
#   output/figure_oa1_demographics.csv
# Depends on: helpers.R, original_extracted/.../responses.RDS, studies.RDS
# Description: Figure OA1. Who the respondents were, in each of the three sets of
#   experiments. Every number the figure prints on its face is written to the CSV
#   beside it.

source(here::here("maintained", "helpers.R"))

studies <- read_deposit_rds("studies.RDS") |> select(study_id, dataset)

respondents <-
  read_deposit_rds("responses.RDS") |>
  filter(drop == 0) |>
  left_join(studies, by = "study_id") |>
  select(response_id, dataset, age, gender, education, ethnicity, party) |>
  mutate(
    dataset = factor(dataset, levels = dataset_levels),
    gender = case_when(is.na(gender) ~ NA_character_,
                       !gender %in% c("female", "male") ~ "other",
                       .default = gender),
    ethnicity = if_else(str_detect(ethnicity, "hispanic"), "hispanic-or-latino",
                        ethnicity),
    education = if_else(education == "school", "some-school", education)
  )

age_summary <-
  respondents |>
  group_by(dataset) |>
  summarize(mean = mean(age), sd = sd(age), .groups = "drop") |>
  pivot_longer(c(mean, sd), names_to = "statistic", values_to = "value") |>
  mutate(variable = "age", level = NA_character_)

share_summary <- function(column) {
  respondents |>
    count(dataset, level = .data[[column]]) |>
    group_by(dataset) |>
    mutate(value = n / sum(n)) |>
    ungroup() |>
    transmute(dataset, variable = column, level = as.character(level),
              statistic = "proportion", value)
}

demographics <-
  bind_rows(
    age_summary,
    map(c("gender", "education", "ethnicity", "party"), share_summary) |> list_rbind()
  ) |>
  arrange(variable, dataset, level, statistic, .locale = "en") |>
  select(variable, dataset, level, statistic, value)

write_csv(demographics, out("figure_oa1_demographics.csv"))

share_panel <- function(column, title) {
  demographics |>
    filter(variable == column) |>
    mutate(level = replace_na(level, "NA")) |>
    ggplot(aes(x = dataset, y = value, fill = level)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(aes(label = sprintf("%.2f", value)),
              position = position_dodge(width = 0.9), vjust = -0.4, size = 2.2) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "", y = "Proportion", fill = "") +
    theme_hewitt() +
    theme(legend.position = "bottom")
}

age_panel <-
  demographics |>
  filter(variable == "age") |>
  pivot_wider(names_from = statistic, values_from = value) |>
  ggplot(aes(x = dataset, y = mean)) +
  geom_col(fill = "gray70") +
  geom_text(aes(label = sprintf("%.2f\n(SD = %.2f)", mean, sd)), vjust = -0.3,
            size = 2.6) +
  scale_y_continuous(limits = c(0, 55), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Age", x = "", y = "Mean age in years") +
  theme_hewitt()

g <- (age_panel + share_panel("gender", "Gender")) /
  (share_panel("education", "Education") + share_panel("ethnicity", "Ethnicity")) /
  (share_panel("party", "Party") + plot_spacer())

save_figure(g, "figure_oa1_demographics", width = 16, height = 20)

print(demographics |> filter(variable == "age"))
print(str_glue("{nrow(demographics)} demographic summaries across ",
               "{n_distinct(demographics$variable)} variables."))
