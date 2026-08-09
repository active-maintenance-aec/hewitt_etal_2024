# hewitt_etal_2024/maintained/table_1_studies_summary.R
# Output: output/table_1_studies_summary.tex, output/table_1_studies_summary.csv,
#   output/table_1_studies_summary_cells.csv, output/table_da1_study_level.csv
# Depends on: helpers.R, original_extracted/.../studies.RDS
# Description: Table 1, which summarises the three sets of experiments, and the
#   study-level tables behind it (Dataverse Appendix Tables DA1 to DA3).

source(here::here("maintained", "helpers.R"))

studies <- read_deposit_rds("studies.RDS")

# Study level, which is Tables DA1 to DA3 ---------------------------------------

study_level <-
  studies |>
  transmute(
    dataset,
    study_id,
    date = as.character(date),
    n = as.integer(n_responses_study),
    n_treatments,
    n_per_treatment = as.integer(n_per_treatment),
    n_states = as.integer(n_states),
    has_votechoice = !is.na(votechoice),
    has_favorability = !is.na(favorability),
    has_age = covariates.BirthYear2020.X8236c9b99112,
    has_gender = covariates.Gender.Xa01aeb488250,
    has_education = covariates.Education_Alt2.X39f690afdab3,
    has_ethnicity = covariates.WhatRaceOrEthnicGroupMostIdentifyWith.Xcbe8335da070,
    has_ideology = covariates.PoliticalBeliefsNoOp.X756e64b019fe,
    has_partisanship = covariates.PoliticalPartyNoOp.Xc0099c5732d9,
    has_trump_approval = covariates.TrumpApproveNoOp.Xaeca50508c71,
    excluding_respondents = case_when(
      any_treatment_added & any_treatment_removed ~ "after added/removed",
      any_treatment_added ~ "after treatment added",
      any_treatment_removed ~ "after treatment removed",
      .default = ""
    )
  )

write_csv(study_level, out("table_da1_study_level.csv"))

# Table 1 ------------------------------------------------------------------------

# The deposit reads the first and last study off the rows in file order, which is
# not date order. Both readings are computed here, because they disagree: the
# 2020 downballot rows end at a study dated 2020-12-22 while the last study in
# that set was fielded on 2020-12-25. The column is headed "Last study", so the
# maximum is the quantity it names and the rewrite reports it.
date_range_by_order <-
  study_level |>
  group_by(dataset) |>
  summarize(first_in_file = first(date), last_in_file = last(date), .groups = "drop")

# N per treatment is the treatment-weighted mean of the per-study figure, and
# the three outcome shares are truncated rather than rounded, which is what the
# published table prints.
table_1 <-
  study_level |>
  group_by(dataset) |>
  summarize(
    first_study = min(date),
    last_study = max(date),
    total_n = sum(n),
    n_treatments_total = sum(n_treatments),
    n_per_treatment = as.integer(sum(n_per_treatment * n_treatments) / sum(n_treatments)),
    votechoice_only = as.integer(mean(has_votechoice & !has_favorability) * 100),
    favorability_only = as.integer(mean(!has_votechoice & has_favorability) * 100),
    both_outcomes = as.integer(mean(has_votechoice & has_favorability) * 100),
    .groups = "drop"
  ) |>
  left_join(date_range_by_order, by = "dataset") |>
  mutate(dataset = factor(dataset, levels = dataset_levels)) |>
  arrange(dataset)

write_csv(table_1, out("table_1_studies_summary.csv"))

# One row per published cell, unrounded where the cell is a number, so that the
# comparison against the page never re-rounds an already formatted string.
table_1_cells <-
  table_1 |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(-dataset, names_to = "quantity", values_to = "value") |>
  arrange(dataset, quantity, .locale = "en")

write_csv(table_1_cells, out("table_1_studies_summary_cells.csv"))

table_1_display <-
  table_1 |>
  transmute(
    Dataset = as.character(dataset),
    `First study` = first_study,
    `Last study` = last_study,
    `Total N` = format(total_n, big.mark = ","),
    `# Treatments` = n_treatments_total,
    `N per treatment` = n_per_treatment,
    `Vote choice only` = paste0(votechoice_only, "%"),
    `Favorability only` = paste0(favorability_only, "%"),
    `Both outcomes` = paste0(both_outcomes, "%")
  )

table_1_display |>
  kable("latex", booktabs = TRUE, escape = FALSE, align = "lccrrrrrr",
        linesep = "") |>
  kable_styling(latex_options = "scale_down") |>
  write_lines(out("table_1_studies_summary.tex"))

print(table_1_display)
print(str_glue("{nrow(study_level)} studies; {sum(study_level$n_treatments)} treatments; ",
               "{format(sum(study_level$n), big.mark = ',')} responses."))
