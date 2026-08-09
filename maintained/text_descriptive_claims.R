# hewitt_etal_2024/maintained/text_descriptive_claims.R
# Output: output/text_descriptive_claims.csv
# Depends on: helpers.R, estimate_ates.R, metaregressions.R, figure_3 output
# Description: Claims the article makes about shape, sign or count rather than
#   about a printed number. Each gets a computed truth value and a line of
#   evidence, because a sentence with no numeral in it is still a claim.

source(here::here("maintained", "helpers.R"))

studies <- read_deposit_rds("studies.RDS")
responses_all <- read_deposit_rds("responses.RDS")
estimates <- read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE)
t_statistics <- read_csv(out("figure_3_t_statistics.csv"), show_col_types = FALSE)
reliability <- read_csv(out("text_reliability_all_specs.csv"), show_col_types = FALSE)
table_1_cells <- read_csv(out("table_1_studies_summary_cells.csv"), show_col_types = FALSE)

claims <- list()

record <- function(quantity, holds, evidence) {
  claims[[length(claims) + 1]] <<- tibble(quantity = quantity, holds = holds,
                                          evidence = evidence)
  invisible(NULL)
}

# Sample size ---------------------------------------------------------------------

analysed <- sum(as.integer(studies$n_responses_study))
collected <- nrow(responses_all)

record(
  "over_500000_respondents",
  collected > 500000,
  str_glue("The deposit holds {format(collected, big.mark = ',')} responses in all ",
           "and {format(analysed, big.mark = ',')} after the exclusions the article ",
           "describes, which is what Table 1 totals.")
)

# Heterogeneity -------------------------------------------------------------------

q_tests <-
  estimates |>
  filter(metareg_group == "overall", specification == "no_moderators",
         weighting == "unweighted", term == "sigma",
         outcome %in% c("favorability", "votechoice"))

# The article's threshold is the one its own table prints, so the comparison is
# made at three decimals: the 2020 presidential p-value is 0.0010664, which the
# table prints as 0.001.
record(
  "five_of_six_q_tests_below_0001",
  sum(round(q_tests$p.value, 3) <= 0.001) == 5,
  str_glue("{sum(round(q_tests$p.value, 3) <= 0.001)} of {nrow(q_tests)} Q-test ",
           "p-values are at or below 0.001 at the precision Table 2 prints; the ",
           "largest is {sprintf('%.3f', max(q_tests$p.value))}.")
)

# Consistency across contexts --------------------------------------------------------

consistency <-
  t_statistics |>
  mutate(significant = p.value < 0.05, sign = sign(statistic)) |>
  group_by(term_name, outcome_name) |>
  summarize(
    contexts = n(),
    all_significant = all(significant),
    same_sign = n_distinct(sign) == 1,
    .groups = "drop"
  ) |>
  mutate(consistent = all_significant & same_sign)

record(
  "one_consistent_hypothesis",
  sum(consistency$consistent) == 1,
  str_glue("Of {nrow(consistency)} hypothesis-by-outcome combinations, ",
           "{sum(consistency$consistent)} has coefficients that are significant ",
           "with the same sign in every context it was tested in: ",
           "{str_flatten(str_c(consistency$term_name[consistency$consistent], ' on ', ",
                        "str_to_lower(consistency$outcome_name[consistency$consistent])), ', ')}.")
)

record(
  "thirty_nine_opportunities",
  nrow(consistency) == 39,
  str_glue("Figure 3 has {n_distinct(t_statistics$term_name)} hypothesis rows and two ",
           "outcome columns, so {nrow(consistency)} hypothesis-by-outcome ",
           "combinations, carrying {nrow(t_statistics)} t-statistics in all.")
)

# Time to election --------------------------------------------------------------------

time_to_election <-
  estimates |>
  filter(metareg_name == "Time to election", specification == "standard",
         weighting == "unweighted", term == "log_days_until_election",
         outcome %in% c("favorability", "votechoice"))

favorability_declines <- time_to_election |>
  filter(outcome == "favorability", p.value < 0.05, estimate > 0)
votechoice_declines <- time_to_election |>
  filter(outcome == "votechoice", p.value < 0.05, estimate > 0)

record(
  "favorability_declines_toward_election",
  nrow(favorability_declines) >= 2 & nrow(votechoice_declines) == 0,
  str_glue("The coefficient on log days until election is positive and significant in ",
           "{nrow(favorability_declines)} of three contexts for favorability and ",
           "{nrow(votechoice_declines)} of three for vote choice.")
)

# Pushiness ----------------------------------------------------------------------------

pushy <- t_statistics |> filter(term_name == "How pushy")

record(
  "pushy_significant_favorability_only",
  all(pushy$p.value[pushy$outcome_name == "Favorability"] < 0.05) &
    all(pushy$statistic[pushy$outcome_name == "Favorability"] > 0) &
    !all(pushy$p.value[pushy$outcome_name == "Vote choice"] < 0.05),
  str_glue("Pushiness is significant in both 2020 contexts on favorability ",
           "(t = {str_flatten(sprintf('%.2f', pushy$statistic[pushy$outcome_name == 'Favorability']), ', ')}) ",
           "and in one of two on vote choice ",
           "(t = {str_flatten(sprintf('%.2f', pushy$statistic[pushy$outcome_name == 'Vote choice']), ', ')}).")
)

# Reliability ---------------------------------------------------------------------------

single_rater <- reliability |> filter(spec == "ICC(1)")

messenger_race_gender <- single_rater |>
  filter(term %in% c("messenger_female", "messenger_male", "messenger_white",
                     "messenger_black", "messenger_latinx"))

record(
  "messenger_race_gender_single_rater_above_08",
  all(messenger_race_gender$estimate > 0.8),
  str_glue("Single-rater ICC for the messenger race and gender items runs ",
           "{sprintf('%.2f', min(messenger_race_gender$estimate))} to ",
           "{sprintf('%.2f', max(messenger_race_gender$estimate))} across ",
           "{nrow(messenger_race_gender)} item-years.")
)

paired <- single_rater |>
  select(term, year, estimate) |>
  pivot_wider(names_from = year, values_from = estimate, names_prefix = "y") |>
  filter(!is.na(y2018), !is.na(y2020))

# "Overall" reads as the average across items, which is the comparison made
# here; item by item the two years are close to even.
record(
  "reliability_higher_in_2020",
  mean(paired$y2020) > mean(paired$y2018),
  str_glue("Mean single-rater reliability across the {nrow(paired)} items coded in ",
           "both years is {sprintf('%.3f', mean(paired$y2018))} in 2018 and ",
           "{sprintf('%.3f', mean(paired$y2020))} in 2020, and {sum(paired$y2020 > paired$y2018)} ",
           "of {nrow(paired)} items are individually higher in 2020.")
)

# Field dates ------------------------------------------------------------------------------

study_dates <- studies |>
  group_by(dataset) |>
  summarize(first = min(date), last = max(date), .groups = "drop")

response_dates <- responses_all |>
  left_join(studies |> select(study_id, dataset), by = "study_id") |>
  group_by(dataset) |>
  summarize(first = min(as.Date(start_time)), last = max(as.Date(start_time)),
            .groups = "drop")

record(
  "field_dates_2018",
  FALSE,
  str_glue("The 2018 studies were fielded between ",
           "{study_dates$first[study_dates$dataset == '2018']} and ",
           "{study_dates$last[study_dates$dataset == '2018']}, and their responses ",
           "were collected between ",
           "{response_dates$first[response_dates$dataset == '2018']} and ",
           "{response_dates$last[response_dates$dataset == '2018']}. The article ",
           "gives April 4, 2018 to March 15, 2019, which is the first response and ",
           "the last study date.")
)

record(
  "field_dates_2020",
  FALSE,
  str_glue("The 2020 studies were fielded between ",
           "{min(study_dates$first[study_dates$dataset != '2018'])} and ",
           "{max(study_dates$last[study_dates$dataset != '2018'])}, and their ",
           "responses were collected between ",
           "{min(response_dates$first[response_dates$dataset != '2018'])} and ",
           "{max(response_dates$last[response_dates$dataset != '2018'])}. The article ",
           "gives December 7, 2019 to December 24, 2020, which matches neither ",
           "endpoint at the upper end.")
)

last_study_2020d <- table_1_cells |>
  filter(dataset == "2020 Downballot", quantity %in% c("last_study", "last_in_file"))

record(
  "table_1_last_study_2020_downballot",
  FALSE,
  str_glue("The last 2020 downballot study was fielded on ",
           "{last_study_2020d$value[last_study_2020d$quantity == 'last_study']}; the ",
           "published cell gives ",
           "{last_study_2020d$value[last_study_2020d$quantity == 'last_in_file']}, ",
           "which is the last study in the data file's order rather than in date order.")
)

text_descriptive_claims <- list_rbind(claims)

stopifnot(!any(duplicated(text_descriptive_claims$quantity)),
          all(!is.na(text_descriptive_claims$holds)))

write_csv(text_descriptive_claims, out("text_descriptive_claims.csv"))

print(text_descriptive_claims, n = Inf)
