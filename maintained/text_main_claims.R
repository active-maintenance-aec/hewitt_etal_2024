# hewitt_etal_2024/maintained/text_main_claims.R
# Output: output/text_main_claims.csv
# Depends on: helpers.R, estimate_ates.R, metaregressions.R and figure_4 output
# Description: Quantities the article states in prose that belong to no float.
#   One row per quantity, unrounded, so that the ground truth and the claims
#   file both read the number rather than recomputing it.

source(here::here("maintained", "helpers.R"))

studies <- read_deposit_rds("studies.RDS")
responses_all <- read_deposit_rds("responses.RDS")
ates <- read_csv(out("ate_estimates.csv"), show_col_types = FALSE)
estimates <- read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE)
spend <- read_csv(out("figure_4_optimal_spend.csv"), show_col_types = FALSE)
reliability <- read_csv(out("text_reliability_all_specs.csv"), show_col_types = FALSE)
missingness <- read_deposit_rds("missingness_results.RDS")
tau_over_mu <- read_csv(processed("tau_over_mu_stats.csv"), show_col_types = FALSE)
simulations <- read_csv(processed("sims_df.csv"), show_col_types = FALSE)

overall <- estimates |>
  filter(metareg_group == "overall", specification == "no_moderators",
         weighting == "unweighted")

mu <- overall |> filter(term == "intercept")
tau <- overall |> filter(term == "sigma")

pick <- function(d, outcome_id, dataset_name, column = "estimate") {
  row <- d |> filter(outcome == outcome_id, dataset == dataset_name)
  stopifnot(nrow(row) == 1)
  row[[column]]
}

# The ratio of the standard deviation of true effects to the average effect is
# the article's headline parameter. The deposit ships it as a data file that no
# deposited script writes, so both the deposited value and the two averages a
# reader could form from the printed table cells are recorded here.
ratios_all <- tau |>
  select(outcome, dataset, tau = estimate) |>
  join_one_to_one(mu |> select(outcome, dataset, mu = estimate),
                  by = c("outcome", "dataset")) |>
  mutate(ratio = tau / mu)

# The single-rater reliability is ICC(1), which is the figure the article quotes
# in prose; the figures and tables use the k-rater average instead.
single_rater <- reliability |> filter(spec == "ICC(1)")

single_rater_value <- function(term_id, year_id) {
  row <- single_rater |> filter(term == term_id, year == year_id)
  stopifnot(nrow(row) == 1)
  row$estimate
}

# The 2020 studies for which Swayable's engineers reconstructed survey
# completion from their raw logs. The intercept of each study's
# completion-on-treatment regression is that study's placebo attrition rate.
attrition_2020 <- missingness$differential_attrition_results |>
  filter(missingness_type == "attrition")

attrition_studies <- n_distinct(attrition_2020$study_id)
attrition_rate <- attrition_2020 |>
  filter(term == "(Intercept)") |>
  summarize(rate = mean(estimate)) |>
  pull(rate)
attrition_significant_studies <- attrition_2020 |>
  filter(term == "treat", p.value < 0.05) |>
  distinct(study_id) |>
  nrow()

optimal_share <- function(ratio) {
  spend |> filter(tau_over_mu == ratio) |> pull(fraction_of_budget)
}

dollars_per_vote_at <- function(budget_level, ratio) {
  row <- spend |> filter(budget == budget_level, tau_over_mu == ratio)
  stopifnot(nrow(row) == 1)
  row$dollars_per_vote
}

text_main_claims <- tribble(
  ~quantity, ~value,
  "n_studies", nrow(studies),
  "n_advertisements", n_distinct(ates$content_id),
  "n_treatments", sum(studies$n_treatments),
  "n_responses_analysed", sum(as.integer(studies$n_responses_study)),
  "n_responses_collected", nrow(responses_all),
  "n_studies_treatment_changed_2018",
    sum(studies$dataset == "2018" &
          (studies$any_treatment_added | studies$any_treatment_removed)),
  "n_studies_treatment_changed_2020",
    sum(studies$dataset != "2018" &
          (studies$any_treatment_added | studies$any_treatment_removed)),
  "mu_votechoice_2018", pick(mu, "votechoice", "2018"),
  "mu_votechoice_2020d", pick(mu, "votechoice", "2020 Downballot"),
  "mu_votechoice_2020p", pick(mu, "votechoice", "2020 Presidential"),
  "tau_votechoice_2018", pick(tau, "votechoice", "2018"),
  "tau_votechoice_2020d", pick(tau, "votechoice", "2020 Downballot"),
  "tau_votechoice_2020p", pick(tau, "votechoice", "2020 Presidential"),
  "p_votechoice_2020d", pick(tau, "votechoice", "2020 Downballot", "p.value"),
  "tau_over_mu_deposited", tau_over_mu$estimate,
  "tau_over_mu_mean_of_six", mean(ratios_all$ratio[ratios_all$outcome != "votechoice_dichotomized"]),
  "tau_over_mu_mean_votechoice", mean(ratios_all$ratio[ratios_all$outcome == "votechoice"]),
  "largest_tau_in_sd_units", max(tau$estimate[tau$outcome == "votechoice"]) / 50,
  "icc_pushy_single_rater", single_rater_value("how_pushy", 2020),
  "icc_messenger_female_single_rater_2020", single_rater_value("messenger_female", 2020),
  "icc_messenger_female_single_rater_2018", single_rater_value("messenger_female", 2018),
  "attrition_studies_2020", attrition_studies,
  "attrition_rate_2020", attrition_rate,
  "attrition_significant_studies_2020", attrition_significant_studies,
  "optimal_share_min_at_051", min(optimal_share(0.51)),
  "optimal_share_max_at_051", max(optimal_share(0.51)),
  "optimal_share_min_at_101", min(optimal_share(1.01)),
  "optimal_share_max_at_101", max(optimal_share(1.01)),
  "dollars_per_vote_gap_small_large_at_051",
    dollars_per_vote_at(5e5, 0.51) - dollars_per_vote_at(5e6, 0.51),
  "simulation_n_sims", unique(simulations$n_sims),
  "simulation_cost_per_ad", unique(simulations$c_per_ad),
  "simulation_cost_per_subject", unique(simulations$c_per_sub),
  "simulation_cost_per_vote", unique(simulations$cpv),
  "simulation_mu", unique(simulations$mu),
  "simulation_budget_min", min(simulations$budget),
  "simulation_budget_max", max(simulations$budget)
)

# The R-squared range the article quotes comes from the omnibus model that puts
# every 2018 primary hypothesis into one meta-regression.
omnibus_r2 <-
  estimates |>
  filter(metareg_name == "All primary", specification == "standard",
         weighting == "unweighted", term == "r2_comparison") |>
  select(outcome, dataset, estimate)

text_main_claims <-
  text_main_claims |>
  bind_rows(tribble(
    ~quantity, ~value,
    "omnibus_r2_favorability_min", min(omnibus_r2$estimate[omnibus_r2$outcome == "favorability"]),
    "omnibus_r2_favorability_max", max(omnibus_r2$estimate[omnibus_r2$outcome == "favorability"]),
    "omnibus_r2_votechoice_min", min(omnibus_r2$estimate[omnibus_r2$outcome == "votechoice"]),
    "omnibus_r2_votechoice_max", max(omnibus_r2$estimate[omnibus_r2$outcome == "votechoice"])
  ))

stopifnot(!any(duplicated(text_main_claims$quantity)),
          all(!is.na(text_main_claims$value)))

write_csv(text_main_claims, out("text_main_claims.csv"))

print(text_main_claims, n = Inf)
