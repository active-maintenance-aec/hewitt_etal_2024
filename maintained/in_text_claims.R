# hewitt_etal_2024/maintained/in_text_claims.R
# Output: printed to the console; nothing is written
# Depends on: maintained/output/*, ground_truth/published_claims.csv
# Description: The second instrument. Every number the article states in prose is
#   recomputed here from the pipeline's own output, by a path of its own, and
#   printed beside the sentence it comes from. It reads the extraction, because a
#   block cannot quote the article's figure without it, and it never reads the
#   ground truth, because agreeing with the comparison would prove nothing.
#
#   Each printed line is CLAIM <id> = <value> || <label>. The id on that line is
#   the only link the coverage gate uses.

source(here::here("maintained", "helpers.R"))

options(width = 200)

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

digits_for <- function(claim_id) {
  row <- published_claims |> filter(.data$claim_id == .env$claim_id)
  stopifnot(nrow(row) == 1)
  row$digits
}

comparison_for <- function(claim_id) {
  row <- published_claims |> filter(.data$claim_id == .env$claim_id)
  stopifnot(nrow(row) == 1)
  if (is.na(row$comparison)) "==" else row$comparison
}

# Signed zero is normalised here as well as on the transcription side; whichever
# instrument normalises, both must.
render_at <- function(x, digits) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  str_replace(out, "^-(0(\\.0+)?)$", "\\1")
}

emit <- function(claim_id, value, label) {
  rendered <- if (comparison_for(claim_id) == "approx" || is.na(value)) "NA" else
    render_at(value, digits_for(claim_id))
  cat("CLAIM ", claim_id, " = ", rendered, " || ", label, "\n", sep = "")
}

emit_holds <- function(claim_id, holds, label) {
  cat("CLAIM ", claim_id, " = ", as.character(holds), " || ", label, "\n", sep = "")
}

# Pipeline output -----------------------------------------------------------------

main <- read_csv(out("text_main_claims.csv"), show_col_types = FALSE)
descriptive <- read_csv(out("text_descriptive_claims.csv"), show_col_types = FALSE)
estimates <- read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE)
ates <- read_csv(out("ate_estimates.csv"), show_col_types = FALSE)
spend <- read_csv(out("figure_4_optimal_spend.csv"), show_col_types = FALSE)

quantity <- function(name) {
  row <- main |> filter(.data$quantity == .env$name)
  stopifnot(nrow(row) == 1)
  row$value
}

holds_for <- function(name) {
  row <- descriptive |> filter(.data$quantity == .env$name)
  stopifnot(nrow(row) == 1)
  row$holds
}

# Abstract ----

# "we analyze a unique archive of 146 advertising experiments conducted by US
#  campaigns in 2018 and 2020 using the platform Swayable."
emit("abstract_n_experiments", n_distinct(ates$study_id),
     "Distinct studies with at least one estimated treatment effect")

# "This archive includes 617 advertisements produced by 51 campaigns and tested
#  with over 500,000 respondents."
emit("abstract_n_ads", n_distinct(ates$content_id),
     "Distinct advertisements with an estimated treatment effect")

# "This archive includes 617 advertisements produced by 51 campaigns"
emit("abstract_n_campaigns", NA_real_,
     "No campaign identifier in the deposit; the study and advertisement counts are what it supports")

# "tested with over 500,000 respondents"
emit_holds("abstract_n_respondents", holds_for("over_500000_respondents"),
           "Responses in the deposit exceed 500,000")

# Introduction ----

# "we find that the average ad affected immediately measured vote choice by 2.3
#  percentage points, 1.2 points, and 0.8 points, respectively."
emit("intro_mu_votechoice_2018", quantity("mu_votechoice_2018"),
     "Meta-analytic mean effect on vote choice, 2018")
emit("intro_mu_votechoice_2020d", quantity("mu_votechoice_2020d"),
     "Meta-analytic mean effect on vote choice, 2020 downballot")
emit("intro_mu_votechoice_2020p", quantity("mu_votechoice_2020p"),
     "Meta-analytic mean effect on vote choice, 2020 presidential")

# "our meta-analytic results indicate that the standard deviation of the
#  distribution of true treatment effects were 1.5 points, 0.5 points, and 0.3
#  points, respectively"
emit("intro_tau_votechoice_2018", quantity("tau_votechoice_2018"),
     "Standard deviation of true effects on vote choice, 2018")
emit("intro_tau_votechoice_2020d", quantity("tau_votechoice_2020d"),
     "Standard deviation of true effects on vote choice, 2020 downballot")
emit("intro_tau_votechoice_2020p", quantity("tau_votechoice_2020p"),
     "Standard deviation of true effects on vote choice, 2020 presidential")

# "even the largest of these amounts to just 0.03 standard deviations."
emit("intro_largest_in_sd_units", max(c(quantity("tau_votechoice_2018"),
                                        quantity("tau_votechoice_2020d"),
                                        quantity("tau_votechoice_2020p"))) / 50,
     "Largest vote choice tau divided by the 50 point standard deviation of a fair coin")

# "it is common for advertisements to be 50% less or 50% more persuasive than
#  the average advertisement."
emit("intro_relative_effectiveness", NA_real_,
     "A hedged restatement of the tau over mu ratio")

# "optimal campaign behavior would be to devote a substantial portion (over
#  10%) of their media budget to ad experimentation"
emit("intro_budget_share",
     100 * max(spend$fraction_of_budget[spend$tau_over_mu == 0.51]),
     "Largest optimal experimentation share across budgets at the estimated variability")

# Research design ----

# "We analyze data from 146 randomized survey experiments conducted by the
#  technology platform Swayable during the 2018 and 2020 elections."
emit("design_n_experiments", n_distinct(ates$study_id),
     "Distinct studies in the analysis")

# "All data in the 2018 studies were collected by Swayable between April 4, 2018
#  and March 15, 2019."
emit_holds("design_field_dates_2018", holds_for("field_dates_2018"),
           "Both stated 2018 endpoints describe the same quantity")

# "All data in the 2020 studies (both downballot and presidential) were
#  collected between December 7, 2019 and December 24, 2020."
emit_holds("design_field_dates_2020", holds_for("field_dates_2020"),
           "Both stated 2020 endpoints describe the same quantity")

# "in a small number of cases (four studies in 2018, seven studies in 2020),
#  Swayable either added or subtracted a treatment at some point during the
#  experiment"
emit("design_treat_changed_2018", quantity("n_studies_treatment_changed_2018"),
     "2018 studies flagged as having added or removed a treatment")
emit("design_treat_changed_2020", quantity("n_studies_treatment_changed_2020"),
     "2020 studies flagged as having added or removed a treatment")

# "for a subset of the surveys in 2020 (N = 22), Swayable's engineers were able
#  to reconstruct a dataset of all subjects who ever started the survey. The
#  average rate of post-treatment attrition was approximately 4%, and in only
#  one of these 22 studies, do we find a statistically significant effect of
#  treatment on survey completion."
emit("design_attrition_studies", quantity("attrition_studies_2020"),
     "2020 studies with reconstructed survey completion data")
emit("design_attrition_rate", NA_real_,
     "A hedged attrition rate; the computed placebo rate is in the ground truth")
emit("design_attrition_significant", quantity("attrition_significant_studies_2020"),
     "Studies with a significant effect of treatment on completion")

# "Research assistants were highly consistent in their ratings of the most
#  explicit features such as the race and gender of the primary messenger
#  (single-rater ICC > 0.8)"
emit_holds("icc_messenger_threshold",
           holds_for("messenger_race_gender_single_rater_above_08"),
           "Every messenger race and gender item clears a single-rater ICC of 0.8")

# "but less consistent in their ratings of the most subjective characteristics,
#  such as how 'pushy' the ad was (single-rater ICC = 0.23)."
emit("icc_pushy", quantity("icc_pushy_single_rater"),
     "Single-rater ICC(1) for the pushiness item in 2020")

# "Overall, single-rater reliability was higher in 2020 compared to 2018"
emit_holds("reliability_higher_2020", holds_for("reliability_higher_in_2020"),
           "Mean single-rater reliability is higher in 2020")

# Results ----

# "in 2018, the average estimated effect on immediately measured vote choice of
#  a single ad was 2.3 percentage points; in 2020 downballot races, the
#  estimated effect was halved to 1.2 percentage points. In the 2020
#  presidential races, the average estimated effect was smaller at 0.8
#  percentage points."
emit("results_mu_votechoice_2018", quantity("mu_votechoice_2018"),
     "Table 2 mu, vote choice, 2018")
emit("results_mu_votechoice_2020d", quantity("mu_votechoice_2020d"),
     "Table 2 mu, vote choice, 2020 downballot")
emit("results_mu_votechoice_2020p", quantity("mu_votechoice_2020p"),
     "Table 2 mu, vote choice, 2020 presidential")

# "In 2018, the estimated standard deviation of the true treatment effects was
#  1.5 points, but in both the downballot and presidential races in 2020, the
#  standard deviations were smaller, at 0.5 and 0.3 percentage points"
emit("results_tau_votechoice_2018", quantity("tau_votechoice_2018"),
     "Table 2 tau, vote choice, 2018")
emit("results_tau_votechoice_2020d", quantity("tau_votechoice_2020d"),
     "Table 2 tau, vote choice, 2020 downballot")
emit("results_tau_votechoice_2020p", quantity("tau_votechoice_2020p"),
     "Table 2 tau, vote choice, 2020 presidential")

# "the ratio of these figures is 0.51 on average, indicating intuitively that it
#  is commonplace for ads to be 51% more or less effective than the average ad."
emit("results_tau_over_mu", quantity("tau_over_mu_deposited"),
     "Ratio of tau to mu as the deposit records it")
emit("results_pct_more_or_less", 100 * quantity("tau_over_mu_deposited"),
     "The same ratio expressed as a percentage")

# "Q-tests indicate that we can conclusively reject the null hypothesis that the
#  true treatment effects are homogeneous in five of six cases (p <= 0.001),
#  while the p-value is 0.054 is the sixth case."
emit("results_p_votechoice_2020d", quantity("p_votechoice_2020d"),
     "Q-test p-value for vote choice in 2020 downballot races")
emit_holds("results_five_of_six", holds_for("five_of_six_q_tests_below_0001"),
           "Five of the six Q-test p-values are at or below 0.001")

# "Across the 39 opportunities, we observe just one case in which the
#  coefficient estimates across election types were all statistically
#  significant and had the same sign"
emit_holds("results_opportunities", holds_for("thirty_nine_opportunities"),
           "Figure 3 offers 39 hypothesis-by-outcome combinations")
emit_holds("results_one_consistent", holds_for("one_consistent_hypothesis"),
           "Exactly one combination is significant with the same sign throughout")

# "In both the 2020 downballot and 2020 presidential, 'how pushy' the ad was a
#  significant and positive predictor of effectiveness - but only for the
#  favorability outcome, not the vote choice outcome."
emit_holds("results_pushy_favorability",
           holds_for("pushy_significant_favorability_only"),
           "Pushiness is significant on favorability in both 2020 contexts and not throughout on vote choice")

# "we find meaningful decreases in effects on favorability closer to election
#  day, although this pattern does not replicate for vote choice"
emit_holds("results_effects_decline",
           holds_for("favorability_declines_toward_election"),
           "Time to election predicts favorability effects and not vote choice effects")

# "the adjusted R2 values range between 0.12 and 0.34 for favorability and
#  between 0 and 0.32 for vote choice."
omnibus <- estimates |>
  filter(metareg_name == "All primary", specification == "standard",
         weighting == "unweighted", term == "r2_comparison")

emit("results_r2_favorability_min",
     min(omnibus$estimate[omnibus$outcome == "favorability"]),
     "Smallest omnibus R-squared, favorability")
emit("results_r2_favorability_max",
     max(omnibus$estimate[omnibus$outcome == "favorability"]),
     "Largest omnibus R-squared, favorability")
emit("results_r2_votechoice_min",
     min(omnibus$estimate[omnibus$outcome == "votechoice"]),
     "Smallest omnibus R-squared, vote choice")
emit("results_r2_votechoice_max",
     max(omnibus$estimate[omnibus$outcome == "votechoice"]),
     "Largest omnibus R-squared, vote choice")

# The returns to experimentation ----

# "we consider campaigns with media budgets ranging from $500,000 to $5,000,000."
emit("sim_budget_min", min(spend$budget), "Smallest simulated media budget")
emit("sim_budget_max", max(spend$budget), "Largest simulated media budget")

# "if they believed ad variability were tiny (0.01), of the magnitude we
#  estimate (0.51), or large (1.01)"
emit("sim_tau_tiny", min(spend$tau_over_mu), "Smallest simulated ad variability")
emit("sim_tau_large", max(spend$tau_over_mu), "Largest simulated ad variability")

# "we simulate 10,000 potential experiments on ads with true effects that vary"
emit("sim_n_experiments", quantity("simulation_n_sims"),
     "Simulated experiments per setting")

# "we anchor our simulations to the estimated cost per vote of TV campaign
#  advertising of $200 per vote found in prior work"
emit("sim_cost_per_vote", quantity("simulation_cost_per_vote"),
     "Cost per vote assumed by the simulation")

# "if ad variability is the size we estimate in this article, 0.51, campaigns
#  should spend approximately 10%-13% of their media budgets on
#  experimentation. If ad variability were even larger, they should spend even
#  more, approximately 13%-17%."
emit("sim_optimal_min", 100 * min(spend$fraction_of_budget[spend$tau_over_mu == 0.51]),
     "Smallest optimal experimentation share at the estimated variability")
emit("sim_optimal_max", 100 * max(spend$fraction_of_budget[spend$tau_over_mu == 0.51]),
     "Largest optimal experimentation share at the estimated variability")
emit("sim_optimal_large_min",
     100 * min(spend$fraction_of_budget[spend$tau_over_mu == 1.01]),
     "Smallest optimal experimentation share at high variability")
emit("sim_optimal_large_max",
     100 * max(spend$fraction_of_budget[spend$tau_over_mu == 1.01]),
     "Largest optimal experimentation share at high variability")

# "when ad variability is 0.51, wealthier campaigns win votes around $50 per
#  vote less than small campaigns."
emit("sim_dollars_per_vote_gap", NA_real_,
     "A hedged figure; the computed gap is in the ground truth")

# Discussion ----

# "We found that the ratio of ads' average effects to the standard deviation of
#  these effects was approximately 0.51, meaning intuitively that it is
#  commonplace for ads to be 51% better than the average ad."
emit("disc_tau_over_mu", quantity("tau_over_mu_deposited"),
     "Ratio of tau to mu as the deposit records it")
emit("disc_pct_better", 100 * quantity("tau_over_mu_deposited"),
     "The same ratio expressed as a percentage")

# APSR online appendix ----

overall_dichotomized <- estimates |>
  filter(metareg_group == "overall", specification == "no_moderators",
         weighting == "unweighted", term == "intercept",
         outcome == "votechoice_dichotomized")

# "Compared these values with the average survey treatment effects mu from Table
#  OA2, we calculate the survey-to-field conversion factor to be approximately
#  100 based on the 2020 Downballot dataset."
emit("appendix_c1_mu_2020d",
     overall_dichotomized$estimate[overall_dichotomized$dataset == "2020 Downballot"],
     "Table OA2 mu for 2020 downballot, which Appendix C.1 quotes")

# "When applying the same method as above relate these estimated returns with
#  Swayable's 2020 Presidential data (mu = 1.07, Table OA2)"
emit("appendix_c1_mu_2020p",
     overall_dichotomized$estimate[overall_dichotomized$dataset == "2020 Presidential"],
     "Table OA2 mu for 2020 presidential, which Appendix C.1 quotes")

# "from a low of five ads to a high of 575 ads, the total number of average
#  effects on vote choice in our meta-study."
emit("appendix_d_max_ads", sum(ates$outcome == "votechoice"),
     "Advertisement-level effects on vote choice")
