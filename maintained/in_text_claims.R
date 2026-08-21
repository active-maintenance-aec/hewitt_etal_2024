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
library(excheckr)

options(width = 200)

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

# The corrections this paper's errata note publishes, read from the spine errata.qmd writes
# rather than from the note's prose. A claim an entry names is scored against the correction
# and not against the sentence the article prints, so a corrected claim that drifted back to
# the published value stops the run instead of quietly reading as a match.
#
# quantity_classes stays at its default. Entry 4 corrects a cell of Table 1 and names
# table_1_cells, which is a float-coverage row: no *_cells row is a block, so this file never
# prints one, and the entry is classed "float" for that reason.
errata_entries <- read_csv(here::here("errata_entries.csv"),
                           col_types = cols(.default = col_character()))

# The extraction already records which claims cannot be compared at the precision the article
# prints; it just records it in the article's own vocabulary. That column is translated here
# rather than duplicated, so `comparison` stays the single source both instruments read.
#
#   "=="      the article states a value, and it is compared
#   "approx"  the article hedges ("approximately 4 per cent", "about 10%"), so agreement at
#             printed precision is the wrong test. The emitter this replaces threw the
#             computed value away and printed NA, which is the uncountable comparison: the
#             number now prints beside the hedge, and is asserted against nothing.
#   ">", "<"  the article states a bound, not a value, so equality against the bound is the
#             wrong test for the same reason.
published_claims <- published_claims |>
  mutate(expect = case_when(is.na(comparison) | comparison == "==" ~ "compare",
                            comparison %in% c("approx", ">", "<") ~ "range"))

# The scoring machinery comes from excheckr, which carries the verdict ladder, the typography
# parser and the printed form this file used to define for itself, signed zero included.
# format = "id" is the CLAIM <id> = <value> || [verdict] <label> line
# ground_truth/build_ground_truth.R parses, and it must stay exactly that.
#
# The precision is no longer named twice: claim_digits() reproduces every digit this
# extraction declares from the typography of value_paper alone, and the only rows it cannot
# are the eight with no published value, which are descriptive and print a truth value.
claim_start(published = published_claims, errata = errata_entries, format = "id",
            expect_column = "expect")

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
claim("abstract_n_experiments", n_distinct(ates$study_id),
     "Distinct studies with at least one estimated treatment effect")

# "This archive includes 617 advertisements produced by 51 campaigns and tested
#  with over 500,000 respondents."
claim("abstract_n_ads", n_distinct(ates$content_id),
     "Distinct advertisements with an estimated treatment effect")

# "This archive includes 617 advertisements produced by 51 campaigns"
claim("abstract_n_campaigns", NA_real_,
     "No campaign identifier in the deposit; the study and advertisement counts are what it supports",
     expect = "absent")

# "tested with over 500,000 respondents"
claim("abstract_n_respondents", holds_for("over_500000_respondents"),
           "Responses in the deposit exceed 500,000",
      digits = 0, expect = "shape")

# Introduction ----

# "we find that the average ad affected immediately measured vote choice by 2.3
#  percentage points, 1.2 points, and 0.8 points, respectively."
claim("intro_mu_votechoice_2018", quantity("mu_votechoice_2018"),
     "Meta-analytic mean effect on vote choice, 2018")
claim("intro_mu_votechoice_2020d", quantity("mu_votechoice_2020d"),
     "Meta-analytic mean effect on vote choice, 2020 downballot")
claim("intro_mu_votechoice_2020p", quantity("mu_votechoice_2020p"),
     "Meta-analytic mean effect on vote choice, 2020 presidential")

# "our meta-analytic results indicate that the standard deviation of the
#  distribution of true treatment effects were 1.5 points, 0.5 points, and 0.3
#  points, respectively"
claim("intro_tau_votechoice_2018", quantity("tau_votechoice_2018"),
     "Standard deviation of true effects on vote choice, 2018")
claim("intro_tau_votechoice_2020d", quantity("tau_votechoice_2020d"),
     "Standard deviation of true effects on vote choice, 2020 downballot")
claim("intro_tau_votechoice_2020p", quantity("tau_votechoice_2020p"),
     "Standard deviation of true effects on vote choice, 2020 presidential")

# "even the largest of these amounts to just 0.03 standard deviations."
claim("intro_largest_in_sd_units", max(c(quantity("tau_votechoice_2018"),
                                        quantity("tau_votechoice_2020d"),
                                        quantity("tau_votechoice_2020p"))) / 50,
     "Largest vote choice tau divided by the 50 point standard deviation of a fair coin")

# "it is common for advertisements to be 50% less or 50% more persuasive than
#  the average advertisement."
claim("intro_relative_effectiveness", 100 * quantity("tau_over_mu_deposited"),
     "A hedged restatement of the tau over mu ratio")

# "optimal campaign behavior would be to devote a substantial portion (over
#  10%) of their media budget to ad experimentation"
claim("intro_budget_share",
     100 * max(spend$fraction_of_budget[spend$tau_over_mu == 0.51]),
     "Largest optimal experimentation share across budgets at the estimated variability")

# Research design ----

# "We analyze data from 146 randomized survey experiments conducted by the
#  technology platform Swayable during the 2018 and 2020 elections."
claim("design_n_experiments", n_distinct(ates$study_id),
     "Distinct studies in the analysis")

# "All data in the 2018 studies were collected by Swayable between April 4, 2018
#  and March 15, 2019."
claim("design_field_dates_2018", holds_for("field_dates_2018"),
           "Both stated 2018 endpoints describe the same quantity",
      digits = 0, expect = "shape")

# "All data in the 2020 studies (both downballot and presidential) were
#  collected between December 7, 2019 and December 24, 2020."
claim("design_field_dates_2020", holds_for("field_dates_2020"),
           "Both stated 2020 endpoints describe the same quantity",
      digits = 0, expect = "shape")

# "in a small number of cases (four studies in 2018, seven studies in 2020),
#  Swayable either added or subtracted a treatment at some point during the
#  experiment"
claim("design_treat_changed_2018", quantity("n_studies_treatment_changed_2018"),
     "2018 studies flagged as having added or removed a treatment")
claim("design_treat_changed_2020", quantity("n_studies_treatment_changed_2020"),
     "2020 studies flagged as having added or removed a treatment")

# "for a subset of the surveys in 2020 (N = 22), Swayable's engineers were able
#  to reconstruct a dataset of all subjects who ever started the survey. The
#  average rate of post-treatment attrition was approximately 4%, and in only
#  one of these 22 studies, do we find a statistically significant effect of
#  treatment on survey completion."
claim("design_attrition_studies", quantity("attrition_studies_2020"),
     "2020 studies with reconstructed survey completion data")
claim("design_attrition_rate", 100 * quantity("attrition_rate_2020"),
     "A hedged attrition rate, computed here rather than left to the ground truth")
claim("design_attrition_significant", quantity("attrition_significant_studies_2020"),
     "Studies with a significant effect of treatment on completion")

# "Research assistants were highly consistent in their ratings of the most
#  explicit features such as the race and gender of the primary messenger
#  (single-rater ICC > 0.8)"
claim("icc_messenger_threshold",
           holds_for("messenger_race_gender_single_rater_above_08"),
           "Every messenger race and gender item clears a single-rater ICC of 0.8",
      digits = 0, expect = "shape")

# "but less consistent in their ratings of the most subjective characteristics,
#  such as how 'pushy' the ad was (single-rater ICC = 0.23)."
claim("icc_pushy", quantity("icc_pushy_single_rater"),
     "Single-rater ICC(1) for the pushiness item in 2020")

# "Overall, single-rater reliability was higher in 2020 compared to 2018"
claim("reliability_higher_2020", holds_for("reliability_higher_in_2020"),
           "Mean single-rater reliability is higher in 2020",
      digits = 0, expect = "shape")

# Results ----

# "in 2018, the average estimated effect on immediately measured vote choice of
#  a single ad was 2.3 percentage points; in 2020 downballot races, the
#  estimated effect was halved to 1.2 percentage points. In the 2020
#  presidential races, the average estimated effect was smaller at 0.8
#  percentage points."
claim("results_mu_votechoice_2018", quantity("mu_votechoice_2018"),
     "Table 2 mu, vote choice, 2018")
claim("results_mu_votechoice_2020d", quantity("mu_votechoice_2020d"),
     "Table 2 mu, vote choice, 2020 downballot")
claim("results_mu_votechoice_2020p", quantity("mu_votechoice_2020p"),
     "Table 2 mu, vote choice, 2020 presidential")

# "In 2018, the estimated standard deviation of the true treatment effects was
#  1.5 points, but in both the downballot and presidential races in 2020, the
#  standard deviations were smaller, at 0.5 and 0.3 percentage points"
claim("results_tau_votechoice_2018", quantity("tau_votechoice_2018"),
     "Table 2 tau, vote choice, 2018")
claim("results_tau_votechoice_2020d", quantity("tau_votechoice_2020d"),
     "Table 2 tau, vote choice, 2020 downballot")
claim("results_tau_votechoice_2020p", quantity("tau_votechoice_2020p"),
     "Table 2 tau, vote choice, 2020 presidential")

# "the ratio of these figures is 0.51 on average, indicating intuitively that it
#  is commonplace for ads to be 51% more or less effective than the average ad."
claim("results_tau_over_mu", quantity("tau_over_mu_deposited"),
     "Ratio of tau to mu as the deposit records it")
claim("results_pct_more_or_less", 100 * quantity("tau_over_mu_deposited"),
     "The same ratio expressed as a percentage")

# "Q-tests indicate that we can conclusively reject the null hypothesis that the
#  true treatment effects are homogeneous in five of six cases (p <= 0.001),
#  while the p-value is 0.054 is the sixth case."
claim("results_p_votechoice_2020d", quantity("p_votechoice_2020d"),
     "Q-test p-value for vote choice in 2020 downballot races")
claim("results_five_of_six", holds_for("five_of_six_q_tests_below_0001"),
           "Five of the six Q-test p-values are at or below 0.001",
      digits = 0, expect = "shape")

# "Across the 39 opportunities, we observe just one case in which the
#  coefficient estimates across election types were all statistically
#  significant and had the same sign"
claim("results_opportunities", holds_for("thirty_nine_opportunities"),
           "Figure 3 offers 39 hypothesis-by-outcome combinations",
      digits = 0, expect = "shape")
claim("results_one_consistent", holds_for("one_consistent_hypothesis"),
           "Exactly one combination is significant with the same sign throughout",
      digits = 0, expect = "shape")

# "In both the 2020 downballot and 2020 presidential, 'how pushy' the ad was a
#  significant and positive predictor of effectiveness - but only for the
#  favorability outcome, not the vote choice outcome."
claim("results_pushy_favorability",
           holds_for("pushy_significant_favorability_only"),
           "Pushiness is significant on favorability in both 2020 contexts and not throughout on vote choice",
      digits = 0, expect = "shape")

# "we find meaningful decreases in effects on favorability closer to election
#  day, although this pattern does not replicate for vote choice"
claim("results_effects_decline",
           holds_for("favorability_declines_toward_election"),
           "Time to election predicts favorability effects and not vote choice effects",
      digits = 0, expect = "shape")

# "the adjusted R2 values range between 0.12 and 0.34 for favorability and
#  between 0 and 0.32 for vote choice."
omnibus <- estimates |>
  filter(metareg_name == "All primary", specification == "standard",
         weighting == "unweighted", term == "r2_comparison")

claim("results_r2_favorability_min",
     min(omnibus$estimate[omnibus$outcome == "favorability"]),
     "Smallest omnibus R-squared, favorability")
claim("results_r2_favorability_max",
     max(omnibus$estimate[omnibus$outcome == "favorability"]),
     "Largest omnibus R-squared, favorability")
claim("results_r2_votechoice_min",
     min(omnibus$estimate[omnibus$outcome == "votechoice"]),
     "Smallest omnibus R-squared, vote choice")
claim("results_r2_votechoice_max",
     max(omnibus$estimate[omnibus$outcome == "votechoice"]),
     "Largest omnibus R-squared, vote choice")

# The returns to experimentation ----

# "we consider campaigns with media budgets ranging from $500,000 to $5,000,000."
claim("sim_budget_min", min(spend$budget), "Smallest simulated media budget")
claim("sim_budget_max", max(spend$budget), "Largest simulated media budget")

# "if they believed ad variability were tiny (0.01), of the magnitude we
#  estimate (0.51), or large (1.01)"
claim("sim_tau_tiny", min(spend$tau_over_mu), "Smallest simulated ad variability")
claim("sim_tau_large", max(spend$tau_over_mu), "Largest simulated ad variability")

# "we simulate 10,000 potential experiments on ads with true effects that vary"
claim("sim_n_experiments", quantity("simulation_n_sims"),
     "Simulated experiments per setting")

# "we anchor our simulations to the estimated cost per vote of TV campaign
#  advertising of $200 per vote found in prior work"
claim("sim_cost_per_vote", quantity("simulation_cost_per_vote"),
     "Cost per vote assumed by the simulation")

# "if ad variability is the size we estimate in this article, 0.51, campaigns
#  should spend approximately 10%-13% of their media budgets on
#  experimentation. If ad variability were even larger, they should spend even
#  more, approximately 13%-17%."
claim("sim_optimal_min", 100 * min(spend$fraction_of_budget[spend$tau_over_mu == 0.51]),
     "Smallest optimal experimentation share at the estimated variability")
claim("sim_optimal_max", 100 * max(spend$fraction_of_budget[spend$tau_over_mu == 0.51]),
     "Largest optimal experimentation share at the estimated variability")
claim("sim_optimal_large_min",
     100 * min(spend$fraction_of_budget[spend$tau_over_mu == 1.01]),
     "Smallest optimal experimentation share at high variability")
claim("sim_optimal_large_max",
     100 * max(spend$fraction_of_budget[spend$tau_over_mu == 1.01]),
     "Largest optimal experimentation share at high variability")

# "when ad variability is 0.51, wealthier campaigns win votes around $50 per
#  vote less than small campaigns."
claim("sim_dollars_per_vote_gap", quantity("dollars_per_vote_gap_small_large_at_051"),
     "A hedged figure, computed here rather than left to the ground truth")

# Discussion ----

# "We found that the ratio of ads' average effects to the standard deviation of
#  these effects was approximately 0.51, meaning intuitively that it is
#  commonplace for ads to be 51% better than the average ad."
claim("disc_tau_over_mu", quantity("tau_over_mu_deposited"),
     "Ratio of tau to mu as the deposit records it")
claim("disc_pct_better", 100 * quantity("tau_over_mu_deposited"),
     "The same ratio expressed as a percentage")

# APSR online appendix ----

overall_dichotomized <- estimates |>
  filter(metareg_group == "overall", specification == "no_moderators",
         weighting == "unweighted", term == "intercept",
         outcome == "votechoice_dichotomized")

# "Compared these values with the average survey treatment effects mu from Table
#  OA2, we calculate the survey-to-field conversion factor to be approximately
#  100 based on the 2020 Downballot dataset."
claim("appendix_c1_mu_2020d",
     overall_dichotomized$estimate[overall_dichotomized$dataset == "2020 Downballot"],
     "Table OA2 mu for 2020 downballot, which Appendix C.1 quotes")

# "When applying the same method as above relate these estimated returns with
#  Swayable's 2020 Presidential data (mu = 1.07, Table OA2)"
claim("appendix_c1_mu_2020p",
     overall_dichotomized$estimate[overall_dichotomized$dataset == "2020 Presidential"],
     "Table OA2 mu for 2020 presidential, which Appendix C.1 quotes")

# "from a low of five ads to a high of 575 ads, the total number of average
#  effects on vote choice in our meta-study."
claim("appendix_d_max_ads", sum(ates$outcome == "votechoice"),
     "Advertisement-level effects on vote choice")

# Gates ----
# The verdicts above are assertions only if something reads them. assert_claims() is what
# reads them: no claim ended on a failing verdict, every claim a quantity erratum names was
# printed and printed as corrected, the claims printed are exactly those the extraction
# declares need a block, and the verdict counts partition the claims.
assert_claims()

# The exemptions are counted, never merely allowed. A file that let the unasserted set grow
# in silence would report the same clean run whether it checked every claim or none.
invisible(claim_summary())
