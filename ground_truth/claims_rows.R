# hewitt_etal_2024/ground_truth/claims_rows.R
# Output: none (sourced by build_ground_truth.R, which owns the rows list)
# Depends on: the accessors and the gt_row() constructor defined in
#   build_ground_truth.R
# Description: One ground-truth row per prose claim in the extraction. The float
#   rows are built by the loop over float_summary in build_ground_truth.R; these
#   are the sentences.

# Abstract and introduction ------------------------------------------------------

gt_row("abstract_n_experiments", "abstract", "146 advertising experiments",
      value_rewrite = text_value("n_studies"),
      value_script = archive_text("n_studies"))

gt_row("abstract_n_ads", "abstract", "617 advertisements",
      value_rewrite = text_value("n_advertisements"),
      value_script = archive_text("n_advertisements"))

gt_row("abstract_n_campaigns", "abstract", "produced by 51 campaigns",
      defect_locus = "archive",
      note = str_glue("The deposit carries no campaign identifier, only ",
                      "{text_value('n_studies')} studies and ",
                      "{text_value('n_advertisements')} advertisements, so the ",
                      "number of campaigns cannot be recovered."))

gt_row("abstract_n_respondents", "abstract", "tested with over 500,000 respondents",
      value_rewrite = text_value("n_responses_collected"),
      holds = descriptive_holds("over_500000_respondents"),
      note = descriptive_evidence("over_500000_respondents"))

gt_row("intro_mu_votechoice_2018", "text", "average 2018 effect on vote choice",
      value_rewrite = text_value("mu_votechoice_2018"))
gt_row("intro_mu_votechoice_2020d", "text", "average 2020 downballot effect",
      value_rewrite = text_value("mu_votechoice_2020d"))
gt_row("intro_mu_votechoice_2020p", "text", "average 2020 presidential effect",
      value_rewrite = text_value("mu_votechoice_2020p"))
gt_row("intro_tau_votechoice_2018", "text", "2018 standard deviation of true effects",
      value_rewrite = text_value("tau_votechoice_2018"),
      defect_locus = "paper_internal",
      note = "Table 2 prints 1.42 for this quantity, which rounds to 1.4.")
gt_row("intro_tau_votechoice_2020d", "text", "2020 downballot standard deviation",
      value_rewrite = text_value("tau_votechoice_2020d"))
gt_row("intro_tau_votechoice_2020p", "text", "2020 presidential standard deviation",
      value_rewrite = text_value("tau_votechoice_2020p"))

gt_row("intro_largest_in_sd_units", "text",
      "largest standard deviation in standard units",
      value_rewrite = text_value("largest_tau_in_sd_units"),
      note = "The largest vote choice tau divided by the 50 point standard deviation of a fair binary outcome.")

gt_row("intro_relative_effectiveness", "text",
      "commonplace for ads to be 50% less or more persuasive",
      value_rewrite = 100 * text_value("tau_over_mu_deposited"),
      note = "A hedged restatement of the ratio, so no verdict is taken.")

gt_row("intro_budget_share", "text",
      "over 10% of the media budget devoted to experimentation",
      value_rewrite = 100 * text_value("optimal_share_max_at_051"),
      note = str_glue("At the estimated variability the optimal share runs ",
                      "{sprintf('%.1f', 100 * text_value('optimal_share_min_at_051'))} to ",
                      "{sprintf('%.1f', 100 * text_value('optimal_share_max_at_051'))} per cent ",
                      "across budget levels."))

# Research design ------------------------------------------------------------------

gt_row("design_n_experiments", "text", "146 randomized survey experiments",
      value_rewrite = text_value("n_studies"),
      value_script = archive_text("n_studies"))

gt_row("design_field_dates_2018", "text",
      "2018 data collected between April 4, 2018 and March 15, 2019",
      holds = descriptive_holds("field_dates_2018"),
      defect_locus = "unresolved",
      note = descriptive_evidence("field_dates_2018"))

gt_row("design_field_dates_2020", "text",
      "2020 data collected between December 7, 2019 and December 24, 2020",
      holds = descriptive_holds("field_dates_2020"),
      defect_locus = "unresolved",
      note = descriptive_evidence("field_dates_2020"))

gt_row("design_treat_changed_2018", "text",
      "four 2018 studies added or subtracted a treatment",
      value_rewrite = text_value("n_studies_treatment_changed_2018"))
gt_row("design_treat_changed_2020", "text",
      "seven 2020 studies added or subtracted a treatment",
      value_rewrite = text_value("n_studies_treatment_changed_2020"))

gt_row("design_attrition_studies", "text",
      "22 studies with reconstructed completion data",
      value_rewrite = text_value("attrition_studies_2020"))

gt_row("design_attrition_rate", "text",
      "average post-treatment attrition of approximately 4 per cent",
      value_rewrite = text_value("attrition_rate_2020"),
      note = "A hedged figure, so no verdict is taken.")

gt_row("design_attrition_significant", "text",
      "one study with a significant effect of treatment on completion",
      value_rewrite = text_value("attrition_significant_studies_2020"))

gt_row("icc_messenger_threshold", "text",
      "single-rater reliability above 0.8 for messenger race and gender",
      value_rewrite = text_value("icc_messenger_female_single_rater_2020"),
      holds = descriptive_holds("messenger_race_gender_single_rater_above_08"),
      note = descriptive_evidence("messenger_race_gender_single_rater_above_08"))

gt_row("icc_pushy", "text", "single-rater reliability of 0.23 for how pushy the ad was",
      value_rewrite = text_value("icc_pushy_single_rater"),
      defect_locus = "paper_internal",
      note = "The 2020 single-rater ICC for how_pushy is the only reliability figure the deposit holds for this item.")

gt_row("reliability_higher_2020", "text",
      "single-rater reliability higher in 2020 than in 2018",
      holds = descriptive_holds("reliability_higher_in_2020"),
      note = descriptive_evidence("reliability_higher_in_2020"))

# Results ----------------------------------------------------------------------------

gt_row("results_mu_votechoice_2018", "text", "2018 average estimated effect",
      value_rewrite = text_value("mu_votechoice_2018"))
gt_row("results_mu_votechoice_2020d", "text", "2020 downballot average estimated effect",
      value_rewrite = text_value("mu_votechoice_2020d"))
gt_row("results_mu_votechoice_2020p", "text", "2020 presidential average estimated effect",
      value_rewrite = text_value("mu_votechoice_2020p"))
gt_row("results_tau_votechoice_2018", "text", "2018 estimated standard deviation",
      value_rewrite = text_value("tau_votechoice_2018"),
      defect_locus = "paper_internal",
      note = "Table 2 prints 1.42 for this quantity, which rounds to 1.4.")
gt_row("results_tau_votechoice_2020d", "text", "2020 downballot estimated standard deviation",
      value_rewrite = text_value("tau_votechoice_2020d"))
gt_row("results_tau_votechoice_2020p", "text", "2020 presidential estimated standard deviation",
      value_rewrite = text_value("tau_votechoice_2020p"))

gt_row("results_tau_over_mu", "text", "ratio of tau to mu of 0.51 on average",
      value_rewrite = text_value("tau_over_mu_deposited"),
      note = str_glue("The deposit ships the ratio as a data file that no deposited ",
                      "script writes. Averaging the six published ratios gives ",
                      "{sprintf('%.2f', text_value('tau_over_mu_mean_of_six'))} and ",
                      "averaging the three vote choice ratios gives ",
                      "{sprintf('%.2f', text_value('tau_over_mu_mean_votechoice'))}."))

gt_row("results_pct_more_or_less", "text", "51 per cent more or less effective",
      value_rewrite = text_value("tau_over_mu_deposited"),
      note = "The same deposited ratio, expressed as a percentage.")

gt_row("results_p_votechoice_2020d", "text", "the sixth Q-test p-value is 0.054",
      value_rewrite = text_value("p_votechoice_2020d"))

gt_row("results_five_of_six", "text",
      "Q-tests reject homogeneity in five of six cases",
      holds = descriptive_holds("five_of_six_q_tests_below_0001"),
      note = descriptive_evidence("five_of_six_q_tests_below_0001"))

gt_row("results_opportunities", "text", "39 opportunities",
      holds = descriptive_holds("thirty_nine_opportunities"),
      defect_locus = "unresolved",
      note = descriptive_evidence("thirty_nine_opportunities"))

gt_row("results_one_consistent", "text",
      "just one hypothesis significant with the same sign in every context",
      holds = descriptive_holds("one_consistent_hypothesis"),
      note = descriptive_evidence("one_consistent_hypothesis"))

gt_row("results_pushy_favorability", "text",
      "how pushy predicts favorability in both 2020 contexts but not vote choice",
      holds = descriptive_holds("pushy_significant_favorability_only"),
      note = descriptive_evidence("pushy_significant_favorability_only"))

gt_row("results_effects_decline", "text",
      "effects on favorability decline closer to election day, vote choice does not",
      holds = descriptive_holds("favorability_declines_toward_election"),
      note = descriptive_evidence("favorability_declines_toward_election"))

gt_row("results_r2_favorability_min", "text", "adjusted R-squared for favorability, lower end",
      value_rewrite = text_value("omnibus_r2_favorability_min"))
gt_row("results_r2_favorability_max", "text", "adjusted R-squared for favorability, upper end",
      value_rewrite = text_value("omnibus_r2_favorability_max"))
gt_row("results_r2_votechoice_min", "text", "adjusted R-squared for vote choice, lower end",
      value_rewrite = text_value("omnibus_r2_votechoice_min"),
      note = "Dataverse Appendix Table DA28 prints this cell as less than zero, which rounds to the zero the sentence gives.")
gt_row("results_r2_votechoice_max", "text", "adjusted R-squared for vote choice, upper end",
      value_rewrite = text_value("omnibus_r2_votechoice_max"),
      defect_locus = "paper_internal",
      note = "Dataverse Appendix Table DA28 prints 0.39 in this cell, which the sentence gives as 0.32.")

# Returns to experimentation -----------------------------------------------------------

gt_row("sim_budget_min", "text", "simulated budgets from $500,000",
      value_rewrite = text_value("simulation_budget_min"))
gt_row("sim_budget_max", "text", "simulated budgets to $5,000,000",
      value_rewrite = text_value("simulation_budget_max"))
gt_row("sim_tau_tiny", "text", "tiny ad variability scenario",
      value_rewrite = min(figure_4_spend$tau_over_mu))
gt_row("sim_tau_large", "text", "large ad variability scenario",
      value_rewrite = max(figure_4_spend$tau_over_mu))
gt_row("sim_n_experiments", "text", "10,000 simulated experiments per scenario",
      value_rewrite = text_value("simulation_n_sims"))
gt_row("sim_cost_per_vote", "text", "$200 per vote benchmark",
      value_rewrite = text_value("simulation_cost_per_vote"))

gt_row("sim_optimal_min", "text", "optimal spend at the estimated variability, lower end",
      value_rewrite = 100 * text_value("optimal_share_min_at_051"))
gt_row("sim_optimal_max", "text", "optimal spend at the estimated variability, upper end",
      value_rewrite = 100 * text_value("optimal_share_max_at_051"))
gt_row("sim_optimal_large_min", "text", "optimal spend at high variability, lower end",
      value_rewrite = 100 * text_value("optimal_share_min_at_101"))
gt_row("sim_optimal_large_max", "text", "optimal spend at high variability, upper end",
      value_rewrite = 100 * text_value("optimal_share_max_at_101"))

gt_row("sim_dollars_per_vote_gap", "text",
      "wealthy campaigns win votes around $50 per vote less than small ones",
      value_rewrite = text_value("dollars_per_vote_gap_small_large_at_051"),
      note = "A hedged figure, so no verdict is taken.")

gt_row("disc_tau_over_mu", "text", "ratio of approximately 0.51",
      value_rewrite = text_value("tau_over_mu_deposited"),
      note = "The same deposited ratio as in the Results section.")
gt_row("disc_pct_better", "text", "51 per cent better than the average ad",
      value_rewrite = text_value("tau_over_mu_deposited"),
      note = "The same deposited ratio, expressed as a percentage.")

# APSR online appendix -------------------------------------------------------------------

gt_row("appendix_c1_mu_2020d", "table_oa2",
      "average survey effect for 2020 downballot, quoted in Appendix C.1",
      value_rewrite = overall_estimate("votechoice_dichotomized", "2020 Downballot",
                                       "intercept"),
      defect_locus = "paper_internal",
      note = "Table OA2 prints 1.51 for this cell.")

gt_row("appendix_c1_mu_2020p", "table_oa2",
      "average survey effect for 2020 presidential, quoted in Appendix C.1",
      value_rewrite = overall_estimate("votechoice_dichotomized", "2020 Presidential",
                                       "intercept"))

gt_row("appendix_d_max_ads", "text",
      "575 vote choice effects available to the analyst",
      value_rewrite = sum(ate_estimates$outcome == "votechoice"))

# Everything the deposit was never going to record ------------------------------------

# The remaining extraction rows are design constants, values copied from other
# articles, and the simulation grid the deposit computes and does not save. They
# get a row so the coverage check can see them, and no verdict, because there is
# nothing on the pipeline side to compare against.
uncovered_reason <- c(
  definitional = "A design constant the article states and the deposit does not record.",
  transcribed = "Copied from another article; checked once against the source.",
  structural = "Produced by a simulation grid the deposit computes and does not save."
)

walk(
  published_claims |>
    filter(!needs_block, !str_ends(claim_id, "_cells")) |>
    transmute(claim_id, claim_type, claim) |>
    pmap(list),
  function(row) {
    gt_row(row$claim_id, "text", row$claim,
          defect_locus = if (row$claim_type == "structural") "archive" else NA_character_,
          note = unname(uncovered_reason[row$claim_type]))
  }
)
