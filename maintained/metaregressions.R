# hewitt_etal_2024/maintained/metaregressions.R
# Output: output/metaregression_estimates.csv, output/metaregression_contrasts.csv
# Depends on: helpers.R, estimate_ates.R output, original_extracted/.../tagging_*.RDS
# Description: The article's second analysis step. Every advertisement's treatment
#   effect estimate is one observation; the meta-analytic models decompose the
#   variation in those estimates into sampling variability and variation in the
#   true effects, and the meta-regressions ask whether coded features of an
#   advertisement predict its effect.
#
#   Two properties of the design drive the estimator, and both are kept exactly
#   as the article specifies. Treatments within a study share a placebo group, so
#   their estimates are correlated: the models take the block-diagonal
#   variance-covariance matrix of each study's fit rather than only the standard
#   errors. And every advertisement is its own random effect, so tau is the
#   standard deviation of the true effects rather than the observed spread of the
#   estimates.
#
#   Four specifications appear in the published tables and figures:
#     no_moderators        no fixed effects at all
#     standard             race-type fixed effects (the main specification)
#     study_fixed_effects  study fixed effects
#     study_random_effects study random effects, in addition to the ad effect
#   plus an inverse-probability-weighted variant of the 2018 estimates, which the
#   appendix uses as an attrition robustness check.

source(here::here("maintained", "helpers.R"))

fits_all <- read_rds(here::here("maintained", "clean_data", "ate_fits.rds"))
fits_ipw <- read_deposit_rds("regression_fits_ipw.RDS")

studies <- read_deposit_rds("studies.RDS")
tagging <- bind_rows(
  read_deposit_rds("tagging_2018.RDS"),
  read_deposit_rds("tagging_2020.RDS")
)

# The hypotheses ---------------------------------------------------------------

# Each row is one meta-regression the article reports. terms_to_test names the
# coefficients that carry the hypothesis, as against the controls that share the
# model with them; the article's figures show only those. comparison_formula is
# the same model with the tested terms removed, which is what the published
# R-squared "all vs. control" compares against.
metaregressions <- tribble(
  ~metareg_group, ~metareg_name, ~formula, ~terms_to_test,
  "overall", "Overall", "~ 1", "intercept",

  "primary", "Primary focus", "~ primary_focus_issues + primary_focus_candidate", NA,
  "primary", "Fact type", "~ candidate_facts + policy_facts + primary_focus_issues + primary_focus_candidate", "candidate_facts,policy_facts",
  "primary", "Persuasive techniques", "~ technique_neg_name + technique_pos_name + technique_pos_transfer + technique_neg_transfer + technique_pos_testimonial + technique_neg_testimonial + technique_plain_folks", NA,
  "primary", "New fact (where fact present)", "~ fact_new_only + primary_focus_candidate + primary_focus_issues", "fact_new_only",

  "secondary", "Messenger: politician", "~ messenger_politician", NA,
  "secondary", "Explicit vote for", "~ explicit_vote_for", NA,
  "secondary", "Emotion: anger", "~ emotion_anger", NA,
  "secondary", "Emotion: enthusiasm", "~ emotion_enthusiasm", NA,
  "secondary", "Messenger: female", "~ messenger_female", NA,
  "secondary", "Primary tone", "~ tone_positive + tone_contrast", NA,
  "secondary", "Specificity: Candidate attribute", "~ how_specific_candidate_fact", NA,
  "secondary", "Specificity: Issue fact", "~ how_specific_policy_fact", NA,
  "secondary", "Production value", "~ production_value_high", NA,
  "secondary", "Cited fact (where fact present)", "~ fact_cite_only + primary_focus_candidate + primary_focus_issues", "fact_cite_only",

  "new", "Pushiness", "~ how_pushy", NA,
  "new", "Issue: BLM/Race", "~ issue_blm_race", NA,
  "new", "Issue: COVID-19", "~ issue_covid", NA,
  "new", "Issue: Decency", "~ issue_decency", NA,
  "new", "Messenger: Republican", "~ messenger_republican", NA,
  "new", "Messenger: Everyday people", "~ messenger_every_people", NA,
  "new", "Messenger: Healthcare worker", "~ messenger_healthcare", NA,

  "other", "Time to election", "~ log_days_until_election", NA,

  # The three omnibus models put every hypothesis of a given vintage into one
  # meta-regression, which is where the article's R-squared figures come from.
  "omnibus", "All primary", "~primary_focus_issues + primary_focus_candidate + candidate_facts + policy_facts + messenger_politician + explicit_vote_for + emotion_anger + technique_neg_name + technique_pos_name + technique_pos_transfer + technique_neg_transfer + technique_pos_testimonial + technique_neg_testimonial + technique_plain_folks + fact_new_yes", NA,
  "omnibus", "All primary and secondary", "~primary_focus_issues + primary_focus_candidate + candidate_facts + policy_facts + messenger_politician + explicit_vote_for + emotion_anger + technique_neg_name + technique_pos_name + technique_pos_transfer + technique_neg_transfer + technique_pos_testimonial + technique_neg_testimonial + technique_plain_folks + fact_new_yes + how_specific_candidate_fact_impute + how_specific_policy_fact_impute + production_value_high + fact_cite_yes", NA,
  "omnibus", "All primary, secondary and new", "~primary_focus_issues + primary_focus_candidate + candidate_facts + policy_facts + messenger_politician + explicit_vote_for + emotion_anger + technique_neg_name + technique_pos_name + technique_pos_transfer + technique_neg_transfer + technique_pos_testimonial + technique_neg_testimonial + technique_plain_folks + fact_new_yes + how_specific_candidate_fact_impute + how_specific_policy_fact_impute + production_value_high + fact_cite_yes + how_pushy + issue_blm_race + issue_covid + issue_decency + messenger_republican + messenger_every_people + messenger_healthcare", NA
) |>
  mutate(terms_to_test = if_else(is.na(terms_to_test),
                                 str_replace_all(formula, c("~| " = "", "\\+" = ",")),
                                 terms_to_test))

# The comparison model drops the tested terms and keeps the controls.
drop_tested_terms <- function(formula, terms_to_test) {
  if (str_detect(terms_to_test, "intercept")) return(NA_character_)
  reduce(str_split(terms_to_test, ",")[[1]],
         function(f, term) str_replace_all(f, fixed(term), "1"),
         .init = formula) |>
    str_replace_all(" *\\+ *1", "")
}

metaregressions <- metaregressions |>
  mutate(comparison_formula = map2_chr(formula, terms_to_test, drop_tested_terms))

# Display names for every coefficient any of the models above can return.
term_names <- c(
  "intercept" = "Intercept",
  "candidate_facts" = "Candidate facts",
  "policy_facts" = "Policy facts",
  "primary_focus_candidate" = "Primary focus: Candidate",
  "primary_focus_issues" = "Primary focus: Issues",
  "fact_new_only" = "New fact (where fact present)",
  "fact_new_yes" = "New fact",
  "fact_cite_yes" = "Cited fact",
  "how_specific_candidate_fact_impute" = "Specificity: Candidate facts",
  "how_specific_policy_fact_impute" = "Specificity: Policy facts",
  "fact_cite_only" = "Cited fact (where fact present)",
  "technique_neg_name" = "Technique: Negative name-calling",
  "technique_neg_testimonial" = "Technique: Negative testimonial",
  "technique_neg_transfer" = "Technique: Negative transfer of association",
  "technique_plain_folks" = "Technique: Plain folks",
  "technique_pos_name" = "Technique: Positive name-calling",
  "technique_pos_testimonial" = "Technique: Positive testimonial",
  "technique_pos_transfer" = "Technique: Positive transfer of association",
  "emotion_anger" = "Emotion: Anger",
  "emotion_enthusiasm" = "Emotion: Enthusiasm",
  "explicit_vote_for" = "Explicit vote for",
  "messenger_female" = "Messenger: Female",
  "messenger_politician" = "Messenger: Politician",
  "tone_contrast" = "Primary tone: Contrast",
  "tone_positive" = "Primary tone: Positive",
  "production_value_high" = "Production value: High",
  "how_specific_candidate_fact" = "Specificity: Candidate facts",
  "how_specific_policy_fact" = "Specificity: Policy facts",
  "how_pushy" = "How pushy",
  "issue_blm_race" = "Issue: BLM/Race",
  "issue_covid" = "Issue: COVID-19",
  "issue_decency" = "Issue: Decency",
  "messenger_republican" = "Messenger: Republican",
  "messenger_every_people" = "Messenger: Everyday people",
  "messenger_healthcare" = "Messenger: Healthcare worker",
  "log_days_until_election" = "Days until election (log scale)",
  "race_levelCongress" = "Race: Congress",
  "race_levelGov" = "Race: Gov",
  "race_levelOther" = "Race: Other",
  "race_levelStateLeg" = "Race: StateLeg",
  "race_levelRunoff" = "Race: GA Runoff"
)

specifications <- tribble(
  ~specification, ~fixef, ~ranef,
  "no_moderators", NA_character_, NA_character_,
  "standard", "~race_level", NA_character_,
  "study_fixed_effects", "~study_id", NA_character_,
  "study_random_effects", NA_character_, "~1|study_id"
)

# Assembling one meta-regression's inputs --------------------------------------

# One row per advertisement, with the study's variance-covariance blocks laid
# out along the diagonal in the same order. The time-to-election moderator is
# standardised across all three contexts before the restriction to one of them,
# so that its scale is comparable between the three columns of Table OA1.
metaregression_inputs <- function(fits, dataset_name) {
  df <- fits |>
    map(tidy) |>
    list_rbind(names_to = "study_id") |>
    mutate(content_id = str_extract(term, "(?<=content_id).*")) |>
    left_join(tagging, by = "content_id", na_matches = "never") |>
    left_join(studies, by = "study_id", suffix = c("", ".study")) |>
    mutate(
      race_level = coalesce(race_level, race_level.study),
      log_days_until_election = as.vector(scale(log(days_until_election))) / 2
    )

  V <- fits |> map(vcov) |> Matrix::bdiag() |> as.matrix()

  keep <- df$dataset == dataset_name & !is.na(df$content_id)
  df <- df[keep, ]
  V <- V[keep, keep]

  # The blocks come off the fits, so the diagonal must already equal the squared
  # standard errors; the symmetrisation below only removes floating-point dust.
  stopifnot(max(abs(sqrt(diag(V)) - df$std.error)) < 1e-7,
            max(abs(V - t(V))) < 1e-7)
  V <- (V + t(V)) / 2

  list(df = df, V = V)
}

# Fitting ----------------------------------------------------------------------

rma_fit <- function(df, V, mods, random) {
  tryCatch(
    rma.mv(data = df, yi = estimate, V = V, mods = mods, random = random,
           test = "t", sparse = TRUE, control = list(rel.tol = 1e-8)),
    error = function(cond) {
      rma.mv(data = df, yi = estimate, V = V, mods = mods, random = random,
             test = "t", control = list(rel.tol = 1e-8))
    }
  )
}

# Returns the coefficients of one meta-regression plus the variance quantities
# the published tables print beneath them: tau, the two R-squared statistics and
# the number of advertisements. Rows whose moderators are missing are dropped
# from both the data and the variance-covariance matrix, which is what makes the
# "where fact present" hypotheses estimable at all.
run_metaregression <- function(df, V, formula, fixef, ranef, comparison_formula,
                               terms_to_test) {
  if (as.formula(formula) != ~1) {
    form_vars <- str_extract_all(formula, "[A-Za-z_]+(?![A-Za-z_\\(])")[[1]]
    is_na <- df |> transmute(if_any(all_of(form_vars), is.na)) |> pull()
    if (all(is_na)) return(NULL)
    df <- df[!is_na, ]
    V <- V[!is_na, !is_na]
  }

  # A context with only one race type cannot carry race fixed effects.
  if (n_distinct(df$race_level) == 1) fixef <- str_replace(fixef, "race_level", "1")
  mods <- as.formula(if (is.na(fixef)) formula else
    paste(fixef, "+", str_replace(formula, "~", "")))
  random <- if (is.na(ranef)) ~ 1 | content_id else
    c(~ 1 | content_id, as.formula(ranef))

  m <- rma_fit(df, V, mods, random)

  out_terms <- m |>
    tidy(conf.int = TRUE) |>
    mutate(term = str_replace(term, "^overall$", "intercept"), variance_term = FALSE) |>
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high,
           variance_term)

  extra <- tibble(term = "N_content", estimate = n_distinct(m$data$content_id),
                  std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
                  conf.low = NA_real_, conf.high = NA_real_, variance_term = TRUE)

  if (is.na(ranef)) {
    if (as.formula(formula) == ~1) {
      # The profile-likelihood interval on the between-advertisement standard
      # deviation is what the article prints beside tau.
      ci <- confint(m)$random["sigma", ]
      extra <- extra |> add_row(term = "sigma", estimate = ci[["estimate"]],
                                conf.low = ci[["ci.lb"]], conf.high = ci[["ci.ub"]],
                                p.value = m$QEp, variance_term = TRUE)
    } else {
      extra <- extra |> add_row(term = "sigma", estimate = sqrt(m$sigma2),
                                variance_term = TRUE)
    }

    if (!(as.formula(formula) == (~1) & is.na(fixef))) {
      m0 <- rma_fit(df, V, ~1, ~ 1 | content_id)
      extra <- extra |> add_row(term = "r2", estimate = 1 - m$sigma2 / m0$sigma2,
                                variance_term = TRUE)
    }

    if (!is.na(comparison_formula)) {
      comparison_mods <- as.formula(if (is.na(fixef)) comparison_formula else
        paste(fixef, "+", str_replace(comparison_formula, "~", "")))
      m0_comparison <- rma_fit(df, V, comparison_mods, random)
      extra <- extra |>
        add_row(term = "r2_comparison",
                estimate = 1 - m$sigma2 / m0_comparison$sigma2, variance_term = TRUE)
    }
  }

  # The joint test for a difference across contexts needs the covariance between
  # the tested coefficients, not only their standard errors, so the relevant
  # block of vcov(m) travels with the coefficients.
  vcov_m <- vcov(m)
  colnames(vcov_m) <- rownames(vcov_m) <- out_terms$term
  tested <- rownames(vcov_m) %in% str_split(terms_to_test, ",")[[1]]

  list(
    rows = bind_rows(out_terms, extra),
    vcov = if (any(tested)) vcov_m[tested, tested, drop = FALSE] else NULL
  )
}

# Running every published specification ----------------------------------------

# The article reports the overall model under three specifications and the
# hypothesis tests under the standard one, with the 2018 hypothesis tests also
# reported under the study-effect specifications and under inverse-probability
# weighting.
jobs <- bind_rows(
  crossing(metaregressions |> filter(metareg_group == "overall"),
           specifications |> filter(specification != "study_random_effects"),
           outcome = names(fits_all), dataset = dataset_levels, weighting = "unweighted"),
  crossing(metaregressions |> filter(!metareg_group %in% c("overall")),
           specifications |> filter(specification == "standard"),
           outcome = names(fits_all), dataset = dataset_levels, weighting = "unweighted"),
  crossing(metaregressions |> filter(metareg_group %in% c("primary", "secondary", "new")),
           specifications |> filter(specification %in% c("study_fixed_effects",
                                                         "study_random_effects")),
           outcome = names(fits_all), dataset = "2018", weighting = "unweighted"),
  crossing(metaregressions |> filter(metareg_group %in% c("primary", "secondary", "new")),
           specifications |> filter(specification == "standard"),
           outcome = names(fits_ipw), dataset = "2018", weighting = "ipw")
)

inputs <- crossing(outcome = names(fits_all), dataset = dataset_levels,
                   weighting = c("unweighted", "ipw")) |>
  filter(weighting == "unweighted" | dataset == "2018") |>
  mutate(input = pmap(list(outcome, dataset, weighting), function(o, d, w) {
    metaregression_inputs(if (w == "ipw") fits_ipw[[o]] else fits_all[[o]], d)
  }))

fitted_jobs <-
  jobs |>
  mutate(result = pmap(
    list(outcome, dataset, weighting, formula, fixef, ranef, comparison_formula,
         terms_to_test),
    function(o, d, w, form, fx, rx, cmp, tt) {
      input <- inputs$input[[which(inputs$outcome == o & inputs$dataset == d &
                                     inputs$weighting == w)]]
      run_metaregression(input$df, input$V, form, fx, rx, cmp, tt)
    }
  ))

metaregression_estimates <-
  fitted_jobs |>
  mutate(result = map(result, "rows")) |>
  unnest(result) |>
  mutate(
    outcome_name = recode(outcome, favorability = "Favorability",
                          votechoice = "Vote choice",
                          votechoice_dichotomized = "Vote choice (dichotomized)"),
    term_name = unname(term_names[term]),
    tested = map2_lgl(terms_to_test, term, function(tt, t) str_detect(tt, fixed(t)))
  ) |>
  select(metareg_group, metareg_name, formula, terms_to_test, specification,
         weighting, outcome, outcome_name, dataset, term, term_name, tested,
         variance_term, estimate, std.error, statistic, p.value, conf.low, conf.high)

write_csv(metaregression_estimates, out("metaregression_estimates.csv"))

# Pooling and contrasts ---------------------------------------------------------

# The published tables and forest plots carry four further columns beside the
# three contexts: a precision-weighted pooled estimate and the three pairwise
# differences between contexts. The differences use the normal approximation,
# treating the three contexts as independent, which they are, since no
# advertisement appears in two of them.
contrast_inputs <- metaregression_estimates |>
  filter(!variance_term, weighting == "unweighted") |>
  select(metareg_group, metareg_name, specification, outcome, outcome_name,
         term, term_name, tested, dataset, estimate, std.error)

pooled <- contrast_inputs |>
  group_by(metareg_group, metareg_name, specification, outcome, outcome_name,
           term, term_name, tested) |>
  summarize(
    estimate = weighted.mean(estimate, w = std.error^-2, na.rm = TRUE),
    std.error = sum(std.error^-2, na.rm = TRUE)^-0.5,
    .groups = "drop"
  ) |>
  mutate(contrast = "Combined")

difference <- function(a, b, label) {
  contrast_inputs |>
    filter(dataset %in% c(a, b)) |>
    select(-std.error) |>
    pivot_wider(names_from = dataset, values_from = estimate) |>
    join_one_to_one(
      contrast_inputs |>
        filter(dataset %in% c(a, b)) |>
        select(-estimate) |>
        pivot_wider(names_from = dataset, values_from = std.error,
                    names_prefix = "se_"),
      by = c("metareg_group", "metareg_name", "specification", "outcome",
             "outcome_name", "term", "term_name", "tested")
    ) |>
    transmute(
      metareg_group, metareg_name, specification, outcome, outcome_name, term,
      term_name, tested,
      estimate = .data[[a]] - .data[[b]],
      std.error = sqrt(.data[[paste0("se_", a)]]^2 + .data[[paste0("se_", b)]]^2),
      contrast = label
    )
}

metaregression_contrasts <-
  bind_rows(
    pooled,
    difference("2020 Downballot", "2018", "2020D-2018"),
    difference("2020 Presidential", "2018", "2020P-2018"),
    difference("2020 Presidential", "2020 Downballot", "2020P-2020D")
  ) |>
  filter(!is.na(estimate)) |>
  mutate(statistic = estimate / std.error,
         significant = abs(statistic) > 1.96) |>
  arrange(metareg_group, metareg_name, specification, outcome, term, contrast,
          .locale = "en")

write_csv(metaregression_contrasts, out("metaregression_contrasts.csv"))

# The joint test that a hypothesis's coefficients are equal across contexts.
# The tested coefficients from the three contexts are stacked into one vector
# with their covariance blocks laid along the diagonal, and the Q statistic of a
# model carrying only the term identity asks whether what remains is more than
# sampling variability.
joint_difference_test <- function(group) {
  blocks <- group$vcov |> discard(is.null)
  if (length(blocks) < 2) return(tibble())
  V <- as.matrix(Matrix::bdiag(blocks))
  x <- group |>
    filter(map_lgl(vcov, ~ !is.null(.x))) |>
    select(dataset, coefficients) |>
    unnest(coefficients) |>
    filter(!is.na(estimate))
  if (nrow(x) != nrow(V)) return(tibble())
  if (max(abs(x$std.error - sqrt(diag(V)))) > 1e-7) return(tibble())
  mods <- if (n_distinct(x$term) == 1) ~1 else ~term
  m <- rma.mv(yi = estimate, V = V, data = x, mods = mods)
  tibble(p_diff = m$QEp)
}

joint_tests <-
  fitted_jobs |>
  filter(weighting == "unweighted") |>
  mutate(
    vcov = map(result, "vcov"),
    coefficients = map2(result, terms_to_test, function(r, tt) {
      if (is.null(r)) return(tibble(term = character(), estimate = double(),
                                    std.error = double()))
      keep <- str_split(tt, ",")[[1]]
      r$rows |> filter(term %in% keep) |> select(term, estimate, std.error)
    })
  ) |>
  arrange(dataset, .locale = "en") |>
  group_by(metareg_group, metareg_name, specification, outcome) |>
  group_modify(function(group, key) joint_difference_test(group)) |>
  ungroup()

write_csv(joint_tests, out("metaregression_joint_tests.csv"))

print(metaregression_estimates |> count(specification, weighting))
print(str_glue("{nrow(metaregression_estimates)} meta-regression rows across ",
               "{n_distinct(metaregression_estimates$metareg_name)} hypotheses; ",
               "{nrow(metaregression_contrasts)} pooled estimates and contrasts."))
