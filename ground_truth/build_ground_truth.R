# hewitt_etal_2024/ground_truth/build_ground_truth.R
# Output: ground_truth/hewitt_etal_2024_ground_truth.csv,
#   ground_truth/float_coverage.csv
# Depends on: ground_truth/published_claims.csv,
#   ground_truth/published_maintext_tables.csv,
#   ground_truth/published_appendix_values.csv, ground_truth/archive_values.csv,
#   maintained/output/*, maintained/in_text_claims.R
# Description: Build the comparison between what the article prints, what the
#   deposited code produces and what the maintained rewrite produces, then run
#   the coverage gate over the second instrument. Every number here other than
#   value_paper is read out of a pipeline output; value_paper comes only from
#   the article, through the extraction and the two positional transcriptions.

library(tidyverse)
library(here)

here::i_am("ground_truth/build_ground_truth.R")

paper_id <- "hewitt_etal_2024"

# A value_paper column whose entries all look numeric is guessed as a double,
# which silently turns -0.00 into 0 and 0.020 into 0.02. Every reader of these
# files forces the type.
published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)
published_maintext <- read_csv(
  here::here("ground_truth", "published_maintext_tables.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)
published_appendix <- read_csv(
  here::here("ground_truth", "published_appendix_values.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

# Normalisation and comparison ------------------------------------------------

# Typography rather than arithmetic: the house rule is a leading zero
# everywhere, the Unicode minus is not a hyphen, a thousands separator is not a
# digit, and a cell printed -0.00 is the same claim as one printed 0.00. What is
# preserved is the number of decimals, the only typographic fact the comparison
# needs.
normalise_printed <- function(x) {
  x |>
    str_replace_all("−|–|—", "-") |>
    str_remove_all(",") |>
    str_replace("^(-?)\\.", "\\10.") |>
    str_replace("^-(0(\\.0+)?)$", "\\1")
}

# Render a number at the article's precision, with signed zero normalised on
# this side too. Whichever instrument normalises, both must.
render_at <- function(x, digits) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  str_replace(out, "^-(0(\\.0+)?)$", "\\1")
}

convert_units <- function(x, units) {
  case_when(
    units %in% c("percent", "percentage_points_as_percent") ~ x * 100,
    .default = x
  )
}

# The rewrite is compared by exact string equality at the page's precision, with
# an epsilon so that a value sitting a hair from the rounding boundary is not
# rejected by floating point.
agrees_exactly <- function(value, value_paper, digits) {
  if (is.na(value) || is.na(value_paper) || is.na(digits)) return(NA_integer_)
  target <- suppressWarnings(as.numeric(normalise_printed(value_paper)))
  if (is.na(target)) return(NA_integer_)
  as.integer(render_at(value, digits) == normalise_printed(value_paper) |
               abs(round(value, digits) - target) < 1e-9 * max(1, abs(target)))
}

# The deposit's stored quantities are unrounded, but the published values they
# are compared against are already rounded, and a cell rounded separately from
# its neighbour can differ by a full unit in the last printed digit. The archive
# column is allowed that unit; the rewrite column is not.
agrees_loosely <- function(value, value_paper, digits) {
  if (is.na(value) || is.na(value_paper) || is.na(digits)) return(NA_integer_)
  target <- suppressWarnings(as.numeric(normalise_printed(value_paper)))
  if (is.na(target)) return(NA_integer_)
  as.integer(abs(value - target) <= 10^(-digits) + 1e-9)
}

# Checks that depend only on the extraction, before anything consumes it -------

stopifnot(
  !any(duplicated(published_claims$claim_id)),
  all(nzchar(published_claims$claim_id)),
  all(published_claims$claim_type %in%
        c("pipeline", "descriptive", "definitional", "structural", "transcribed")),
  all(published_claims$needs_block %in% c(TRUE, FALSE)),
  all(is.na(published_claims$comparison) |
        published_claims$comparison %in% c("==", "<", ">", "<=", ">=", "approx"))
)

# Every pipeline and descriptive row must require a block. The other three
# classes require one only where the pipeline can reach the quantity.
stopifnot(all(
  published_claims$needs_block[
    published_claims$claim_type %in% c("pipeline", "descriptive")
  ]
))

# value_paper is stored as the article prints it, normalised only for
# typography. A stored string that does not survive a round trip through its own
# recorded precision means digits is wrong about the precision even where it is
# right about the value, which numeric equality would pass. This is the check a
# wrong digits has to trip, so it runs before any comparison uses digits.
round_trips <- function(d) {
  numeric_rows <- d |>
    filter(!is.na(value_paper), !is.na(digits),
           str_detect(value_paper, "^-?\\d+(\\.\\d+)?$"))
  stopifnot(
    all(numeric_rows$value_paper == normalise_printed(numeric_rows$value_paper)),
    all(render_at(as.numeric(numeric_rows$value_paper), numeric_rows$digits) ==
          numeric_rows$value_paper)
  )
  invisible(NULL)
}

round_trips(published_claims)
round_trips(published_maintext)
round_trips(published_appendix)

# The prose transcription and the positional transcriptions are two hand
# readings of the same pages, and nothing else compares them.
reconcile <- tribble(
  ~claim_id, ~source, ~float, ~row_key, ~quantity,
  "results_mu_votechoice_2018", "maintext", "table_2", "Vote choice | 2018", "mu",
  "results_mu_votechoice_2020d", "maintext", "table_2", "Vote choice | 2020 Downballot", "mu",
  "results_mu_votechoice_2020p", "maintext", "table_2", "Vote choice | 2020 Presidential", "mu",
  "results_p_votechoice_2020d", "maintext", "table_2", "Vote choice | 2020 Downballot", "p_no_moderators",
  "appendix_c1_mu_2020p", "appendix", "table_oa2", "2020 Presidential", "mu"
) |>
  left_join(published_claims |> select(claim_id, prose = value_paper,
                                       prose_digits = digits), by = "claim_id") |>
  left_join(
    bind_rows(published_maintext |> mutate(source = "maintext"),
              published_appendix |> mutate(source = "appendix")) |>
      select(source, float, row_key, quantity, cell = value_paper,
             cell_digits = digits),
    by = c("source", "float", "row_key", "quantity")
  ) |>
  mutate(
    shared_digits = pmin(prose_digits, cell_digits),
    agrees = render_at(as.numeric(prose), shared_digits) ==
      render_at(as.numeric(cell), shared_digits)
  )

stopifnot(nrow(reconcile) == 5, !any(is.na(reconcile$prose)),
          !any(is.na(reconcile$cell)))
if (!all(reconcile$agrees)) {
  print(reconcile |> filter(!agrees), n = Inf)
  stop("A prose transcription disagrees with the positional transcription of the same cell.")
}

# Pipeline output --------------------------------------------------------------

read_output <- function(file) {
  read_csv(here::here("maintained", "output", file), show_col_types = FALSE)
}

table_1_cells <- read_output("table_1_studies_summary_cells.csv")
table_2_cells <- read_output("table_2_heterogeneity_cells.csv")
table_oa2_cells <- read_output("table_oa2_dichotomized_cells.csv")
figure_1_meta <- read_output("figure_1_meta_means.csv")
figure_2_tau <- read_output("figure_2_tau_labels.csv")
figure_2_inset <- read_output("figure_2_inset.csv")
figure_3_t <- read_output("figure_3_t_statistics.csv")
figure_oa12_t <- read_output("figure_oa12_t_statistics.csv")
reliability_all <- read_output("text_reliability_all_specs.csv")
table_da35 <- read_output("table_da35_reliability.csv")
demographics <- read_output("figure_oa1_demographics.csv")
metareg_estimates <- read_output("metaregression_estimates.csv")
metareg_contrasts <- read_output("metaregression_contrasts.csv")
metareg_joint <- read_output("metaregression_joint_tests.csv")
ate_estimates <- read_output("ate_estimates.csv")
figure_4_gains <- read_output("figure_4_gains_from_testing.csv")
figure_4_spend <- read_output("figure_4_optimal_spend.csv")
text_claims <- read_output("text_main_claims.csv")
descriptive_claims <- read_output("text_descriptive_claims.csv")

archive_values <- read_csv(here::here("ground_truth", "archive_values.csv"),
                           show_col_types = FALSE)

outcome_label <- c(favorability = "Favorability", votechoice = "Vote choice",
                   votechoice_dichotomized = "Vote choice (dichotomized)")

# The rewrite's answer to every transcribed cell -------------------------------

rewrite_table_1 <-
  table_1_cells |>
  filter(quantity %in% c("first_study", "last_study", "total_n",
                         "n_treatments_total", "n_per_treatment",
                         "votechoice_only", "favorability_only", "both_outcomes")) |>
  transmute(float = "table_1", row_key = dataset, quantity,
            value_rewrite = suppressWarnings(as.numeric(value)),
            value_text = value)

rewrite_table_2 <-
  table_2_cells |>
  mutate(
    row_key = str_c(outcome_label[outcome], " | ", dataset),
    quantity = case_when(
      parameter == "mu" & quantity == "estimate" ~ "mu",
      parameter == "mu" & quantity == "conf.low" ~ "mu_ci_low",
      parameter == "mu" & quantity == "conf.high" ~ "mu_ci_high",
      parameter == "tau" & quantity == "estimate" ~ str_c("tau_", specification),
      parameter == "tau" & quantity == "conf.low" ~ str_c("tau_", specification, "_ci_low"),
      parameter == "tau" & quantity == "conf.high" ~ str_c("tau_", specification, "_ci_high"),
      parameter == "tau" & quantity == "p.value" ~ str_c("p_", specification)
    )
  ) |>
  filter(!is.na(quantity), quantity != "std.error") |>
  transmute(float = "table_2", row_key, quantity, value_rewrite = value)

rewrite_table_oa2 <-
  table_oa2_cells |>
  mutate(
    quantity = case_when(
      parameter == "mu" & quantity == "estimate" ~ "mu",
      parameter == "mu" & quantity == "conf.low" ~ "mu_ci_low",
      parameter == "mu" & quantity == "conf.high" ~ "mu_ci_high",
      parameter == "tau" & quantity == "estimate" ~ str_c("tau_", specification),
      parameter == "tau" & quantity == "conf.low" ~ str_c("tau_", specification, "_ci_low"),
      parameter == "tau" & quantity == "conf.high" ~ str_c("tau_", specification, "_ci_high"),
      parameter == "tau" & quantity == "p.value" ~ str_c("p_", specification)
    )
  ) |>
  filter(!is.na(quantity), quantity != "std.error") |>
  transmute(float = "table_oa2", row_key = dataset, quantity, value_rewrite = value)

rewrite_figure_1 <-
  figure_1_meta |>
  transmute(float = "figure_1", row_key = str_c(outcome_label[outcome], " | ", dataset),
            mu = estimate, std.error) |>
  pivot_longer(c(mu, std.error), names_to = "quantity", values_to = "value_rewrite")

rewrite_figure_2 <-
  bind_rows(
    figure_2_tau |>
      filter(outcome != "votechoice_dichotomized") |>
      transmute(float = "figure_2",
                row_key = str_c(outcome_label[outcome], " | ", dataset),
                tau, tau_ci_low = tau_low, tau_ci_high = tau_high) |>
      pivot_longer(c(tau, tau_ci_low, tau_ci_high), names_to = "quantity",
                   values_to = "value_rewrite"),
    figure_2_inset |>
      arrange(estimate) |>
      transmute(float = "figure_2",
                row_key = c("Two largest treatment effects | second",
                            "Two largest treatment effects | first"),
                quantity = "n_obs", value_rewrite = n_obs)
  )

rewrite_matrix <- function(d, float) {
  d |>
    transmute(float = float,
              row_key = str_c(term_name, " | ", outcome_name, " | ", dataset),
              quantity = "t_statistic", value_rewrite = statistic)
}

rewrite_figure_3 <- rewrite_matrix(figure_3_t, "figure_3")
rewrite_figure_oa10 <- rewrite_matrix(figure_3_t, "figure_oa10")

rewrite_figure_oa12 <-
  figure_oa12_t |>
  transmute(float = "figure_oa12",
            row_key = str_c(term_name, " | ", outcome_name, " | ",
                            str_replace_all(specification_name, "\n", " ")),
            quantity = "t_statistic", value_rewrite = statistic)

rewrite_table_da35 <-
  table_da35 |>
  transmute(float = "table_da35", row_key = str_c(term_name, " | ", year),
            estimate, conf.low, conf.high) |>
  pivot_longer(c(estimate, conf.low, conf.high), names_to = "quantity",
               values_to = "value_rewrite")

# The metaregression tables ------------------------------------------------------

# The published tables abbreviate the technique labels, and the row a cell sits
# on is the only key the transcription has, so the abbreviation is reproduced
# here. This is a label, not a value.
table_label <- function(term_name) {
  term_name |>
    str_replace("^Technique: ", "") |>
    str_replace("^Positive ", "Pos. ") |>
    str_replace("^Negative ", "Neg. ")
}

metaregression_tables <- tribble(
  ~float, ~metareg_name, ~specification,
  "table_da4", "Overall", "no_moderators",
  "table_da5", "Overall", "standard",
  "table_da6", "Overall", "study_fixed_effects",
  "table_da7", "Primary focus", "standard",
  "table_da8", "New fact (where fact present)", "standard",
  "table_da9", "Fact type", "standard",
  "table_da10", "Persuasive techniques", "standard",
  "table_da11", "Specificity: Candidate attribute", "standard",
  "table_da12", "Specificity: Issue fact", "standard",
  "table_da13", "Production value", "standard",
  "table_da14", "Messenger: politician", "standard",
  "table_da15", "Messenger: female", "standard",
  "table_da16", "Explicit vote for", "standard",
  "table_da17", "Emotion: enthusiasm", "standard",
  "table_da18", "Emotion: anger", "standard",
  "table_da19", "Primary tone", "standard",
  "table_da20", "Cited fact (where fact present)", "standard",
  "table_da21", "Pushiness", "standard",
  "table_da22", "Messenger: Republican", "standard",
  "table_da23", "Messenger: Healthcare worker", "standard",
  "table_da24", "Messenger: Everyday people", "standard",
  "table_da25", "Issue: Decency", "standard",
  "table_da26", "Issue: COVID-19", "standard",
  "table_da27", "Issue: BLM/Race", "standard",
  "table_da28", "All primary", "standard",
  "table_da29", "All primary and secondary", "standard",
  "table_da30", "All primary, secondary and new", "standard",
  "table_oa1", "Time to election", "standard"
)

column_for_dataset <- c("2018" = "2018", "2020 Downballot" = "2020D",
                        "2020 Presidential" = "2020P")

coefficient_cells <-
  metaregression_tables |>
  left_join(
    metareg_estimates |>
      filter(weighting == "unweighted", !variance_term) |>
      select(metareg_name, specification, outcome, dataset, term, term_name,
             estimate, std.error),
    by = c("metareg_name", "specification"), relationship = "one-to-many"
  ) |>
  filter(!is.na(term), !str_starts(term, "study_id")) |>
  transmute(
    float,
    row_key = str_c(outcome_label[outcome], " | ", table_label(term_name), " | ",
                    column_for_dataset[dataset]),
    estimate, std.error
  ) |>
  pivot_longer(c(estimate, std.error), names_to = "quantity",
               values_to = "value_rewrite")

contrast_cells <-
  metaregression_tables |>
  left_join(
    metareg_contrasts |>
      filter(tested) |>
      select(metareg_name, specification, outcome, contrast, term_name, estimate,
             std.error),
    by = c("metareg_name", "specification"), relationship = "one-to-many"
  ) |>
  filter(!is.na(term_name)) |>
  transmute(
    float,
    row_key = str_c(outcome_label[outcome], " | ", table_label(term_name), " | ",
                    contrast),
    estimate, std.error
  ) |>
  pivot_longer(c(estimate, std.error), names_to = "quantity",
               values_to = "value_rewrite")

variance_cells <-
  metaregression_tables |>
  left_join(
    metareg_estimates |>
      filter(weighting == "unweighted", variance_term) |>
      select(metareg_name, specification, outcome, dataset, term, estimate),
    by = c("metareg_name", "specification"), relationship = "one-to-many"
  ) |>
  filter(term %in% c("sigma", "r2", "r2_comparison", "N_content")) |>
  transmute(
    float,
    row_key = str_c(outcome_label[outcome], " | ",
                    recode(term, sigma = "tau", r2 = "R2 all predictors",
                           r2_comparison = "R2 all vs control",
                           N_content = "N treatments"),
                    " | ", column_for_dataset[dataset]),
    quantity = "value", value_rewrite = estimate
  )

# The joint test is printed in the difference column of whichever variance row
# comes first, which is the R-squared row where there is one and the tau row
# otherwise.
joint_cells <-
  metaregression_tables |>
  left_join(metareg_joint, by = c("metareg_name", "specification"),
            relationship = "one-to-many") |>
  filter(!is.na(p_diff)) |>
  cross_join(tibble(label = c("R2 all vs control", "R2 all predictors", "tau"))) |>
  transmute(float,
            row_key = str_c(outcome_label[outcome], " | ", label, " | 2020P-2020D"),
            quantity = "p_diff", value_rewrite = p_diff)

rewrite_metaregression_tables <-
  bind_rows(coefficient_cells, contrast_cells, variance_cells, joint_cells) |>
  filter(!is.na(value_rewrite)) |>
  distinct(float, row_key, quantity, .keep_all = TRUE)

# Cell-by-cell comparison ---------------------------------------------------------

# Table 3 states the three constants the simulation runs on, which the
# simulation output records.
rewrite_table_3 <-
  read_output("text_main_claims.csv") |>
  filter(quantity %in% c("simulation_cost_per_vote", "simulation_cost_per_ad",
                         "simulation_cost_per_subject")) |>
  transmute(
    float = "table_3",
    row_key = recode(quantity,
                     simulation_cost_per_vote = "Typical advertising effect",
                     simulation_cost_per_ad = "Cost of producing one ad",
                     simulation_cost_per_subject = "Cost of ad testing"),
    quantity = recode(quantity,
                      simulation_cost_per_vote = "dollars_per_vote",
                      simulation_cost_per_ad = "dollars",
                      simulation_cost_per_subject = "dollars_per_subject"),
    value_rewrite = value
  )

rewrite_cells <-
  bind_rows(
    rewrite_table_1 |> select(-value_text), rewrite_table_2, rewrite_table_oa2,
    rewrite_figure_1, rewrite_figure_2, rewrite_figure_3, rewrite_figure_oa10,
    rewrite_figure_oa12, rewrite_table_da35, rewrite_metaregression_tables,
    rewrite_table_3
  ) |>
  distinct(float, row_key, quantity, .keep_all = TRUE)

archive_cells <- archive_values |> distinct(float, row_key, quantity, .keep_all = TRUE)

published_cells <- bind_rows(published_maintext, published_appendix)

# The published "<0" cells are a claim that a negative R-squared was printed as
# an inequality; they are compared with the operator rather than by equality.
cell_comparison <-
  published_cells |>
  left_join(rewrite_cells, by = c("float", "row_key", "quantity")) |>
  left_join(archive_cells, by = c("float", "row_key", "quantity")) |>
  mutate(
    is_inequality = str_starts(value_paper, "<"),
    inequality_bound = suppressWarnings(as.numeric(str_remove(value_paper, "^<"))),
    is_date = str_detect(value_paper, "^\\d{4}-\\d{2}-\\d{2}$"),
    reproduced_rewrite = case_when(
      is.na(value_rewrite) & !is_date ~ NA_integer_,
      is_inequality ~ as.integer(value_rewrite < inequality_bound),
      is_date ~ NA_integer_,
      .default = map2_int(value_rewrite, row_number(),
                          function(v, i) agrees_exactly(v, value_paper[i], digits[i]))
    ),
    reproduced_archive = case_when(
      is.na(value_script) ~ NA_integer_,
      is_inequality ~ as.integer(value_script < inequality_bound),
      .default = map2_int(value_script, row_number(),
                          function(v, i) agrees_loosely(v, value_paper[i], digits[i]))
    )
  )

# Table 1's two date cells are compared as strings, since a date is not a number.
date_comparison <-
  published_cells |>
  filter(str_detect(value_paper, "^\\d{4}-\\d{2}-\\d{2}$")) |>
  left_join(rewrite_table_1 |> select(float, row_key, quantity, value_text),
            by = c("float", "row_key", "quantity")) |>
  mutate(reproduced_rewrite = as.integer(value_paper == value_text))

cell_comparison <-
  cell_comparison |>
  rows_update(date_comparison |> select(float, row_key, quantity, reproduced_rewrite),
              by = c("float", "row_key", "quantity"))

float_summary <-
  cell_comparison |>
  group_by(float) |>
  summarize(
    published_numbers = n(),
    covered = sum(!is.na(reproduced_rewrite)),
    reproduced_by_rewrite = sum(reproduced_rewrite == 1, na.rm = TRUE),
    reproduced_by_archive = sum(reproduced_archive == 1, na.rm = TRUE),
    from_archive = sum(!is.na(reproduced_archive)),
    .groups = "drop"
  ) |>
  arrange(float, .locale = "en")

# Assembling the rows -----------------------------------------------------------

rows <- list()

gt_row <- function(claim_id, table_figure, claim, value_rewrite = NA_real_,
                  value_script = NA_real_, holds = NA, defect_locus = NA_character_,
                  note = NA_character_) {
  spec <- published_claims |> filter(.data$claim_id == .env$claim_id)
  stopifnot(nrow(spec) == 1)

  digits <- spec$digits
  units <- spec$units
  comparison <- if (is.na(spec$comparison)) "==" else spec$comparison

  paper_string <- spec$value_paper
  paper_numeric <- suppressWarnings(as.numeric(normalise_printed(paper_string)))

  converted_rewrite <- convert_units(value_rewrite, units)
  converted_script <- convert_units(value_script, units)

  verdict <- function(value, exact) {
    if (is.na(paper_numeric) || is.na(value)) return(NA_integer_)
    switch(
      comparison,
      "==" = if (exact) agrees_exactly(value, paper_string, digits)
             else agrees_loosely(value, paper_string, digits),
      "<" = as.integer(value < paper_numeric),
      "<=" = as.integer(value <= paper_numeric),
      ">" = as.integer(value > paper_numeric),
      ">=" = as.integer(value >= paper_numeric),
      "approx" = NA_integer_
    )
  }

  match_rewrite <- verdict(converted_rewrite, exact = TRUE)
  match_script <- verdict(converted_script, exact = FALSE)

  rendered <- if (is.na(converted_rewrite) || is.na(digits)) NA_character_ else
    render_at(converted_rewrite, digits)
  verdict_clause <- case_when(
    !is.na(match_rewrite) & match_rewrite == 1 ~
      str_glue("Rewrite gives {rendered}, which matches."),
    !is.na(match_rewrite) & match_rewrite == 0 ~
      str_glue("Rewrite gives {rendered} against a published {paper_string}."),
    !is.na(holds) & holds ~ "The claim holds.",
    !is.na(holds) & !holds ~ "The claim does not hold.",
    !is.na(rendered) ~ str_glue("Rewrite gives {rendered}; no verdict is taken."),
    .default = "No rewrite counterpart."
  )
  full_note <- if (is.na(note)) as.character(verdict_clause) else
    paste(note, verdict_clause)

  rows[[length(rows) + 1]] <<- tibble(
    paper_id = paper_id,
    claim_id = claim_id,
    table_figure = table_figure,
    claim = claim,
    value_script = converted_script,
    value_paper = paper_string,
    match = match_script,
    value_rewrite = converted_rewrite,
    match_rewrite = match_rewrite,
    holds = holds,
    defect_locus = defect_locus,
    notes = full_note
  )
  invisible(NULL)
}

# Accessors ----------------------------------------------------------------------

text_value <- function(name) {
  row <- text_claims |> filter(quantity == name)
  stopifnot(nrow(row) == 1)
  row$value
}

descriptive_holds <- function(name) {
  row <- descriptive_claims |> filter(quantity == name)
  stopifnot(nrow(row) == 1)
  row$holds
}

descriptive_evidence <- function(name) {
  row <- descriptive_claims |> filter(quantity == name)
  stopifnot(nrow(row) == 1)
  row$evidence
}

archive_text <- function(name) {
  row <- archive_values |> filter(float == "text", row_key == name)
  if (nrow(row) == 0) return(NA_real_)
  stopifnot(nrow(row) == 1)
  row$value_script
}

overall_estimate <- function(outcome_id, dataset_name, term_id,
                             specification_id = "no_moderators", column = "estimate") {
  row <- metareg_estimates |>
    filter(metareg_group == "overall", specification == specification_id,
           weighting == "unweighted", outcome == outcome_id,
           dataset == dataset_name, term == term_id)
  stopifnot(nrow(row) == 1)
  row[[column]]
}

cells_row <- function(float_id, key, quantity_id) {
  row <- cell_comparison |>
    filter(float == float_id, row_key == key, quantity == quantity_id)
  stopifnot(nrow(row) == 1)
  row
}

# Float-level rows: cells reproduced of cells ---------------------------------------

# Two floats do not reproduce in full, and each has an established cause, so
# neither is left to the default. Nothing else is defaulted either: a float that
# reproduces every cell carries no locus at all.
float_locus <- tribble(
  ~float, ~locus, ~explanation,
  "table_1", "archive",
  "The last study cell reads the last row of the data file rather than the latest date.",
  "figure_2", "rewrite",
  "Both inset labels are reproduced; the rewrite draws the two bars in the opposite vertical order from the published inset."
)

pwalk(float_summary, function(float, published_numbers, covered,
                              reproduced_by_rewrite, reproduced_by_archive,
                              from_archive) {
  claim_id <- str_c(float, "_cells")
  if (!claim_id %in% published_claims$claim_id) return(invisible(NULL))
  known <- float_locus |> filter(.data$float == .env$float)
  stopifnot(nrow(known) <= 1)
  adverse_float <- reproduced_by_rewrite < published_numbers
  if (adverse_float && nrow(known) == 0) {
    stop("Float ", float, " does not reproduce in full and has no recorded cause.")
  }
  gt_row(
    claim_id, float,
    str_glue("{float}: cells reproduced of published cells"),
    value_rewrite = reproduced_by_rewrite,
    value_script = if (from_archive > 0) reproduced_by_archive else NA_real_,
    defect_locus = if (adverse_float) known$locus else NA_character_,
    note = str_glue("{covered} of {published_numbers} published cells have a ",
                    "rewrite counterpart; {from_archive} have an archive counterpart.",
                    if (adverse_float) str_c(" ", known$explanation) else "")
  )
})

source(here::here("ground_truth", "claims_rows.R"), local = TRUE)

# Output -----------------------------------------------------------------------------

ground_truth <- list_rbind(rows)

missing_rows <- setdiff(published_claims$claim_id, ground_truth$claim_id)
extra_rows <- setdiff(ground_truth$claim_id, published_claims$claim_id)
if (length(missing_rows) > 0 || length(extra_rows) > 0) {
  print(list(missing = missing_rows, extra = extra_rows))
  stop("The ground truth and the extraction do not cover the same claims.")
}
stopifnot(!any(duplicated(ground_truth$claim_id)))

# The locus rule, in three states. An adverse row must carry a locus, a clean
# match must not, and a row with no verdict may.
adverse <- with(ground_truth,
                (!is.na(match) & match == 0) |
                  (!is.na(match_rewrite) & match_rewrite == 0) |
                  (!is.na(holds) & !holds))
clean <- with(ground_truth,
              !adverse & ((!is.na(match_rewrite) & match_rewrite == 1) |
                            (!is.na(holds) & holds)))
if (any(adverse & is.na(ground_truth$defect_locus))) {
  print(ground_truth |> filter(adverse & is.na(defect_locus)) |>
          select(claim_id, value_paper, value_rewrite, match, match_rewrite, holds),
        n = Inf)
  stop("An adverse row carries no defect_locus.")
}
if (any(clean & !is.na(ground_truth$defect_locus))) {
  print(ground_truth |> filter(clean & !is.na(defect_locus)) |>
          select(claim_id, value_paper, value_rewrite, match_rewrite, holds,
                 defect_locus), n = Inf)
  stop("A clean match carries a defect_locus.")
}
stopifnot(all(is.na(ground_truth$defect_locus) |
                ground_truth$defect_locus %in%
                c("paper_internal", "archive", "environment", "rewrite", "unresolved")))

# The coverage gate ------------------------------------------------------------------

# The second instrument is read as a program, not as text: it is run, its output
# is captured, and the printed claim lines are counted. A block that errors, or
# that prints nothing, satisfies a textual gate completely and fails this one.
# Its own environment, because both files necessarily read the same outputs and
# name objects for what they hold.
claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env(), echo = FALSE)
)

# The filter matches a claim line's whole shape rather than its prefix. in_text_claims.R now
# closes with excheckr's two CLAIM SUMMARY lines, which a prefix match also takes.
printed <- claims_output |>
  str_subset("^CLAIM [^ ]+ = .* \\|\\| ") |>
  str_match("^CLAIM ([^ ]+) = (.*?) \\|\\| (.*)$")
printed_claims <- tibble(
  claim_id = printed[, 2],
  printed_value = printed[, 3],
  label = printed[, 4]
)

required <- published_claims |> filter(needs_block)

missing_blocks <- setdiff(required$claim_id, printed_claims$claim_id)
unknown_blocks <- setdiff(printed_claims$claim_id, published_claims$claim_id)
if (length(missing_blocks) > 0 || length(unknown_blocks) > 0) {
  print(list(missing = missing_blocks, unknown = unknown_blocks))
  stop("in_text_claims.R does not print exactly the claims the extraction requires.")
}
if (nrow(printed_claims) != nrow(required)) {
  print(printed_claims |> count(claim_id) |> filter(n > 1))
  stop("in_text_claims.R printed ", nrow(printed_claims), " claims against ",
       nrow(required), " extraction rows requiring a block.")
}

# Cross-instrument comparison. The two files reach the same claimed number by
# separate paths from the same pipeline outputs; where they disagree, one of
# them is wrong.
cross <-
  printed_claims |>
  left_join(ground_truth |> select(claim_id, value_rewrite, holds), by = "claim_id") |>
  left_join(published_claims |> select(claim_id, digits, comparison, claim_type),
            by = "claim_id") |>
  mutate(
    # This has to move whenever the printed form on the other side moves, or the two
    # instruments disagree about a convention and it reads exactly like a finding.
    #
    # Two conventions changed when in_text_claims.R went onto excheckr::claim(). A
    # descriptive claim prints its truth value as 1 or 0 rather than TRUE or FALSE, which is
    # what the rest of the corpus prints. And a hedged claim now prints the number it
    # computes instead of NA, so the "approx" arm that skipped the comparison is gone: the
    # two instruments must still agree on the value even where neither compares it against
    # the article's hedge, and that is seven claims this gate had never checked.
    expected = pmap_chr(
      list(claim_type, holds, value_rewrite, digits),
      function(type, holds_value, value, digits) {
        if (type == "descriptive" && !is.na(holds_value)) {
          return(as.character(as.numeric(holds_value)))
        }
        if (is.na(value) || is.na(digits)) return(NA_character_)
        render_at(value, digits)
      }
    ),
    agrees = is.na(expected) | printed_value == expected
  )

if (!all(cross$agrees)) {
  print(cross |> filter(!agrees) |> select(claim_id, printed_value, expected), n = Inf)
  stop("The two instruments disagree about a claimed value.")
}

float_coverage <-
  float_summary |>
  transmute(float, published_numbers, covered, reproduced_by_rewrite,
            reproduced_by_archive = if_else(from_archive > 0, reproduced_by_archive,
                                            NA_integer_))

# Errata spine gate. errata.qmd names, for each published entry, the ground truth rows that
# entry corrects. An id that no longer exists is a typo or a renamed claim, and a dangling
# reference in a document whose whole purpose is correcting the record is worse than a build
# that refuses to finish.
errata_path <- here::here("errata_entries.csv")
if (file.exists(errata_path)) {
  errata_ids <- read_csv(errata_path, show_col_types = FALSE) |>
    pull(claim_ids) |>
    str_split(";") |>
    unlist() |>
    str_trim()
  errata_ids <- errata_ids[!is.na(errata_ids) & errata_ids != ""]
  dangling_errata_ids <- setdiff(errata_ids, ground_truth$claim_id)
  if (length(dangling_errata_ids) > 0) {
    stop("errata_entries.csv lists claim ids absent from the ground truth: ",
         paste(dangling_errata_ids, collapse = ", "))
  }
  print(str_glue("Errata spine: {length(unique(errata_ids))} distinct claim ids listed, ",
                 "all present in the ground truth."))
}

write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))
write_csv(ground_truth,
          here::here("ground_truth", str_glue("{paper_id}_ground_truth.csv")))

print(ground_truth |> count(match_rewrite, holds))
print(ground_truth |> filter(!is.na(defect_locus)) |> count(defect_locus))
print(float_coverage, n = Inf)
print(str_glue("{nrow(ground_truth)} ground truth rows; ",
               "{nrow(printed_claims)} claims printed by the second instrument; ",
               "{sum(float_coverage$published_numbers)} published cells transcribed."))
