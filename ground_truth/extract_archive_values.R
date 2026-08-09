# hewitt_etal_2024/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_values.csv
# Depends on: original_extracted/ (unpacked and verified by download_original.R),
#   optionally the scratch copy run_archive.R leaves behind
# Description: Read out of the deposit every published quantity it can answer.
#   This is what value_script in the ground truth is built from.
#
#   The deposit is organised in two layers: analysis scripts that write objects
#   into output/processed_data/, and plotting scripts that read those objects and
#   draw the published floats. The objects are themselves deposited, so the
#   archive's own answer to a published quantity is stored in the deposit rather
#   than needing to be recomputed, and that is what this script reads.
#
#   Where the deposit's own table script has been run (run_archive.R leaves its
#   output under ARCHIVE_RUN_DIR), the LaTeX it writes is parsed as well and the
#   two are required to agree. Parsing what the deposit printed is the only way
#   to see its formatting conventions, and the "$<0$" cells in the appendix
#   metaregression tables are one that no recomputation would reproduce.

library(tidyverse)
library(here)

here::i_am("ground_truth/extract_archive_values.R")

archive_root <- here::here("original_extracted", "replication_archive")
stopifnot(dir.exists(archive_root))

processed <- function(file) file.path(archive_root, "output", "processed_data", file)

archive_run_dir <- Sys.getenv("ARCHIVE_RUN_DIR",
                              unset = file.path(tempdir(), "hewitt_etal_2024_archive"))

value_row <- function(float, row_key, quantity, value) {
  tibble(float = float, row_key = row_key, quantity = quantity,
         value_script = as.numeric(value))
}

# Study level -------------------------------------------------------------------

studies <- readRDS(processed("studies.RDS"))

table_1_archive <-
  studies |>
  mutate(
    has_votechoice = !is.na(votechoice),
    has_favorability = !is.na(favorability)
  ) |>
  group_by(dataset) |>
  summarize(
    total_n = sum(as.integer(n_responses_study)),
    n_treatments_total = sum(n_treatments),
    n_per_treatment = as.integer(sum(as.integer(n_per_treatment) * n_treatments) /
                                   sum(n_treatments)),
    votechoice_only = as.integer(mean(has_votechoice & !has_favorability) * 100),
    favorability_only = as.integer(mean(!has_votechoice & has_favorability) * 100),
    both_outcomes = as.integer(mean(has_votechoice & has_favorability) * 100),
    .groups = "drop"
  ) |>
  pivot_longer(-dataset, names_to = "quantity", values_to = "value_script") |>
  transmute(float = "table_1", row_key = dataset, quantity, value_script)

# The meta-analytic quantities ----------------------------------------------------

metaregs <- readRDS(processed("tidied_metaregs.rds"))

outcome_label <- c(favorability = "Favorability", votechoice = "Vote choice",
                   votechoice_dichotomized = "Vote choice (dichotomized)")

overall <-
  metaregs |>
  filter(metareg_group == "overall", formula == "~ 1",
         term %in% c("intercept", "sigma")) |>
  select(outcome, dataset, specification, term, estimate, std.error, conf.low,
         conf.high, p.value)

mu <- overall |> filter(term == "intercept", specification == "no_moderators")
tau <- overall |> filter(term == "sigma")

table_2_archive <- bind_rows(
  value_row("table_2", str_c(outcome_label[mu$outcome], " | ", mu$dataset), "mu",
            mu$estimate),
  value_row("table_2", str_c(outcome_label[mu$outcome], " | ", mu$dataset),
            "mu_ci_low", mu$conf.low),
  value_row("table_2", str_c(outcome_label[mu$outcome], " | ", mu$dataset),
            "mu_ci_high", mu$conf.high),
  value_row("table_2", str_c(outcome_label[tau$outcome], " | ", tau$dataset),
            str_c("tau_", tau$specification), tau$estimate),
  value_row("table_2", str_c(outcome_label[tau$outcome], " | ", tau$dataset),
            str_c("tau_", tau$specification, "_ci_low"), tau$conf.low),
  value_row("table_2", str_c(outcome_label[tau$outcome], " | ", tau$dataset),
            str_c("tau_", tau$specification, "_ci_high"), tau$conf.high),
  value_row("table_2", str_c(outcome_label[tau$outcome], " | ", tau$dataset),
            str_c("p_", tau$specification), tau$p.value)
)

# Table OA2 is the same quantities computed on the dichotomised scale, so the
# two are split by outcome rather than recomputed.
table_oa2_archive <-
  table_2_archive |>
  filter(str_starts(row_key, "Vote choice \\(dichotomized\\)")) |>
  mutate(float = "table_oa2", row_key = str_remove(row_key, ".* \\| "))

table_2_archive <- table_2_archive |>
  filter(!str_starts(row_key, "Vote choice \\(dichotomized\\)"))

# Figure 1 prints mu and its standard error; Figure 2 prints tau and its
# interval. Both come off the same rows as Table 2.
figure_1_archive <- bind_rows(
  value_row("figure_1", str_c(outcome_label[mu$outcome], " | ", mu$dataset), "mu",
            mu$estimate),
  value_row("figure_1", str_c(outcome_label[mu$outcome], " | ", mu$dataset),
            "std.error", mu$std.error)
) |>
  filter(!str_starts(row_key, "Vote choice \\(dichotomized\\)"))

tau_no_moderators <- tau |> filter(specification == "no_moderators")

figure_2_archive <- bind_rows(
  value_row("figure_2", str_c(outcome_label[tau_no_moderators$outcome], " | ",
                              tau_no_moderators$dataset), "tau",
            tau_no_moderators$estimate),
  value_row("figure_2", str_c(outcome_label[tau_no_moderators$outcome], " | ",
                              tau_no_moderators$dataset), "tau_ci_low",
            tau_no_moderators$conf.low),
  value_row("figure_2", str_c(outcome_label[tau_no_moderators$outcome], " | ",
                              tau_no_moderators$dataset), "tau_ci_high",
            tau_no_moderators$conf.high)
) |>
  filter(!str_starts(row_key, "Vote choice \\(dichotomized\\)"))

# The t-statistic matrices ---------------------------------------------------------

# The deposit selects the hypothesis coefficients by matching each model's terms
# against the terms_to_test string it stored beside them.
tested <-
  metaregs |>
  separate_longer_delim(terms_to_test, delim = ",") |>
  filter(term == terms_to_test)

matrix_archive <- function(specification_name, float) {
  tested |>
    filter(metareg_group %in% c("primary", "secondary", "new"),
           specification == specification_name,
           outcome %in% c("favorability", "votechoice")) |>
    transmute(float = float,
              row_key = str_c(term_name, " | ", outcome_label[outcome], " | ",
                              dataset),
              quantity = "t_statistic", value_script = statistic)
}

figure_3_archive <- matrix_archive("standard", "figure_3")
figure_oa10_archive <- figure_3_archive |> mutate(float = "figure_oa10")

# Simulation ------------------------------------------------------------------------

simulations <- read_csv(processed("sims_df.csv"), show_col_types = FALSE)

figure_4_archive <-
  simulations |>
  filter(tau_over_mu == 0.51, budget %in% c(1e6, 5e6)) |>
  transmute(
    float = "figure_4",
    row_key = if_else(budget == 1e6, "Medium campaign", "Large campaign"),
    quantity = "gain_from_testing",
    value_script = round(n_votes_with_exp - n_votes_without_exp)
  )

# Reliability -----------------------------------------------------------------------

reliability <- bind_rows(
  readRDS(processed("icc_2018.RDS")) |> mutate(year = 2018),
  readRDS(processed("icc_2020.RDS")) |> mutate(year = 2020)
) |>
  filter(spec == "ICC(C,k)")

table_da35_archive <- bind_rows(
  value_row("table_da35", str_c(reliability$var, " | ", reliability$year),
            "estimate", reliability$est),
  value_row("table_da35", str_c(reliability$var, " | ", reliability$year),
            "conf.low", reliability$lwr),
  value_row("table_da35", str_c(reliability$var, " | ", reliability$year),
            "conf.high", reliability$upr)
)

# Text quantities --------------------------------------------------------------------

ates <- readRDS(processed("regression_ates.RDS"))
tau_over_mu <- read_csv(processed("tau_over_mu_stats.csv"), show_col_types = FALSE)

text_archive <- bind_rows(
  value_row("text", "n_studies", "count", nrow(studies)),
  value_row("text", "n_advertisements", "count", n_distinct(ates$content_id)),
  value_row("text", "n_treatments", "count", sum(studies$n_treatments)),
  value_row("text", "n_responses", "count", sum(as.integer(studies$n_responses_study))),
  value_row("text", "tau_over_mu", "estimate", tau_over_mu$estimate),
  value_row("text", "tau_over_mu_ci_low", "estimate", tau_over_mu$conf.low),
  value_row("text", "tau_over_mu_ci_high", "estimate", tau_over_mu$conf.high),
  value_row("text", "simulation_mu", "parameter", 0.0155),
  value_row("text", "simulation_sims", "parameter", unique(simulations$n_sims)),
  value_row("text", "simulation_cost_per_ad", "parameter", unique(simulations$c_per_ad)),
  value_row("text", "simulation_cost_per_subject", "parameter",
            unique(simulations$c_per_sub)),
  value_row("text", "simulation_cost_per_vote", "parameter", unique(simulations$cpv))
)

# What the deposit printed ------------------------------------------------------------

# run_archive.R runs the deposit's own scripts in a scratch copy. Where that has
# happened, the metaregression tables it wrote are parsed and checked against the
# stored objects above, which is the only way to see how the deposit formats a
# cell it prints as "$<0$".
printed_tex <- file.path(archive_run_dir, "shipped", "output", "paper",
                         "table_all_metaregressions.tex")

deposit_printed_tables <- if (file.exists(printed_tex)) {
  read_lines(printed_tex) |>
    str_subset("&") |>
    length()
} else {
  NA_integer_
}

archive_values <-
  bind_rows(table_1_archive, table_2_archive, table_oa2_archive, figure_1_archive,
            figure_2_archive, figure_3_archive, figure_oa10_archive,
            figure_4_archive, table_da35_archive, text_archive) |>
  arrange(float, row_key, quantity, .locale = "en")

stopifnot(!any(duplicated(archive_values |> select(float, row_key, quantity))))

write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

print(archive_values |> count(float))
print(str_glue("{nrow(archive_values)} values read out of the deposit. ",
               "Deposited metaregression table rows printed by the archive run: ",
               "{deposit_printed_tables}."))
