# hewitt_etal_2024/maintained/table_2_heterogeneity.R
# Output: output/table_2_heterogeneity.tex, output/table_2_heterogeneity.csv,
#   output/table_2_heterogeneity_cells.csv, output/table_oa2_dichotomized.tex,
#   output/table_oa2_dichotomized.csv, output/table_oa2_dichotomized_cells.csv
# Depends on: helpers.R, metaregressions.R output
# Description: Table 2, the mean effect and the standard deviation of the true
#   effects in each context, under three specifications; and Table OA2, the same
#   table computed on the dichotomised vote-choice scale.

source(here::here("maintained", "helpers.R"))

estimates <- read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE) |>
  filter(metareg_group == "overall", weighting == "unweighted")

# One row per published cell, unrounded ------------------------------------------

# mu and its interval come from the no-moderator model. tau appears three times,
# once per specification, with the Q-test p-value beside it.
mu_cells <-
  estimates |>
  filter(specification == "no_moderators", term == "intercept") |>
  select(outcome, dataset, estimate, std.error, conf.low, conf.high) |>
  pivot_longer(c(estimate, std.error, conf.low, conf.high),
               names_to = "quantity", values_to = "value") |>
  mutate(specification = "no_moderators", parameter = "mu")

tau_cells <-
  estimates |>
  filter(term == "sigma") |>
  select(outcome, dataset, specification, estimate, conf.low, conf.high, p.value) |>
  pivot_longer(c(estimate, conf.low, conf.high, p.value),
               names_to = "quantity", values_to = "value") |>
  mutate(parameter = "tau")

table_2_cells <-
  bind_rows(mu_cells, tau_cells) |>
  mutate(dataset = factor(dataset, levels = dataset_levels)) |>
  arrange(outcome, dataset, parameter, specification, quantity, .locale = "en") |>
  select(outcome, dataset, parameter, specification, quantity, value)

write_csv(table_2_cells |> filter(outcome != "votechoice_dichotomized"),
          out("table_2_heterogeneity_cells.csv"))
write_csv(table_2_cells |> filter(outcome == "votechoice_dichotomized"),
          out("table_oa2_dichotomized_cells.csv"))

# The published layout ------------------------------------------------------------

# The p-value column prints "< 0.001" below that threshold, which is what the
# article does; the unrounded value is in the cells file above.
format_p <- function(p) if_else(p < 0.001, "< 0.001", sprintf("%.3f", p))

build_display <- function(outcomes) {
  mu <- estimates |>
    filter(specification == "no_moderators", term == "intercept",
           outcome %in% outcomes) |>
    transmute(outcome, dataset,
              mu = sprintf("%.2f [%.2f, %.2f]", estimate, conf.low, conf.high))

  tau <- estimates |>
    filter(term == "sigma", outcome %in% outcomes) |>
    transmute(outcome, dataset, specification,
              tau = sprintf("%.2f [%.2f, %.2f]", estimate, conf.low, conf.high),
              p = format_p(p.value)) |>
    pivot_wider(names_from = specification, values_from = c(tau, p))

  join_one_to_one(mu, tau, by = c("outcome", "dataset")) |>
    mutate(
      outcome = factor(outcome, levels = c("votechoice", "favorability",
                                           "votechoice_dichotomized")),
      dataset = factor(dataset, levels = dataset_levels)
    ) |>
    arrange(outcome, dataset) |>
    transmute(
      Outcome = recode(as.character(outcome), votechoice = "Vote choice",
                       favorability = "Favorability",
                       votechoice_dichotomized = "Vote choice (dichotomized)"),
      Election = as.character(dataset),
      `$\\hat\\mu$` = mu,
      `$\\hat\\tau$` = tau_no_moderators,
      `$p$` = p_no_moderators,
      `$\\hat\\tau$ (race FE)` = tau_standard,
      `$p$ (race FE)` = p_standard,
      `$\\hat\\tau$ (study FE)` = tau_study_fixed_effects,
      `$p$ (study FE)` = p_study_fixed_effects
    )
}

table_2 <- build_display(c("votechoice", "favorability"))
table_oa2 <- build_display("votechoice_dichotomized")

write_csv(table_2, out("table_2_heterogeneity.csv"))
write_csv(table_oa2, out("table_oa2_dichotomized.csv"))

table_2 |>
  kable("latex", booktabs = TRUE, escape = FALSE, align = "llcccccc", linesep = "") |>
  kable_styling(latex_options = "scale_down") |>
  write_lines(out("table_2_heterogeneity.tex"))

table_oa2 |>
  select(-Outcome) |>
  kable("latex", booktabs = TRUE, escape = FALSE, align = "lcccccc", linesep = "") |>
  kable_styling(latex_options = "scale_down") |>
  write_lines(out("table_oa2_dichotomized.tex"))

print(table_2)
print(table_oa2)
