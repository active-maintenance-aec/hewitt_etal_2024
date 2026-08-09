# hewitt_etal_2024/maintained/estimate_ates.R
# Output: output/ate_estimates.csv, clean_data/ate_fits.rds
# Depends on: helpers.R, original_extracted/.../responses.RDS, studies.RDS
# Description: Estimate the average treatment effect of every advertisement, by
#   regressing each outcome on treatment indicators and the pre-treatment
#   covariates the study measured. This is the first of the article's two
#   analysis steps; the meta-analysis of these estimates is the second.
#
#   The fitted objects are kept because the meta-analysis needs the full
#   variance-covariance matrix of each study's estimates, not only the standard
#   errors: treatments within a study share a placebo group, so their estimates
#   are correlated and the meta-analytic models take a block-diagonal V. They go
#   to clean_data/ rather than output/ because the collection is 65 MB, which is
#   not something to commit; output/ carries the estimates themselves.

source(here::here("maintained", "helpers.R"))

studies <- read_deposit_rds("studies.RDS")
responses <- read_deposit_rds("responses.RDS") |>
  filter(drop == 0)

outcomes <- c("favorability", "votechoice", "votechoice_dichotomized")

# Covariate categories with fewer than 20 respondents are pooled, and any level
# left with a single respondent is pooled again, because a singleton level makes
# the HC2 standard error undefined.
lump_small_levels <- function(x) {
  x <- fct_lump_min(x, 20)
  if (all(table(x) != 1)) return(x)
  fct_lump_n(x, sum(table(x) > 1) - 1)
}

# One study, one outcome. Returns NULL where the study did not ask the outcome.
# Standard errors are CR2 clustered on the assignment cluster where the study
# has one (the 2018 design assigned treatments to time-based clusters) and HC2
# otherwise, which is what the article describes and what the deposit does.
fit_study <- function(study_id_, outcome_id) {
  d <- responses |> filter(study_id == study_id_)
  if (all(is.na(d[[outcome_id]]))) return(NULL)

  clusters <- d$cluster_id
  if (all(is.na(clusters))) clusters <- NULL

  covariates <- d |>
    select(starts_with("covariates.")) |>
    mutate(across(where(is.character), lump_small_levels)) |>
    select(where(~ !any(is.na(.)) & n_distinct(.) > 1)) |>
    colnames()

  d <- d |> mutate(across(all_of(covariates) & where(is.character), lump_small_levels))

  formula <- as.formula(
    paste(outcome_id, "~", str_flatten(c("content_id", covariates), collapse = " + "))
  )

  fit <- lm_robust(formula, data = d, clusters = clusters)
  fit$n_obs <- c(table(as.character(d$content_id)))
  fit
}

fits <- map(set_names(outcomes), function(outcome_id) {
  map(set_names(studies$study_id), function(study_id_) fit_study(study_id_, outcome_id)) |>
    discard(is.null)
})

dir.create(here::here("maintained", "clean_data"), showWarnings = FALSE)
write_rds(fits, here::here("maintained", "clean_data", "ate_fits.rds"))

ate_estimates <-
  imap(fits, function(outcome_fits, outcome_id) {
    imap(outcome_fits, function(fit, study_id_) {
      tidy(fit) |>
        filter(str_starts(term, "content_id")) |>
        mutate(
          study_id = study_id_,
          outcome = outcome_id,
          content_id = str_extract(term, "(?<=content_id).*"),
          n_obs = fit$n_obs[content_id]
        )
    }) |>
      list_rbind()
  }) |>
  list_rbind() |>
  select(study_id, content_id, outcome, estimate, std.error, statistic, p.value,
         conf.low, conf.high, df, n_obs) |>
  arrange(outcome, study_id, content_id, .locale = "en")

stopifnot(
  nrow(ate_estimates) == nrow(distinct(ate_estimates, outcome, study_id, content_id)),
  all(!is.na(ate_estimates$estimate)),
  all(ate_estimates$n_obs > 0)
)

write_csv(ate_estimates, out("ate_estimates.csv"))

print(ate_estimates |> count(outcome))
print(str_glue("{nrow(ate_estimates)} advertisement-level treatment effects across ",
               "{n_distinct(ate_estimates$study_id)} studies and ",
               "{n_distinct(ate_estimates$content_id)} advertisements."))
