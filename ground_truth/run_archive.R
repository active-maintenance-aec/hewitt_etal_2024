# hewitt_etal_2024/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_overwrites.csv,
#   ground_truth/archive_write_calls.csv
# Depends on: original_extracted/ (unpacked and verified by download_original.R)
# Description: Run every deposited script in a scratch copy of the archive and
#   record where each one stopped. The deposit is run twice, once exactly as
#   shipped and once stripped to data plus code, so that a script passing only
#   because the deposit ships the object it reads is visible as a failure. The
#   scratch copy also answers a question the checksum gate cannot: which
#   deposited files the deposit's own code writes over when it is run in place.
#   Nothing is ever run inside original/ or original_extracted/.
#
#   The as-shipped pass takes several hours. 4_metaregressions.R and
#   sims_for_optimal_spend.R are the long ones, at roughly two and three hours
#   respectively on the machine this was written on, and both parallelise across
#   twelve workers. Set ARCHIVE_RUN_SKIP_SLOW=1 to skip every script the deposit
#   documents as taking more than ten minutes; the skipped rows are recorded as
#   "skipped" rather than silently dropped.

library(tidyverse)
library(here)

here::i_am("ground_truth/run_archive.R")

archive_run_dir <- Sys.getenv("ARCHIVE_RUN_DIR",
                              unset = file.path(tempdir(), "hewitt_etal_2024_archive"))
stopifnot(nzchar(archive_run_dir))

skip_slow <- Sys.getenv("ARCHIVE_RUN_SKIP_SLOW", unset = "0") == "1"

archive_root <- here::here("original_extracted", "replication_archive")
stopifnot(dir.exists(archive_root))

members <- read_csv(here::here("original_members_manifest.csv"), show_col_types = FALSE) |>
  filter(str_starts(member, "replication_archive/")) |>
  mutate(member = str_remove(member, "^replication_archive/"))

# The deposited scripts, in the order README.R and make_all_plots_and_tables.R
# introduce them: the analysis layer that builds output/processed_data/, then
# the plotting layer that reads it. 1_clean_source_data.R is included even
# though the deposit says its inputs are withheld, because "documented as
# unrunnable" and "unrunnable" are different claims and only one of them has
# been measured.
analysis_scripts <- c(
  "analysis_scripts/1_clean_source_data.R",
  "analysis_scripts/2_regressions.R",
  "analysis_scripts/4_metaregressions.R",
  "analysis_scripts/randomization_checks/2_missingness.R",
  "analysis_scripts/randomization_checks/3_balance.R",
  "analysis_scripts/sims_for_optimal_spend.R"
)

plotting_scripts <- str_c("plotting_scripts/", c(
  "Table1_studies_summary", "Table2_effects_summary", "Figure1_raw_estimates",
  "Figure2_distribution", "Figure3_results_matrix", "Figure4_returns_to_exp",
  "appendix_metaregressions", "appendix_attrition", "appendix_balance",
  "appendix_reliability", "appendix_robustness", "appendix_demographics",
  "appendix_tau_from_experiment_100_times",
  "appendix_in_cycle_generalization/treatment_corrs", "appendix_features",
  "appendix_figure_OA14", "appendix_bad_beliefs", "appendix_sd_simulation"
), ".R")

deposited_scripts <- c(analysis_scripts, plotting_scripts)
stopifnot(all(deposited_scripts %in% members$member))

# Runtimes the deposit itself documents in README.R. Used only to decide what
# ARCHIVE_RUN_SKIP_SLOW skips, never reported as a measurement.
slow_scripts <- c(
  "analysis_scripts/4_metaregressions.R",
  "analysis_scripts/sims_for_optimal_spend.R",
  "plotting_scripts/appendix_metaregressions.R"
)

# What the deposit writes ----
# Read before running: an archive that writes nothing cannot produce an artifact
# anyone could diff, which decides whether a comparison against the deposit is
# possible at all. Comment lines are stripped first, since a commented-out write
# is a claim about the published artifact and not a write.
write_calls <-
  tibble(script = deposited_scripts) |>
  mutate(line = map(script, function(s) {
    lines <- read_lines(file.path(archive_root, s))
    tibble(line_number = seq_along(lines), text = str_squish(lines))
  })) |>
  unnest(line) |>
  filter(!str_starts(text, "#")) |>
  filter(str_detect(text, "ggsave\\(|write_csv\\(|write_lines\\(|write_rds\\(|saveRDS\\(|sink\\(|write\\(file")) |>
  mutate(writes_to = str_extract(text, '(?<=")[^"]*\\.(pdf|png|tex|csv|rds|RDS|RData)(?=")')) |>
  filter(!str_detect(text, "read_rds|read_csv|read\\.csv|readRDS")) |>
  select(script, line_number, writes_to, text) |>
  arrange(script, line_number)

write_csv(write_calls, here::here("ground_truth", "archive_write_calls.csv"))

# Is the stripped pass distinguishable from the as-shipped pass? It is only if
# some deposited file is itself written by a deposited script. Asserted rather
# than assumed: "the deposit ships no derived objects" and "every script failed
# before reaching a write" are the same measurement and opposite findings. Here
# the whole of output/processed_data/ is derived, and the script that would
# rebuild the three objects at the bottom of the chain reads source data the
# deposit does not carry.
derived_members <- write_calls |>
  filter(!is.na(writes_to)) |>
  pull(writes_to) |>
  unique() |>
  intersect(members$member)

scrub <- function(x) {
  x |>
    str_replace_all("(/private)?/+[^ '\"]*?/(shipped|stripped)/+", "<scratch>/") |>
    str_replace_all("(/private)?/+[^ '\"]*?hewitt_etal_2024_archive/*", "<scratch>/")
}

make_copy <- function(dest, strip) {
  unlink(dest, recursive = TRUE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.copy(archive_root, dirname(dest), recursive = TRUE)
  file.rename(file.path(dirname(dest), "replication_archive"), dest)
  if (strip && length(derived_members) > 0) unlink(file.path(dest, derived_members))
  invisible(dest)
}

# Run one script in its own R session, from the copy's root, and record where it
# stopped. The timeout is tested before the error text: a killed script's dying
# message is not the reason it stopped, and filing it as one turns a resource
# limit into a code defect.
run_script <- function(script, pass, timeout = 21600) {
  if (skip_slow && script %in% slow_scripts) {
    return(tibble(pass = pass, script = script, exit_status = NA_integer_,
                  outcome = "skipped", stopped_at = NA_character_, log = ""))
  }
  started <- Sys.time()
  result <- system2("Rscript", c("--vanilla", shQuote(script)),
                    stdout = TRUE, stderr = TRUE, timeout = timeout)
  status <- attr(result, "status")
  status <- if (is.null(status)) 0L else as.integer(status)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  timed_out <- status == 124L || elapsed >= timeout
  error_line <- result[str_detect(result, "^Error")] |> head(1)
  tibble(
    pass = pass,
    script = script,
    exit_status = status,
    outcome = case_when(
      timed_out ~ "timeout",
      status == 0 ~ "clean",
      .default = "error"
    ),
    stopped_at = if (length(error_line) == 0) NA_character_ else scrub(str_squish(error_line)),
    log = paste(result, collapse = "\n")
  )
}

run_pass <- function(dir, scripts, pass) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  map(scripts, function(s) {
    print(str_glue("[{pass}] {s}"))
    run_script(s, pass)
  }) |> list_rbind()
}

# As shipped ----
shipped_dir <- file.path(archive_run_dir, "shipped")
make_copy(shipped_dir, strip = FALSE)

deposited <- members$member
mtime_before <- file.mtime(file.path(shipped_dir, deposited))

shipped <- run_pass(shipped_dir, deposited_scripts, "as_shipped")

mtime_after <- file.mtime(file.path(shipped_dir, deposited))
overwritten <- deposited[!is.na(mtime_after) & mtime_after > mtime_before]
added <- setdiff(
  list.files(shipped_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
  deposited
)

overwrites <- bind_rows(
  tibble(file = overwritten, effect = "overwritten"),
  tibble(file = added, effect = "added")
) |>
  arrange(effect, file, .locale = "en")

write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

# Stripped to data plus code ----
stripped_dir <- file.path(archive_run_dir, "stripped")
make_copy(stripped_dir, strip = TRUE)
stripped <- run_pass(stripped_dir, deposited_scripts, "stripped")

status <- bind_rows(shipped, stripped)

walk(which(status$outcome %in% c("error", "timeout")), function(i) {
  print(str_glue("[{status$pass[i]}] {status$script[i]}: {status$outcome[i]}"))
  print(status$stopped_at[i])
})

# The log carries scratch paths and is a property of the run, so it is printed
# and never committed.
status |>
  select(pass, script, exit_status, outcome, stopped_at) |>
  write_csv(here::here("ground_truth", "archive_run_status.csv"))

print(str_glue("Deposited files a deposited script writes over: {length(derived_members)}. ",
               "The stripped pass is the as-shipped pass where that count is zero, ",
               "and the deposit then ships no derived objects."))
print(status |> count(pass, outcome))
print(str_glue("Scratch copy: {archive_run_dir}"))
