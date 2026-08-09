# hewitt_etal_2024/run_all.R
# Runs the whole reproduction in order: fetch, verify and unpack the deposited
# archive, then estimate, then draw the figures and tables, then the in-text
# quantities, then the deposited archive as a program, then the ground truth and
# the second instrument, then the archive check again. Every script is
# self-contained and can be run on its own.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way and
# unpacks the container into original_extracted/.
source(here::here("download_original.R"))

# Estimation ----
# The advertisement-level treatment effects, then the meta-analysis of them.
# These are the only two scripts that fit anything.
source(here::here("maintained", "estimate_ates.R"))
source(here::here("maintained", "metaregressions.R"))

# Main text figures and tables ----
source(here::here("maintained", "table_1_studies_summary.R"))
source(here::here("maintained", "table_2_heterogeneity.R"))
source(here::here("maintained", "figure_1_raw_estimates.R"))
source(here::here("maintained", "figure_2_distribution.R"))
source(here::here("maintained", "figure_3_results_matrix.R"))
source(here::here("maintained", "figure_4_returns_to_exp.R"))

# Appendix figures ----
# figure_oa10 reads figure_3's cells, so it runs after it.
source(here::here("maintained", "figure_oa1_demographics.R"))
source(here::here("maintained", "figure_oa2_metaregression_coefficients.R"))
source(here::here("maintained", "figure_oa9_reliability.R"))
source(here::here("maintained", "figure_oa10_results_by_reliability.R"))
source(here::here("maintained", "figure_oa12_alternative_specifications.R"))

# In-text quantities ----
# Both read the figure and table outputs, so they run after them.
source(here::here("maintained", "text_main_claims.R"))
source(here::here("maintained", "text_descriptive_claims.R"))

# The deposited archive as a program ----
# Runs every deposited script in a scratch copy, as shipped and again stripped
# to data plus code, and records where each one stopped. Then reads out of the
# deposit every published quantity it can answer, which is what value_script in
# the ground truth is built from. Nothing is ever run inside original/ or
# original_extracted/.
#
# The as-shipped pass takes several hours, because two deposited scripts do. Set
# ARCHIVE_RUN_SKIP_SLOW=1 to skip those two and record them as skipped.
source(here::here("ground_truth", "run_archive.R"))
source(here::here("ground_truth", "extract_archive_values.R"))

# Ground truth ----
# Rebuilds the comparison table from the outputs above, so it cannot go stale.
# The build also runs in_text_claims.R under capture.output for the coverage
# gate; the call below is the human-readable pass.
source(here::here("ground_truth", "build_ground_truth.R"))
source(here::here("maintained", "in_text_claims.R"))

# Deposited archive, again ----
# The gate inside download_original.R is a precondition: sourcing it first
# proves original/ was intact when the run began and says nothing about what the
# run did to it. Re-sourcing it here is what catches a script that damaged the
# deposit mid-run.
source(here::here("download_original.R"))
