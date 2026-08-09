# hewitt_etal_2024/maintained/helpers.R
# Output: none (sourced by every script in maintained/)
# Depends on: original_extracted/ (unpacked and verified by download_original.R)
# Description: Packages, paths and the small shared vocabulary the analysis and
#   figure scripts need. This file defines functions and loads no data, so that
#   sourcing it has no effect beyond making the helpers available.

library(here)
library(tidyverse)
library(estimatr)
library(broom)
library(metafor)
library(patchwork)
library(cowplot)
library(ggtext)
# scales::discard masks purrr::discard, which is used to drop the studies that
# did not ask a given outcome.
library(scales, exclude = "discard")
library(knitr)
library(kableExtra)

here::i_am("maintained/helpers.R")

# Paths -----------------------------------------------------------------------

# The deposit is a single zip. download_original.R unpacks it into
# original_extracted/ and verifies all 186 members, so everything downstream
# reads from there and nothing ever reads or writes inside original/.
deposit <- function(...) {
  here::here("original_extracted", "replication_archive", ...)
}

processed <- function(file) {
  deposit("output", "processed_data", file)
}

out <- function(file) {
  here::here("maintained", "output", file)
}

# The deposited names are mixed case (studies.RDS, tidied_metaregs.rds) and the
# deposit's own scripts read several of them in the wrong case, which works on a
# case-insensitive filesystem and fails on a case-sensitive one. Reads here use
# the deposited spelling exactly.
read_deposit_rds <- function(file) {
  path <- processed(file)
  stopifnot(file.exists(path))
  readRDS(path)
}

# Vocabulary ------------------------------------------------------------------

outcome_colors <- c(
  "votechoice" = "blue4",
  "favorability" = "purple3",
  "votechoice_dichotomized" = "gray50"
)

dataset_levels <- c("2018", "2020 Downballot", "2020 Presidential")

# The hypothesis groups Figure 3 stacks vertically, in the article's order.
metareg_group_levels <- c("2018 Primary hypotheses", "2018 Secondary hypotheses",
                          "2020 New hypotheses")

theme_hewitt <- function() {
  theme_bw() +
    theme(
      strip.background = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# A figure whose axis reads 300, 100, 30, 10, 3 from left to right is a reversed
# log axis. The deposit registers this transformation through scales' older
# string interface; the current constructor is used here.
reverselog_trans <- function(base = 10) {
  scales::new_transform(
    name = paste0("reverselog-", format(base)),
    transform = function(x) -log(x, base),
    inverse = function(x) base^(-x),
    breaks = function(x) rev(scales::log_breaks(n = 5, base = base)(x)),
    format = scales::label_number(accuracy = 1),
    domain = c(1e-100, Inf)
  )
}

# Saving ----------------------------------------------------------------------

# Every figure is written twice, as a vector PDF for the report and a raster PNG
# for the web, and every figure script also writes the numbers it plots.
save_figure <- function(plot, stem, width, height) {
  ggsave(out(paste0(stem, ".pdf")), plot = plot, width = width, height = height)
  ggsave(out(paste0(stem, ".png")), plot = plot, width = width, height = height,
         dpi = 300)
  invisible(NULL)
}

# Comparison ------------------------------------------------------------------

# A join between two pipeline outputs, or between a published transcription and
# a pipeline output, must be one to one on both sides. Every join in this
# repository goes through here.
join_one_to_one <- function(x, y, by) {
  stopifnot(
    !any(duplicated(x[by])),
    !any(duplicated(y[by]))
  )
  joined <- dplyr::inner_join(x, y, by = by, relationship = "one-to-one")
  stopifnot(nrow(joined) == nrow(x), nrow(joined) == nrow(y))
  joined
}

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}
