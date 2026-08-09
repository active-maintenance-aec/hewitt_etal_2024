# hewitt_etal_2024/ground_truth/parse_published_values.R
# Output: ground_truth/published_maintext_tables.csv,
#   ground_truth/published_appendix_values.csv
# Depends on: the published article, its APSR online appendix and its Dataverse
#   online appendix, none of which this repository redistributes.
# Description: Transcribe the published tables, and the figures that print their
#   numbers on their faces, positionally rather than in reading order. The two
#   CSVs this writes are committed, so nothing downstream needs the PDFs; run
#   this only to regenerate the transcription.
#
#   Point PAPER_PDF_DIR at a directory holding
#     hewitt_etal_2024.pdf
#     hewitt_etal_2024_online_appendix.pdf
#     hewitt_etal_2024_dataverse_appendix.pdf
#   The article is at doi:10.1017/S0003055423001387 and both appendices are in
#   its supplementary material.
#
#   pdftotext artifacts specific to these three documents, checked against
#   rendered pages: in the article's math mode the decimal point comes through
#   as a colon (0:001 for 0.001) and square brackets as the fraction glyph
#   (½1:60, 2:94 for [1.60, 2.94]); the minus sign is U+2212 throughout; tau
#   arrives as a bare "ˆ" in the appendix tables' variance rows and as "⌧" in
#   the APSR appendix; and R-squared arrives as "R̂2" with a combining circumflex.

library(tidyverse)
library(here)

here::i_am("ground_truth/parse_published_values.R")

pdf_dir <- Sys.getenv("PAPER_PDF_DIR", unset = "~/Dropbox/works/catalog/hewitt_etal_2024/original_materials")
pdf_dir <- path.expand(pdf_dir)
stopifnot(dir.exists(pdf_dir))

pdf_text_layout <- function(file) {
  path <- file.path(pdf_dir, file)
  stopifnot(file.exists(path))
  destination <- tempfile(fileext = ".txt")
  status <- system2("pdftotext", c("-layout", shQuote(path), shQuote(destination)))
  stopifnot(status == 0)
  # A page break arrives as a form feed glued to the first character of the next
  # line, which silently defeats every anchored pattern below.
  read_lines(destination) |> str_remove_all("\f")
}

paper <- pdf_text_layout("hewitt_etal_2024.pdf")
online_appendix <- pdf_text_layout("hewitt_etal_2024_online_appendix.pdf")
dataverse_appendix <- pdf_text_layout("hewitt_etal_2024_dataverse_appendix.pdf")

# Typography, not arithmetic. The house rule is a leading zero everywhere, the
# Unicode minus is not a hyphen, and a thousands separator is not a digit.
normalise_printed <- function(x) {
  x |>
    str_replace_all("\u2212|\u2013|\u2014", "-") |>
    str_remove_all(",") |>
    str_replace("^(-?)\\.", "\\10.") |>
    # A cell printed -0.0 is the same claim as one printed 0.0, and the signed
    # zero has to be normalised here rather than only where the pipeline value
    # is rendered: fifty appendix cells arrive this way.
    str_replace("^-(0(\\.0+)?)$", "\\1")
}

printed_digits <- function(x) {
  if_else(str_detect(x, "\\."), str_length(str_extract(x, "(?<=\\.)\\d+")), 0L)
}

# Article math mode renders the decimal point as a colon and brackets as ½.
demath <- function(x) {
  x |>
    str_replace_all("\u00bd", "[") |>
    str_replace_all("\u0003", "]") |>
    str_replace_all("(?<=\\d):(?=\\d)", ".")
}

cells <- function(float, row_key, quantity, value) {
  value <- normalise_printed(value)
  tibble(float = float, row_key = row_key, quantity = quantity,
         value_paper = value, digits = printed_digits(value))
}

# Main text Table 1 --------------------------------------------------------------

table_1_lines <- paper |>
  str_subset("^\\s*(2018|2020 downballot|2020 presidential)\\s+20\\d\\d-\\d\\d-\\d\\d")

stopifnot(length(table_1_lines) == 3)

table_1 <-
  table_1_lines |>
  map(function(line) {
    parts <- str_match(
      str_squish(line),
      "^(2018|2020 downballot|2020 presidential) (\\S+) (\\S+) ([\\d,]+) (\\d+) (\\d+) (\\d+)% (\\d+)% (\\d+)%$"
    )
    stopifnot(!is.na(parts[1, 1]))
    dataset <- recode(parts[1, 2], "2020 downballot" = "2020 Downballot",
                      "2020 presidential" = "2020 Presidential")
    quantities <- c("first_study", "last_study", "total_n", "n_treatments_total",
                    "n_per_treatment", "votechoice_only", "favorability_only",
                    "both_outcomes")
    cells("table_1", dataset, quantities, parts[1, 3:10])
  }) |>
  list_rbind()

# Main text Table 2 --------------------------------------------------------------

# Each published row is two printed lines: the point estimates, then the
# intervals beneath them.
table_2_starts <- which(str_detect(
  paper, "^\\s*(Vote choice|Favorability)\\s+(2018|2020 downballot|2020 presidential)\\s"
))
stopifnot(length(table_2_starts) == 6)

table_2 <-
  table_2_starts |>
  map(function(i) {
    top <- str_squish(demath(paper[i]))
    bottom <- str_squish(demath(paper[i + 1]))
    head <- str_match(top,
      "^(Vote choice|Favorability) (2018|2020 downballot|2020 presidential) (.*)$")
    numbers <- str_split(head[1, 4], " ")[[1]] |>
      str_replace_all("< 0", "<0") |>
      discard(~ .x == "<")
    numbers <- str_squish(head[1, 4]) |>
      str_replace_all("< 0\\.001", "<0.001") |>
      str_split(" ") |>
      pluck(1)
    stopifnot(length(numbers) == 7)
    intervals <- str_match_all(bottom, "\\[([^,]+), ([^\\]]+)\\]")[[1]]
    stopifnot(nrow(intervals) == 4)

    outcome <- head[1, 2]
    election <- recode(head[1, 3], "2020 downballot" = "2020 Downballot",
                       "2020 presidential" = "2020 Presidential")
    row_key <- paste(outcome, election, sep = " | ")

    bind_rows(
      cells("table_2", row_key,
            c("mu", "tau_no_moderators", "p_no_moderators", "tau_standard",
              "p_standard", "tau_study_fixed_effects", "p_study_fixed_effects"),
            numbers),
      cells("table_2", row_key,
            c("mu_ci_low", "mu_ci_high", "tau_no_moderators_ci_low",
              "tau_no_moderators_ci_high", "tau_standard_ci_low",
              "tau_standard_ci_high", "tau_study_fixed_effects_ci_low",
              "tau_study_fixed_effects_ci_high"),
            as.vector(t(intervals[, 2:3])))
    )
  }) |>
  list_rbind()

# Main text Table 3 --------------------------------------------------------------

table_3 <- bind_rows(
  cells("table_3", "Typical advertising effect", "dollars_per_vote", "200"),
  cells("table_3", "Cost of producing one ad", "dollars", "15000"),
  cells("table_3", "Cost of ad testing", "dollars_per_subject", "2.50")
)

# Main text Figure 1 --------------------------------------------------------------

# The meta-analytic strip prints mu at one decimal with its standard error
# beneath, reading down the page: favorability then vote choice within each of
# the three panels.
figure_1_values <- tribble(
  ~row_key, ~mu, ~se,
  "Favorability | 2018", "2.6", "0.3",
  "Vote choice | 2018", "2.3", "0.3",
  "Favorability | 2020 Downballot", "1.4", "0.2",
  "Vote choice | 2020 Downballot", "1.2", "0.2",
  "Favorability | 2020 Presidential", "1.0", "0.1",
  "Vote choice | 2020 Presidential", "0.8", "0.1"
)

figure_1 <- bind_rows(
  cells("figure_1", figure_1_values$row_key, "mu", figure_1_values$mu),
  cells("figure_1", figure_1_values$row_key, "std.error", figure_1_values$se)
)

# Main text Figure 2 --------------------------------------------------------------

figure_2_values <- tribble(
  ~row_key, ~tau, ~low, ~high,
  "Favorability | 2018", "1.67", "1.29", "2.11",
  "Vote choice | 2018", "1.42", "0.90", "1.99",
  "Favorability | 2020 Downballot", "0.85", "0.50", "1.20",
  "Vote choice | 2020 Downballot", "0.47", "0.00", "0.85",
  "Favorability | 2020 Presidential", "0.44", "0.29", "0.58",
  "Vote choice | 2020 Presidential", "0.34", "0.14", "0.50"
)

figure_2 <- bind_rows(
  cells("figure_2", figure_2_values$row_key, "tau", figure_2_values$tau),
  cells("figure_2", figure_2_values$row_key, "tau_ci_low", figure_2_values$low),
  cells("figure_2", figure_2_values$row_key, "tau_ci_high", figure_2_values$high),
  cells("figure_2", "Two largest treatment effects | first", "n_obs", "403"),
  cells("figure_2", "Two largest treatment effects | second", "n_obs", "534")
)

# The t-statistic matrices: Figure 3, Figure OA10, Figure OA12 --------------------

# A figure that prints numbers on its face is a published table in disguise, so
# these are read positionally: one hypothesis per line, its cells in column
# order, with the significance stars kept as part of the cell.
parse_matrix_rows <- function(lines, hypotheses, n_columns) {
  hypotheses |>
    map(function(hypothesis) {
      pattern <- str_c("^\\s*", str_replace_all(hypothesis, "([\\^$.|?*+()\\[\\]{}])", "\\\\\\1"),
                       "\\s+((?:[-\u2212]?\\d+\\.\\d+\\*{0,2}\\s+)*[-\u2212]?\\d+\\.\\d+\\*{0,2})\\s*$")
      matched <- str_subset(lines, pattern)
      if (length(matched) == 0) return(tibble())
      matched |>
        map(function(line) {
          values <- str_match(line, pattern)[1, 2] |> str_squish() |> str_split(" ") |> pluck(1)
          tibble(hypothesis = hypothesis, position = seq_along(values), cell = values,
                 n_values = length(values))
        }) |>
        list_rbind()
    }) |>
    list_rbind() |>
    filter(n_values %in% n_columns)
}

matrix_hypotheses <- c(
  "Candidate facts", "New fact (where fact present)", "Policy facts",
  "Primary focus: Candidate", "Primary focus: Issues",
  "Technique: Negative name\u2212calling", "Technique: Negative testimonial",
  "Technique: Negative transfer of association", "Technique: Plain folks",
  "Technique: Positive name\u2212calling", "Technique: Positive testimonial",
  "Technique: Positive transfer of association",
  "Cited fact (where fact present)", "Emotion: Anger", "Emotion: Enthusiasm",
  "Explicit vote for", "Messenger: Female", "Messenger: Politician",
  "Primary tone: Contrast", "Primary tone: Positive", "Production value: High",
  "Specificity: Candidate facts", "Specificity: Policy facts", "How pushy",
  "Issue: BLM/Race", "Issue: COVID\u221219", "Issue: Decency",
  "Messenger: Everyday people", "Messenger: Healthcare worker",
  "Messenger: Republican"
)

tidy_name <- function(x) {
  x |>
    str_replace_all("\u2212", "-") |>
    str_replace("Technique: Negative name-calling", "Technique: Negative name-calling") |>
    str_replace("Issue: COVID-19", "Issue: COVID-19")
}

# Figure 3 and Figure OA10 carry six columns for a hypothesis measured in all
# three contexts and four for a 2020-only one; the column order differs between
# them only in the row order, so the same parser reads both.
matrix_columns_six <- c("Favorability | 2018", "Favorability | 2020 Downballot",
                        "Favorability | 2020 Presidential", "Vote choice | 2018",
                        "Vote choice | 2020 Downballot",
                        "Vote choice | 2020 Presidential")
matrix_columns_four <- c("Favorability | 2020 Downballot",
                         "Favorability | 2020 Presidential",
                         "Vote choice | 2020 Downballot",
                         "Vote choice | 2020 Presidential")

label_matrix <- function(parsed, float) {
  parsed |>
    mutate(
      column = if_else(n_values == 6, matrix_columns_six[position],
                       matrix_columns_four[position]),
      row_key = str_c(tidy_name(hypothesis), " | ", column),
      value = str_remove_all(cell, "\\*")
    ) |>
    with(cells(float, row_key, "t_statistic", value))
}

figure_3 <- label_matrix(
  parse_matrix_rows(paper, matrix_hypotheses, c(4, 6)), "figure_3"
)
figure_oa10 <- label_matrix(
  parse_matrix_rows(online_appendix, matrix_hypotheses, c(4, 6)), "figure_oa10"
)

stopifnot(nrow(figure_3) == 166, nrow(figure_oa10) == 166,
          !any(duplicated(figure_3$row_key)),
          !any(duplicated(figure_oa10$row_key)))

# Figure OA12 has eight columns: four specifications, two outcomes.
oa12_columns <- c(
  "Favorability | Standard", "Favorability | IPW",
  "Favorability | Study random effects", "Favorability | Study fixed effects",
  "Vote choice | Standard", "Vote choice | IPW",
  "Vote choice | Study random effects", "Vote choice | Study fixed effects"
)

figure_oa12 <-
  parse_matrix_rows(online_appendix, matrix_hypotheses, 8) |>
  mutate(row_key = str_c(tidy_name(hypothesis), " | ", oa12_columns[position]),
         value = str_remove_all(cell, "\\*")) |>
  with(cells("figure_oa12", row_key, "t_statistic", value))

stopifnot(nrow(figure_oa12) == 184, !any(duplicated(figure_oa12$row_key)))

# Table OA2 -------------------------------------------------------------------------

# Same two-line shape as main text Table 2, with the election label between the
# estimate and the interval line.
oa2_start <- which(str_detect(online_appendix, "Table OA2:"))
stopifnot(length(oa2_start) == 1)
oa2_block <- online_appendix[oa2_start:(oa2_start + 20)]
oa2_estimate_lines <- which(str_detect(oa2_block, "^\\s+\\d\\.\\d\\d\\s+\\d\\.\\d\\d\\s+\\d\\.\\d\\d\\s+\\d\\.\\d\\d\\s*$"))
stopifnot(length(oa2_estimate_lines) == 3)

table_oa2 <-
  oa2_estimate_lines |>
  imap(function(i, k) {
    estimates <- str_squish(oa2_block[i]) |> str_split(" ") |> pluck(1)
    election_line <- str_squish(oa2_block[i + 1])
    intervals <- str_match_all(str_squish(oa2_block[i + 2]),
                               "\\[([^,]+), ([^\\]]+)\\]")[[1]]
    p_values <- str_extract_all(election_line, "(< \\.001|\\d\\.\\d+)")[[1]]
    stopifnot(length(estimates) == 4, nrow(intervals) == 4, length(p_values) == 3)
    election <- c("2018", "2020 Downballot", "2020 Presidential")[k]
    bind_rows(
      cells("table_oa2", election,
            c("mu", "tau_no_moderators", "tau_standard",
              "tau_study_fixed_effects"), estimates),
      cells("table_oa2", election,
            c("mu_ci_low", "mu_ci_high", "tau_no_moderators_ci_low",
              "tau_no_moderators_ci_high", "tau_standard_ci_low",
              "tau_standard_ci_high", "tau_study_fixed_effects_ci_low",
              "tau_study_fixed_effects_ci_high"),
            as.vector(t(intervals[, 2:3]))),
      cells("table_oa2", election,
            c("p_no_moderators", "p_standard", "p_study_fixed_effects"),
            str_replace(p_values, "^< \\.001$", "<0.001"))
    )
  }) |>
  list_rbind()

# The metaregression tables: Table OA1 and Tables DA4 to DA30 -----------------------

metaregression_columns <- c("2018", "2020D", "2020P", "Combined", "2020D-2018",
                            "2020P-2018", "2020P-2020D")

# A metaregression table cell is either "estimate (se)", a bare variance figure,
# a count, or a p-value; a column a row does not reach is simply absent, so the
# cells are located by their character offset against the header line.
parse_metaregression_table <- function(lines, float) {
  header <- which(str_detect(lines, "^\\s*Predictor\\s+2018\\s+2020D") |
                    str_detect(lines, "^\\s*Predictor\\s+2020D\\s+2020P"))
  outcome_lines <- which(str_detect(lines, "Metaregressions with"))
  stopifnot(length(header) >= 1)

  map(header, function(h) {
    outcome <- if (any(outcome_lines < h)) {
      last_outcome <- max(outcome_lines[outcome_lines < h])
      if (str_detect(lines[last_outcome], "Favorability")) "Favorability" else "Vote choice"
    } else NA_character_
    header_line <- lines[h]
    labels <- str_match_all(header_line, "\\S+")[[1]][, 1]
    starts <- str_locate_all(header_line, "\\S+")[[1]][, "start"]
    columns <- tibble(label = labels, start = starts) |> filter(label != "Predictor")

    body <- character()
    j <- h + 1
    while (j <= length(lines) && !str_detect(lines[j], "Metaregressions with|Table DA|^\\s*$|^\\s*DA\\d+\\s*$")) {
      body <- c(body, lines[j])
      j <- j + 1
    }
    # A blank line separates the coefficients from the variance rows, so the
    # scan continues past one blank line but not past two.
    while (j <= length(lines) && str_detect(lines[j], "^\\s*$")) j <- j + 1
    while (j <= length(lines) && !str_detect(lines[j], "Metaregressions with|Table DA|^\\s*$|^\\s*DA\\d+\\s*$")) {
      body <- c(body, lines[j])
      j <- j + 1
    }

    map(body, function(line) {
      # A page footer can land on the same line as a table cell and glue itself
      # to the number: the vote choice tau row of Table DA30 arrives as
      # "DA20.35" where the cell is 0.35.
      line <- str_replace_all(line, "DA\\d+", "    ")
      predictor <- str_squish(str_sub(line, 1, min(columns$start) - 1))
      if (!nzchar(predictor)) return(tibble())
      predictor <- predictor |>
        str_replace("^R.2 \\(all predictors\\)$", "R2 all predictors") |>
        str_replace("^R.2 \\(all vs. control\\)$", "R2 all vs control") |>
        str_replace("^\u02c6$", "tau") |>
        str_replace("^Ntreatments$", "N treatments")
      matches <- str_locate_all(line, "\\S[^\\s]*(?: \\([^)]*\\))?")[[1]]
      tokens <- str_match_all(line, "[-\u2212<]?[\\d.]+ \\([\\d.]+\\)|p = [\\d.]+|<0|[-\u2212]?[\\d.]+")[[1]][, 1]
      positions <- str_locate_all(line, "[-\u2212<]?[\\d.]+ \\([\\d.]+\\)|p = [\\d.]+|<0|[-\u2212]?[\\d.]+")[[1]][, "start"]
      if (length(tokens) == 0) return(tibble())
      keep <- positions >= min(columns$start) - 2
      tokens <- tokens[keep]
      positions <- positions[keep]
      if (length(tokens) == 0) return(tibble())
      column <- map_chr(positions, function(p) {
        columns$label[which.min(abs(columns$start - p))]
      })
      tibble(float = float, outcome = outcome, predictor = predictor,
             column = column, token = tokens)
    }) |>
      list_rbind()
  }) |>
    list_rbind()
}

expand_metaregression_cells <- function(parsed) {
  parsed |>
    filter(!is.na(outcome), predictor != "(Studies)") |>
    mutate(
      row_key = str_c(outcome, " | ", predictor, " | ", column),
      estimate = str_match(token, "^([-\u2212]?[\\d.]+) \\(")[, 2],
      se = str_match(token, "\\(([\\d.]+)\\)$")[, 2],
      p_value = str_match(token, "^p = ([\\d.]+)$")[, 2],
      bare = if_else(is.na(estimate) & is.na(p_value), token, NA_character_)
    ) |>
    (function(d) bind_rows(
      cells(d$float[!is.na(d$estimate)], d$row_key[!is.na(d$estimate)], "estimate",
            d$estimate[!is.na(d$estimate)]),
      cells(d$float[!is.na(d$se)], d$row_key[!is.na(d$se)], "std.error",
            d$se[!is.na(d$se)]),
      cells(d$float[!is.na(d$p_value)], d$row_key[!is.na(d$p_value)], "p_diff",
            d$p_value[!is.na(d$p_value)]),
      cells(d$float[!is.na(d$bare)], d$row_key[!is.na(d$bare)], "value",
            d$bare[!is.na(d$bare)])
    ))()
}

table_oa1 <- expand_metaregression_cells(
  parse_metaregression_table(
    online_appendix[which(str_detect(online_appendix, "Table OA1:")):
                      (which(str_detect(online_appendix, "Table OA2:")) - 1)],
    "table_oa1"
  )
)

dataverse_tables <-
  tibble(number = 4:30) |>
  mutate(start = map_int(number, function(n) {
    hit <- which(str_detect(dataverse_appendix, str_glue("Table DA{n}: Metaregressions")))
    stopifnot(length(hit) == 1)
    hit
  })) |>
  arrange(start) |>
  mutate(end = lead(start, default = length(dataverse_appendix)) - 1L) |>
  mutate(parsed = pmap(list(number, start, end), function(n, s, e) {
    expand_metaregression_cells(
      parse_metaregression_table(dataverse_appendix[s:e], str_glue("table_da{n}"))
    )
  })) |>
  pull(parsed) |>
  list_rbind()

# Table DA35 --------------------------------------------------------------------------

da35_lines <- dataverse_appendix[
  which(str_detect(dataverse_appendix, "Table DA35")):length(dataverse_appendix)
] |>
  str_subset("\\s[-\\d][\\d.]*\\s+[-\\d][\\d.]*\\s+[-\\d][\\d.]*\\s+20\\d\\d\\s+\\w+\\s*$")

table_da35 <-
  da35_lines |>
  map(function(line) {
    parts <- str_match(str_squish(line),
      "^(.*?) (-?[\\d.]+) (-?[\\d.]+) (-?[\\d.]+) (20\\d\\d) (\\w+)$")
    row_key <- str_c(parts[1, 2], " | ", parts[1, 6])
    cells("table_da35", row_key, c("estimate", "conf.low", "conf.high"),
          parts[1, 3:5])
  }) |>
  list_rbind()

stopifnot(nrow(table_da35) == 53 * 3)

# Write -------------------------------------------------------------------------------

published_maintext <-
  bind_rows(table_1, table_2, table_3, figure_1, figure_2, figure_3) |>
  arrange(float, row_key, quantity, .locale = "en")

published_appendix <-
  bind_rows(table_oa1, table_oa2, figure_oa10, figure_oa12, dataverse_tables,
            table_da35) |>
  arrange(float, row_key, quantity, .locale = "en")

stopifnot(
  !any(duplicated(published_maintext |> select(float, row_key, quantity))),
  !any(duplicated(published_appendix |> select(float, row_key, quantity))),
  all(!is.na(published_maintext$value_paper)),
  all(!is.na(published_appendix$value_paper))
)

write_csv(published_maintext, here::here("ground_truth", "published_maintext_tables.csv"))
write_csv(published_appendix, here::here("ground_truth", "published_appendix_values.csv"))

print(published_maintext |> count(float))
print(published_appendix |> count(float), n = Inf)
print(str_glue("{nrow(published_maintext)} main text cells and ",
               "{nrow(published_appendix)} appendix cells transcribed."))
