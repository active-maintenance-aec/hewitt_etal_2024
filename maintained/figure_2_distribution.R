# hewitt_etal_2024/maintained/figure_2_distribution.R
# Output: output/figure_2_distribution.pdf, .png,
#   output/figure_2_tau_labels.csv, output/figure_2_inset.csv
# Depends on: helpers.R, estimate_ates.R and metaregressions.R output
# Description: Figure 2. The estimated distribution of true treatment effects in
#   each context, drawn over a histogram of the estimates themselves, with an
#   inset showing the two largest 2020 presidential vote-choice estimates.

source(here::here("maintained", "helpers.R"))

studies <- read_deposit_rds("studies.RDS") |> select(study_id, dataset)

ates <-
  read_csv(out("ate_estimates.csv"), show_col_types = FALSE) |>
  left_join(studies, by = "study_id")

overall <-
  read_csv(out("metaregression_estimates.csv"), show_col_types = FALSE) |>
  filter(metareg_group == "overall", specification == "no_moderators",
         weighting == "unweighted", term %in% c("intercept", "sigma"))

tau_labels <-
  overall |>
  filter(term == "sigma") |>
  select(outcome, dataset, tau = estimate, tau_low = conf.low, tau_high = conf.high) |>
  join_one_to_one(
    overall |> filter(term == "intercept") |>
      select(outcome, dataset, mu = estimate, mu_low = conf.low, mu_high = conf.high),
    by = c("outcome", "dataset")
  ) |>
  mutate(dataset = factor(dataset, levels = dataset_levels)) |>
  arrange(outcome, dataset, .locale = "en")

write_csv(tau_labels, out("figure_2_tau_labels.csv"))

# The inset names the two largest presidential vote-choice effects and prints
# the number of respondents behind each.
inset_data <-
  ates |>
  filter(outcome == "votechoice", dataset == "2020 Presidential") |>
  slice_max(estimate, n = 2) |>
  select(content_id, estimate, conf.low, conf.high, n_obs)

write_csv(inset_data, out("figure_2_inset.csv"))

binwidth <- 0.1

distribution_panel <- function(outcome_id, dataset_name, xlabel = NULL,
                               ylabel = NULL, title = NULL) {
  d <- ates |> filter(outcome == outcome_id, dataset == dataset_name)
  label <- tau_labels |> filter(outcome == outcome_id, dataset == dataset_name)
  stopifnot(nrow(label) == 1)

  g <- d |>
    ggplot(aes(x = estimate)) +
    geom_histogram(aes(y = after_stat(count) / sum(after_stat(count)) / binwidth,
                       fill = "Unpooled individual estimates"),
                   binwidth = binwidth, alpha = 1) +
    geom_area(stat = "function", mapping = aes(fill = outcome_id),
              fun = ~ dnorm(., mean = label$mu, sd = label$tau),
              color = "black", alpha = 0.35) +
    # ggplot2 4.x rejects a language object passed as `label`, which is how the
    # deposit writes this annotation; a parsed plotmath string is equivalent.
    annotate(geom = "text", x = -2, y = 0.44, color = outcome_colors[[outcome_id]],
             vjust = 0.5, hjust = 0.5, size = 3.5, parse = TRUE,
             label = sprintf("atop(hat(tau) == '%.2f', '[%.2f, %.2f]')",
                             label$tau, label$tau_low, label$tau_high)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_fill_manual(guide = "none",
                      values = c(outcome_colors,
                                 "Unpooled individual estimates" = "gray")) +
    coord_cartesian(xlim = c(-4.5, 9.5), ylim = c(0, 1.3), expand = FALSE) +
    theme_bw() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
          legend.position = "none", plot.title = element_text(hjust = 0.5))

  g <- if (is.null(xlabel)) g + theme(axis.title.x = element_blank()) else g + xlab(xlabel)
  g <- if (is.null(ylabel)) g + theme(axis.title.y = element_blank()) else g + ylab(ylabel)
  if (!is.null(title)) g <- g + ggtitle(title)
  g
}

panels <-
  distribution_panel("favorability", "2018", ylabel = "2018", title = "Favorability") +
  distribution_panel("votechoice", "2018", title = "Votechoice") +
  plot_spacer() +
  distribution_panel("favorability", "2020 Downballot", ylabel = "2020 Downballot") +
  distribution_panel("votechoice", "2020 Downballot") +
  plot_spacer() +
  distribution_panel("favorability", "2020 Presidential", ylabel = "2020 Presidential",
                     xlabel = "Average Treatment Effect (pp)") +
  distribution_panel("votechoice", "2020 Presidential",
                     xlabel = "Average Treatment Effect (pp)") +
  plot_spacer() +
  plot_layout(nrow = 3, ncol = 3, widths = c(3, 3, 1, 3, 3, 1, 3, 3, 1))

inset <-
  inset_data |>
  ggplot(aes(x = content_id, y = estimate)) +
  geom_point(color = "gray") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "gray") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_text(aes(label = paste0("N obs. = ", n_obs)), vjust = -1, size = 2.8) +
  coord_flip() +
  labs(x = "", y = "ATE", title = "Two largest treatment effects") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        plot.title.position = "plot",
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(color = "black", linewidth = 1))

annotated <-
  ggdraw() +
  draw_plot(panels) +
  draw_plot(inset, x = 0.68, y = 0.12, width = 0.3, height = 0.2) +
  geom_segment(data = tibble(x = c(0.685, 0.70), xend = c(0.68, 0.98),
                             y = 0.08, yend = 0.12),
               aes(x = x, xend = xend, y = y, yend = yend),
               color = "black", linetype = "dotted", linewidth = 0.7) +
  geom_segment(aes(x = 0.25, y = 0.8, xend = 0.25, yend = 0.77),
               color = outcome_colors[["favorability"]],
               arrow = arrow(length = unit(7, "pt"), type = "closed")) +
  geom_text(aes(x = 0.25, y = 0.82), label = "Estimated\ndistribution",
            color = outcome_colors[["favorability"]], vjust = 0) +
  geom_segment(aes(x = 0.38, y = 0.76, xend = 0.38, yend = 0.73),
               color = "gray60",
               arrow = arrow(length = unit(7, "pt"), type = "closed")) +
  geom_text(aes(x = 0.38, y = 0.78), label = "Raw\nestimates",
            color = "gray60", vjust = 0)

save_figure(annotated, "figure_2_distribution", width = 7, height = 7)

print(tau_labels |> filter(outcome != "votechoice_dichotomized"))
print(inset_data)
