# hewitt_etal_2024/maintained/figure_4_returns_to_exp.R
# Output: output/figure_4_returns_to_exp.pdf, .png,
#   output/figure_4_optimal_spend.csv, output/figure_4_gains_from_testing.csv
# Depends on: helpers.R, original_extracted/.../sims_df.csv, tau_over_mu_stats.csv
# Description: Figure 4. What a campaign should spend on experimentation, how
#   many votes experimentation buys it, and how both depend on how variable
#   advertisement effects are.
#
#   The three panels read the deposited simulation output. The simulation itself
#   is a deposited script that takes about three hours and draws 10,000
#   experiments at each of 110 budget-by-variability settings; it is checked by
#   ground_truth/run_archive.R, which runs it, rather than repeated here.

source(here::here("maintained", "helpers.R"))

simulations <-
  read_csv(processed("sims_df.csv"), show_col_types = FALSE) |>
  mutate(
    votes_without_experimentation = n_votes_without_exp,
    votes_with_experimentation = n_votes_with_exp,
    net_votes = n_votes_with_exp - n_votes_without_exp,
    dollars_per_vote = budget / n_votes_with_exp,
    fraction_of_budget = exp_cost / budget,
    budget_thousands = budget / 1000
  )

tau_over_mu <- read_csv(processed("tau_over_mu_stats.csv"), show_col_types = FALSE)

write_csv(
  simulations |>
    select(budget, tau_over_mu, n_ads, n_obs, exp_cost, fraction_of_budget,
           votes_with_experimentation, votes_without_experimentation, net_votes,
           dollars_per_vote),
  out("figure_4_optimal_spend.csv")
)

# Panel b names the gain from testing for a medium and a large campaign, and
# those two numbers are the only ones printed on the face of the figure.
gains <-
  simulations |>
  filter(tau_over_mu == 0.51, budget_thousands %in% c(1e3, 5e3)) |>
  transmute(
    campaign = if_else(budget_thousands == 1e3, "Medium campaign", "Large campaign"),
    budget, votes_without_experimentation, votes_with_experimentation,
    gain_from_testing = round(votes_with_experimentation -
                                votes_without_experimentation)
  )

write_csv(gains, out("figure_4_gains_from_testing.csv"))

# Panel a ------------------------------------------------------------------------

variability_labels <- tibble(
  tau_over_mu = c(0.01, 0.51, 1.01),
  label = paste0("tau/mu = ", c(0.01, 0.51, 1.01)),
  budget = c(2000000, 3500000, 4500000),
  fraction_of_budget = c(0.02, 0.09, 0.135)
)

panel_a <-
  simulations |>
  filter(tau_over_mu %in% c(0.01, 0.51, 1.01)) |>
  ggplot(aes(budget, fraction_of_budget, group = tau_over_mu)) +
  geom_point() +
  geom_smooth(se = FALSE, span = 3, color = gray(0.5)) +
  geom_text(data = variability_labels, aes(label = label), size = 2) +
  scale_x_continuous(breaks = seq(0, 5e6, by = 1e6),
                     labels = paste0("$", 0:5, "MM")) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Total budget for ad testing and ad buys",
       y = "Optimal fraction of budget spend on ad testing") +
  theme_hewitt() +
  theme(legend.position = "none")

# Panel b ------------------------------------------------------------------------

pad <- 500

arrows <-
  simulations |>
  filter(tau_over_mu == 0.51, budget_thousands %in% c(1e3, 5e3)) |>
  transmute(
    budget_thousands,
    y = votes_without_experimentation + pad,
    yend = votes_with_experimentation - pad,
    ypos = (votes_without_experimentation + votes_with_experimentation) / 2 -
      if_else(budget_thousands == 1e3, 3000, 4500),
    x = budget_thousands + if_else(budget_thousands == 1e3, 50, -50),
    hjust = if_else(budget_thousands == 1e3, 0, 1),
    campaign = if_else(budget_thousands == 1e3, "Medium campaign", "Large campaign"),
    label = str_glue("{campaign}\ngain from testing:\n ",
                     "{format(round(votes_with_experimentation - votes_without_experimentation), big.mark = ',')} votes")
  )

panel_b <-
  simulations |>
  filter(tau_over_mu == 0.51) |>
  select(budget_thousands, `With experimentation` = votes_with_experimentation,
         `Without experimentation` = votes_without_experimentation) |>
  pivot_longer(-budget_thousands) |>
  ggplot(aes(budget_thousands, value, group = name)) +
  geomtextpath::geom_textline(aes(label = name)) +
  geom_segment(data = arrows,
               aes(x = budget_thousands, xend = budget_thousands, y = y, yend = yend,
                   group = NULL),
               arrow = arrow(length = unit(0.05, "inches"))) +
  geom_text(data = arrows, aes(x = x, y = ypos, label = label, hjust = hjust,
                               group = NULL), size = 2) +
  annotate("text", x = 100, y = 38000, label = "Assumes\ntau/mu = 0.51",
           size = 2, hjust = 0) +
  scale_y_continuous(breaks = seq(0, 4e4, by = 1e4),
                     labels = format(seq(0, 4e4, by = 1e4), big.mark = ",",
                                     scientific = FALSE, trim = TRUE)) +
  scale_x_continuous(breaks = seq(0, 5e3, by = 1e3),
                     labels = paste0("$", 0:5, "MM")) +
  labs(x = "Total budget for ad testing and ad buys",
       y = "Total vote increase from advertising") +
  theme_hewitt() +
  theme(legend.position = "none")

# Panel c ------------------------------------------------------------------------

panel_c <-
  simulations |>
  filter(budget %in% c(500000, 1000000, 5000000)) |>
  mutate(ad_budget = case_when(
    budget == 500000 ~ "Small ($0.5MM)",
    budget == 1000000 ~ "Medium ($1MM)",
    budget == 5000000 ~ "Large ($5MM)"
  )) |>
  ggplot(aes(x = tau_over_mu, y = dollars_per_vote, group = ad_budget)) +
  geom_hline(yintercept = 200, alpha = 0.5, linetype = "dashed") +
  annotate("text", x = 0.8, y = 200, size = 2,
           label = "Average estimated dollars per vote \n(Sides, Vavreck, Warshaw, 2021)") +
  geom_vline(xintercept = tau_over_mu$estimate, linetype = "dotted") +
  annotate("rect", xmin = tau_over_mu$conf.low, xmax = tau_over_mu$conf.high,
           ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "gray") +
  geomtextpath::geom_textpath(aes(label = ad_budget), size = 2) +
  annotate("text", x = 0.5, y = 100, size = 2, hjust = 1,
           label = "Average estimated\ntau/mu = 0.51") +
  coord_cartesian(xlim = c(0, 1)) +
  scale_y_continuous(breaks = seq(50, 250, by = 50),
                     labels = paste0("$", seq(50, 250, by = 50))) +
  labs(y = "Dollars per vote:\n(experiment costs + media spend) / (votes gained)",
       x = "Variability of ad effects (tau/mu)") +
  theme_hewitt() +
  theme(legend.position = "none")

save_figure(panel_a + panel_b + panel_c, "figure_4_returns_to_exp",
            width = 12, height = 4)

print(gains)
print(tau_over_mu)
