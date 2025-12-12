# TIBS Local - Non-Shiny Test

library(reticulate) # Runing Python Code
library(readr) # Read CSV
library(dplyr) # DataFrame manipulaitons
library(tidyr) # DataFrame manipulations
library(binom) # Confidence Intervals
library(ggplot2) # Plots

# If Running Locally:
setwd("TwilightImperiumBattleSimulator")
use_virtualenv("../.venv", required = TRUE)

source_python("simulate.py")
df <- read_csv("all_units_df.csv")

# Set Units
attackers <- list("Infantry" = 2, "Carrier" = 1)
defenders <- list("Infantry" = 2, "Carrier" = 1)
rounds <- as.integer(500)

# Convert to Python Dictionary
attacker_units_dict <- r_to_py(lapply(attackers, as.integer))
defender_units_dict <- r_to_py(lapply(defenders, as.integer))

# Call simulate_battles() from simulate.py
sim <- simulate_battles(attacker_units_dict, defender_units_dict, rounds)

results <- sim[[1]] %>%
  mutate(
    "Combat Type" = c("Space", "Ground", "Overall")
  ) %>%
  select(
    "Combat Type",
    "Attacker Wins",
    "Defender Wins",
    "Draw"
  )

metadata <- sim[[2]]
attacker_stats <- sim[[3]] %>%
  select(-Has_Anti_Fighter, -Has_Bombardment, -Has_Space_Cannon) %>% 
  select(where(~ !all(.x == 0)))  # Remove Columns will all zeros
defender_stats <- sim[[4]] %>%
  select(-Has_Anti_Fighter, -Has_Bombardment, -Has_Space_Cannon) %>%
  select(where(~ !all(.x == 0)))  # Remove Columns will all zeros

# Compute Confidence Intervals

# Bonferroni-adjusted Confidence Level
alpha <- 0.05 / 3
level = 1 - alpha

# Wilson Confidence Interval Bounds
get_wilson_ci <- function(k, n, conf.level = level) {
    ci <- binom.confint(k, n, conf.level = level, methods = "wilson")
    return(c(lower = ci$lower, upper = ci$upper))
}

metadata_ci <- metadata %>%
    rename(
    "Attacker_Wins" = "Attacker Wins",
    "Defender_Wins" = "Defender Wins",
    "Combats_Simulated" = "Combats Simulated"
    ) %>%
    rowwise() %>%
    mutate(
    ci_attacker = list(get_wilson_ci(Attacker_Wins, Combats_Simulated)),
    lwr_attacker = ci_attacker[["lower"]],
    upr_attacker = ci_attacker[["upper"]],
    
    ci_defender = list(get_wilson_ci(Defender_Wins, Combats_Simulated)),
    lwr_defender = ci_defender[["lower"]],
    upr_defender = ci_defender[["upper"]],
    
    ci_draw = list(get_wilson_ci(Draws, Combats_Simulated)),
    lwr_draw = ci_draw[["lower"]],
    upr_draw = ci_draw[["upper"]]
    ) %>%
    ungroup() %>%
    select(Combats_Simulated,
        lwr_attacker, upr_attacker,
        lwr_defender, upr_defender,
        lwr_draw, upr_draw) %>%
    round(2)

ci <- metadata_ci %>%
    mutate(
    "Combat Type" = c("Space", "Ground")
    ) %>%
    rowwise() %>%
    mutate(
    "Attacker CI" = paste(lwr_attacker, "-", upr_attacker),
    "Defender CI" = paste(lwr_defender, "-", upr_defender),
    "Draw CI" = paste(lwr_draw, "-", upr_draw),
    ) %>%
    select(
    "Combat Type",
    "Attacker CI",
    "Defender CI",
    "Draw CI"
    )


results_long <- results %>%
  mutate("Combat Type" = c("Space", "Ground", "Overall")) %>%
  pivot_longer(
    cols = c("Attacker Wins", "Defender Wins", "Draw"),
    names_to = "Battle Result",
    values_to = "Percentage"
  ) %>%
  mutate("Combat Type" = factor(
    `Combat Type`,
    levels = c("Space", "Ground", "Overall")
  ))


# Custom colors (match button colors)
colors <- c(
  "Attacker Wins" = "#1f77b4",
  "Defender Wins" = "#d62728",
  "Draw"          = "#7f7f7f"
)

plot_data <- results_long %>%
  group_by(`Combat Type`) %>%
  mutate(
    Fraction = Percentage / sum(Percentage),
    ymax = cumsum(Fraction),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymin + ymax) / 2,
    label = paste0(Percentage, "%")
  )

ggplot(plot_data) +
  geom_rect(
    aes(
      ymin = ymin,
      ymax = ymax,
      xmin = 3,
      xmax = 4,
      fill = `Battle Result`
    )
  ) +
  geom_text(
    aes(
      x = 3.5,
      y = label_pos,
      label = label
    ),
    size = 3.5,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = colors) +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5
    )
  ) +
  facet_wrap(~ `Combat Type`)
