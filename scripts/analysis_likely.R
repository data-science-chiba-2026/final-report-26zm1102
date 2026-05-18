# analysis_likely.R
# Simple, single-file analysis using tidyverse.
# Purpose: explore how respondents map the word "Likely" to a numeric probability.

# Load tidyverse (includes readr, dplyr, ggplot2, tidyr, etc.)
library(tidyverse)

# Create output directories (safe if they already exist)
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# Read data directly from GitHub (as per TidyTuesday README)
absolute_judgements <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/absolute_judgements.csv'
)
pairwise_comparisons <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/pairwise_comparisons.csv'
)
respondent_metadata <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/respondent_metadata.csv'
)

# Quick data checks (prints to console)
cat("Rows in absolute_judgements:", nrow(absolute_judgements), "\n")
cat("Rows in pairwise_comparisons:", nrow(pairwise_comparisons), "\n")
cat("Rows in respondent_metadata:", nrow(respondent_metadata), "\n")

# Normalize term casing so 'Likely' matches consistently
absolute_judgements <- absolute_judgements %>%
  mutate(term = stringr::str_to_title(term))

pairwise_comparisons <- pairwise_comparisons %>%
  mutate(term1 = stringr::str_to_title(term1),
         term2 = stringr::str_to_title(term2),
         selected = stringr::str_to_title(selected))

# ---- 1) Focused summary for "Likely" ----
likely_df <- absolute_judgements %>%
  filter(term == "Likely")

likely_summary <- likely_df %>%
  summarize(n = n(),
            mean = mean(probability, na.rm = TRUE),
            median = median(probability, na.rm = TRUE),
            sd = sd(probability, na.rm = TRUE),
            IQR = IQR(probability, na.rm = TRUE),
            q10 = quantile(probability, 0.10, na.rm = TRUE),
            q90 = quantile(probability, 0.90, na.rm = TRUE))

# Save summary
readr::write_csv(likely_summary, "outputs/tables/likely_summary.csv")

# Histogram + density for "Likely"
p1 <- ggplot(likely_df, aes(x = probability)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "white") +
  geom_density(color = "darkblue", size = 1) +
  labs(title = "Distribution of numeric estimates for 'Likely'",
       x = "Estimated probability (0-100)", y = "Density") +
  theme_minimal()

ggsave("outputs/plots/likely_hist_density.png", p1, width = 7, height = 4)

# Boxplot for 'Likely' (simple)
p2 <- ggplot(likely_df, aes(y = probability)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of 'Likely' estimates", y = "Probability (0-100)") +
  theme_minimal()

ggsave("outputs/plots/likely_boxplot.png", p2, width = 4, height = 5)

# ---- 2) Comparison across terms (summaries + boxplot) ----
# Compute median per term and order terms by median for plotting
term_summary <- absolute_judgements %>%
  group_by(term) %>%
  summarize(n = n(), median = median(probability, na.rm = TRUE),
            mean = mean(probability, na.rm = TRUE), sd = sd(probability, na.rm = TRUE)) %>%
  arrange(desc(n))

# Save term summary
readr::write_csv(term_summary, "outputs/tables/term_summary.csv")

# For plotting, keep top 12 most-frequently-judged terms to avoid overcrowding
top_terms <- term_summary %>% slice_max(n, n = 12) %>% pull(term)
plot_df <- absolute_judgements %>% filter(term %in% top_terms) %>%
  mutate(term = fct_reorder(term, probability, .fun = median))

p3 <- ggplot(plot_df, aes(x = term, y = probability)) +
  geom_boxplot(fill = "tan") +
  coord_flip() +
  labs(title = "Comparison of numeric estimates across top terms",
       x = "Term", y = "Probability (0-100)") +
  theme_minimal()

ggsave("outputs/plots/terms_boxplot.png", p3, width = 8, height = 6)

# ---- 3) Demographic comparisons for 'Likely' ----
# Join with respondent metadata using response_id to get demographics
likely_demog <- likely_df %>%
  left_join(respondent_metadata, by = "response_id")

# Summary by age_band
age_summary <- likely_demog %>%
  group_by(age_band) %>%
  summarize(n = n(), mean = mean(probability, na.rm = TRUE), sd = sd(probability, na.rm = TRUE)) %>%
  arrange(desc(n))
readr::write_csv(age_summary, "outputs/tables/likely_by_age.csv")

# Simple boxplot of 'Likely' by age_band (only if there are non-missing age bands)
if (nrow(age_summary) > 0) {
  p4 <- likely_demog %>%
    filter(!is.na(age_band)) %>%
    mutate(age_band = fct_reorder(age_band, probability, .fun = median)) %>%
    ggplot(aes(x = age_band, y = probability)) +
    geom_boxplot(fill = "lightgreen") +
    coord_flip() +
    labs(title = "'Likely' estimates by age band", x = "Age band", y = "Probability") +
    theme_minimal()
  ggsave("outputs/plots/likely_by_age_boxplot.png", p4, width = 7, height = 5)
}

# ---- 4) Pairwise comparisons: win-rate of 'Likely' ----
# Find comparisons involving 'Likely' in either position
pair_likely <- pairwise_comparisons %>%
  filter(term1 == "Likely" | term2 == "Likely") %>%
  mutate(opponent = if_else(term1 == "Likely", term2, term1),
         likely_selected = (selected == "Likely"))

if (nrow(pair_likely) > 0) {
  win_rates <- pair_likely %>%
    group_by(opponent) %>%
    summarize(trials = n(), wins = sum(likely_selected, na.rm = TRUE)) %>%
    mutate(win_rate = wins / trials) %>%
    arrange(desc(win_rate))
  readr::write_csv(win_rates, "outputs/tables/likely_pairwise_win_rates.csv")

  p5 <- ggplot(win_rates, aes(x = fct_reorder(opponent, win_rate), y = win_rate)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Win rate of 'Likely' vs opponent terms (pairwise)", x = "Opponent term", y = "Win rate") +
    theme_minimal()
  ggsave("outputs/plots/likely_pairwise_winrate.png", p5, width = 7, height = 5)
}

# ---- Done ----
cat("Analysis complete. Outputs saved in outputs/plots and outputs/tables\n")
