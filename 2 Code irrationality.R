# select "final dataset"

################################################################################
# Libraries
################################################################################
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

################################################################################
# plots
################################################################################

cat("Select the XLSX file with the final dataset...\n")
data_path <- file.choose(new = FALSE)

d <- read_excel(data_path, sheet = "Sheet1") %>%
  rename(
    belief_index = `Belief index (ext)`,   # extended belief index (0–1)
    risky_share  = `Risky share hyp`,      # Actual risky share (0–1)
    female       = Female,                 # 0 = male, 1 = female
    prime        = Prime                   # 0 = control, 1 = prime
  ) %>%
  mutate(
    female = as.integer(female),
    prime  = as.integer(prime)
  )

plot_df <- d %>%
  filter(!is.na(female), prime %in% c(0, 1)) %>%
  mutate(
    gender   = if_else(female == 1, "Female", "Male"),
    prime_lab = if_else(prime == 1, "Prime", "No prime")
  ) %>%
  select(gender, prime_lab, risky_share, belief_index) %>%
  pivot_longer(
    cols = c(risky_share, belief_index),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = recode(measure,
                     "belief_index" = "belief_index",
                     "risky_share"  = "risky_share"),
    measure = factor(measure, levels = c("belief_index", "risky_share"))
  )

sum_df <- plot_df %>%
  group_by(measure, prime_lab, gender) %>%
  summarise(
    n    = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = 1.96 * se,
    .groups = "drop"
  )

theme_set(theme_minimal(base_size = 12))

p <- ggplot(plot_df, aes(x = gender, y = value)) +
  geom_jitter(width = 0.12, alpha = 0.30, size = 1, colour = "grey40") +
  geom_point(
    data = sum_df,
    aes(x = gender, y = mean),
    inherit.aes = FALSE,
    size = 3,
    colour = "black"
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = gender, ymin = mean - ci, ymax = mean + ci),
    inherit.aes = FALSE,
    width = 0.15,
    linewidth = 0.6,
    colour = "black"
  ) +
  geom_text(
    data = sum_df,
    aes(x = gender, y = mean - ci - 0.05, label = paste0("n = ", n)),
    inherit.aes = FALSE,
    size = 3.2,
    vjust = 1.2
  ) +
  facet_grid(measure ~ prime_lab, scales = "free_y") +
  labs(
    title    = "Male vs Female with and without Priming",
    subtitle = "Points = individuals; big dots = mean; bars = 95% CI",
    x = NULL,
    y = NULL
  )

out_path <- file.path(Sys.getenv("HOME"), "Desktop", "combined_2x2_gender_prime_NEW.png")
ggsave(out_path, plot = p, width = 10, height = 7, dpi = 300)
cat("Plot saved to:\n", out_path, "\n")

rm(list = ls())

################################################################################
# density functions
################################################################################

cat("Select the XLSX file with the final dataset...\n")
data_path <- file.choose(new = FALSE)

d_raw <- read_excel(data_path, sheet = "Sheet1")

if ("Prime" %in% names(d_raw) && "prime" %in% names(d_raw)) {
  d_raw <- d_raw %>% dplyr::select(-Prime)
} else if (!"prime" %in% names(d_raw) && "Prime" %in% names(d_raw)) {
  d_raw <- d_raw %>% dplyr::rename(prime = Prime)
}

d <- d_raw %>%
  dplyr::rename(
    belief_index   = `Belief index (ext)`,
    risky_share    = `Risky share hyp`,
    female         = Female,
    educ           = Educ,
    overconfidence = Overconfidence,
    experience     = Experience
  ) %>%
  mutate(
    female = as.integer(female),
    prime  = as.integer(prime)
  ) %>%
  filter(!is.na(female), prime %in% c(0, 1)) %>%
  mutate(
    gender   = if_else(female == 1, "Female", "Male"),
    prime_lab = if_else(prime == 1, "Prime", "No prime")
  )

out_dir <- file.path(Sys.getenv("HOME"), "Desktop")

theme_set(ggplot2::theme_minimal(base_size = 12))

dens_df <- d %>%
  select(gender, prime_lab, belief_index, risky_share) %>%
  pivot_longer(
    cols = c(belief_index, risky_share),
    names_to = "measure",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    measure = factor(
      measure,
      levels = c("belief_index", "risky_share"),
      labels = c("Belief index (ext)", "Risky share (hypotetical)")
    )
  )

p_C <- ggplot(dens_df, aes(x = value, fill = gender)) +
  geom_density(alpha = 0.35) +
  facet_grid(measure ~ prime_lab, scales = "free") +
  labs(
    x = NULL,
    y = "Density",
    title = "Distributions by Prime and Gender",
    fill = NULL
  )

ggsave(file.path(out_dir, "C_density_belief_and_risky_hyp.png"),
       p_C, width = 9, height = 7, dpi = 300)

cat("Saved on Desktop:\n", "- C_density_belief_and_risky_hyp.png\n")

################################################################################
# actual risky share
################################################################################

if (!"risky_share_act" %in% names(d) && "Risky share Act" %in% names(d)) {
  d <- d %>% rename(risky_share_act = `Risky share Act`)
}

plot_df <- d %>%
  filter(!is.na(female), !is.na(risky_share_act)) %>%
  mutate(
    gender = if_else(female == 1, "Female", "Male")
  )

sum_df <- plot_df %>%
  group_by(gender) %>%
  summarise(
    n    = sum(!is.na(risky_share_act)),
    mean = mean(risky_share_act, na.rm = TRUE),
    sd   = sd(risky_share_act, na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = 1.96 * se,
    .groups = "drop"
  )

theme_set(theme_minimal(base_size = 14))

p_scatter <- ggplot(plot_df, aes(x = gender, y = risky_share_act)) +
  geom_jitter(width = 0.08, alpha = 0.30, size = 1, colour = "grey40") +
  geom_point(
    data = sum_df,
    aes(x = gender, y = mean),
    inherit.aes = FALSE,
    size = 3,
    colour = "black"
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = gender, ymin = mean - ci, ymax = mean + ci),
    inherit.aes = FALSE,
    width = 0.12,
    linewidth = 0.6,
    colour = "black"
  ) +
  geom_text(
    data = sum_df,
    aes(x = gender, y = mean - ci - 0.05, label = paste0("n = ", n)),
    inherit.aes = FALSE,
    size = 3.2,
    vjust = 1.2
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Risky share (actual) by gender",
    subtitle = "Points = individuals; big dots = mean; bars = 95% CI",
    x = NULL,
    y = "Risky share (actual)"
  )

p_density <- ggplot(plot_df, aes(x = risky_share_act, fill = gender)) +
  geom_density(alpha = 0.35) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    title = "Risky share (actual) distribution",
    x = "Risky share (actual)",
    y = "Density",
    fill = NULL
  )

combined_plot <- p_scatter + p_density + plot_layout(ncol = 2)

out_path <- file.path(Sys.getenv("HOME"),
                      "Desktop",
                      "risky_share_actual_by_gender_with_density.png")

ggsave(out_path, plot = combined_plot, width = 12, height = 5, dpi = 300)

cat("Immagine salvata in:\n", out_path, "\n")

rm(list = ls())

################################################################################
# overconfidence
################################################################################

cat("Select 'Final dataset.xlsx'...\n")
data_path <- file.choose(new = FALSE)

d <- read_excel(data_path, sheet = "Sheet1") %>%
  mutate(
    female = as.integer(Female),
    gender = if_else(female == 1, "Female", "Male")
  )

print(names(d))

oc1_col <- "Overconfidence"
oc2_col <- "Overconfidence 2"

d <- d %>%
  mutate(
    oc1 = .data[[oc1_col]],
    oc2 = .data[[oc2_col]],
    oc1_z = as.numeric(scale(oc1)),
    oc2_z = as.numeric(scale(oc2))
  )

plot_df <- d %>%
  filter(!is.na(oc1_z), !is.na(oc2_z), !is.na(female)) %>%
  select(gender, oc1_z, oc2_z) %>%
  pivot_longer(
    cols = c(oc1_z, oc2_z),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = recode(
      measure,
      "oc1_z" = "Overconfidence 1",
      "oc2_z" = "Overconfidence 2"
    )
  )

sum_df <- plot_df %>%
  group_by(measure, gender) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value),
    sd = sd(value),
    se = sd / sqrt(n),
    ci = 1.96 * se,
    .groups = "drop"
  )

theme_set(theme_minimal(base_size = 14))

p_scatter <- ggplot(plot_df, aes(x = gender, y = value)) +
  geom_jitter(width = 0.10, alpha = 0.35, size = 1, colour = "grey40") +
  geom_point(
    data = sum_df,
    aes(x = gender, y = mean),
    size = 3,
    colour = "black",
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = gender, ymin = mean - ci, ymax = mean + ci),
    width = 0.12,
    linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = sum_df,
    aes(x = gender, y = mean - ci - 0.2, label = paste0("n = ", n)),
    size = 3.2,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ measure, ncol = 2) +
  labs(
    title = "Overconfidence 1 vs Overconfidence 2 (standardized)",
    subtitle = "Points = individuals; dots = mean; bars = 95% CI",
    x = NULL,
    y = "Overconfidence (z-score)"
  )

p_density <- ggplot(plot_df, aes(x = value, fill = gender)) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ measure, ncol = 2) +
  labs(
    title = "Distribution of Overconfidence 1 and 2 (standardized)",
    x = "Z-score",
    y = "Density",
    fill = NULL
  )

combined_plot <- p_scatter + p_density + plot_layout(ncol = 1)

out_path <- file.path(
  Sys.getenv("HOME"),
  "Desktop",
  "OC1_OC2_STANDARDIZED_COMPARISON.png"
)

ggsave(out_path, combined_plot, width = 11, height = 10, dpi = 300)

cat("\n✔️ Plot saved to:\n", out_path, "\n")

rm(list = ls())
