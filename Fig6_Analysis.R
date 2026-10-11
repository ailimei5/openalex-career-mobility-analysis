# =========================
# Rscript2: Final Figure 6 analysis
# =========================

library(dplyr)
library(readr)
library(ggplot2)

output_folder <- "openalex_outputs_final"
n_bins <- 10

# -------------------------
# Read moves file
# -------------------------
moves <- read_csv(
  file.path(output_folder, "moves_raw_5yr.csv"),
  show_col_types = FALSE
)

# -------------------------
# Convert the columns we actually use
# Some columns may come in as character after csv import
# -------------------------
moves <- moves |>
  mutate(
    delta_r = suppressWarnings(as.numeric(delta_r)),
    delta_c_star = suppressWarnings(as.numeric(delta_c_star)),
    rank = suppressWarnings(as.numeric(rank)),
    next_rank = suppressWarnings(as.numeric(next_rank)),
    mean_before = suppressWarnings(as.numeric(mean_before)),
    mean_after = suppressWarnings(as.numeric(mean_after)),
    sd_all = suppressWarnings(as.numeric(sd_all)),
    n_papers = suppressWarnings(as.numeric(n_papers))
  )

# -------------------------
# Basic cleanup
# -------------------------
moves <- moves |>
  filter(!is.na(delta_r), !is.na(delta_c_star)) |>
  filter(is.finite(delta_r), is.finite(delta_c_star))

# -------------------------
# Stop if file ended up empty
# -------------------------
if (nrow(moves) == 0) {
  stop("No valid rows left after cleaning. Check moves_raw_5yr.csv from Rscript1.")
}

# -------------------------
# Trim extreme delta_c_star values
# This helps stop a few outliers from stretching the plot too much
# -------------------------
low_cut <- as.numeric(quantile(moves$delta_c_star, 0.01, na.rm = TRUE))
high_cut <- as.numeric(quantile(moves$delta_c_star, 0.99, na.rm = TRUE))

moves <- moves |>
  filter(delta_c_star >= low_cut, delta_c_star <= high_cut)

if (nrow(moves) == 0) {
  stop("No rows left after trimming outliers. Try removing that step.")
}

# -------------------------
# Split into negative / zero / positive delta_r
# This makes the binning cleaner on both sides
# -------------------------
moves_neg <- moves |> filter(delta_r < 0)
moves_pos <- moves |> filter(delta_r > 0)
moves_zero <- moves |> filter(delta_r == 0)

# -------------------------
# Bin negative and positive moves separately
# -------------------------
bins_each_side <- max(1, floor(n_bins / 2))

if (nrow(moves_neg) > 0) {
  moves_neg <- moves_neg |>
    mutate(bin = ntile(delta_r, bins_each_side))
}

if (nrow(moves_pos) > 0) {
  moves_pos <- moves_pos |>
    mutate(bin = ntile(delta_r, bins_each_side))
}

if (nrow(moves_zero) > 0) {
  moves_zero <- moves_zero |>
    mutate(bin = 1)
}

# -------------------------
# Summarise each bin
# x = median delta_r in the bin
# y = mean delta_c_star in the bin
# -------------------------
neg_summary <- if (nrow(moves_neg) > 0) {
  moves_neg |>
    group_by(bin) |>
    summarise(
      x = median(delta_r, na.rm = TRUE),
      y = mean(delta_c_star, na.rm = TRUE),
      sd_y = sd(delta_c_star, na.rm = TRUE),
      n = n(),
      se_y = ifelse(n > 1, sd_y / sqrt(n), NA_real_),
      .groups = "drop"
    )
} else {
  tibble(
    bin = numeric(),
    x = numeric(),
    y = numeric(),
    sd_y = numeric(),
    n = numeric(),
    se_y = numeric()
  )
}

pos_summary <- if (nrow(moves_pos) > 0) {
  moves_pos |>
    group_by(bin) |>
    summarise(
      x = median(delta_r, na.rm = TRUE),
      y = mean(delta_c_star, na.rm = TRUE),
      sd_y = sd(delta_c_star, na.rm = TRUE),
      n = n(),
      se_y = ifelse(n > 1, sd_y / sqrt(n), NA_real_),
      .groups = "drop"
    )
} else {
  tibble(
    bin = numeric(),
    x = numeric(),
    y = numeric(),
    sd_y = numeric(),
    n = numeric(),
    se_y = numeric()
  )
}

zero_summary <- if (nrow(moves_zero) > 0) {
  moves_zero |>
    summarise(
      x = 0,
      y = mean(delta_c_star, na.rm = TRUE),
      sd_y = sd(delta_c_star, na.rm = TRUE),
      n = n(),
      se_y = ifelse(n > 1, sd_y / sqrt(n), NA_real_)
    )
} else {
  tibble(
    x = numeric(),
    y = numeric(),
    sd_y = numeric(),
    n = numeric(),
    se_y = numeric()
  )
}

plot_data <- bind_rows(neg_summary, zero_summary, pos_summary) |>
  arrange(x)

if (nrow(plot_data) == 0) {
  stop("No binned data available to plot.")
}

# -------------------------
# Save binned data
# -------------------------
write_csv(plot_data, file.path(output_folder, "figure6_binned_data_5yr.csv"))

# -------------------------
# Plot with SD bars
# -------------------------
plot_sd <- ggplot(plot_data, aes(x = x, y = y)) +
  scale_x_continuous(
    limits = c(-1000, 1000),
    breaks = seq(-1000, 1000, 500)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(
    aes(ymin = y - sd_y, ymax = y + sd_y),
    width = 0
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Figure 6 reproduction with OpenAlex (SD bars)",
    x = expression(Delta*r),
    y = expression(Delta*c^"*")
  ) +
  theme_classic(base_size = 14)

ggsave(
  filename = file.path(output_folder, "figure6_openalex_final_SD.png"),
  plot = plot_sd,
  width = 6,
  height = 5,
  dpi = 300
)

# -------------------------
# Plot with SE bars
# -------------------------
plot_se <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(
    aes(ymin = y - se_y, ymax = y + se_y),
    width = 0
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Figure 6 reproduction with OpenAlex (SE bars)",
    x = expression(Delta*r),
    y = expression(Delta*c^"*")
  ) +
  theme_classic(base_size = 14)

ggsave(
  filename = file.path(output_folder, "figure6_openalex_final_SE.png"),
  plot = plot_se,
  width = 6,
  height = 5,
  dpi = 300
)

# -------------------------
# Show plots
# -------------------------
print(plot_sd)
print(plot_se)

message("Done.")
message("Saved binned data: ", file.path(output_folder, "figure6_binned_data_5yr.csv"))
message("Saved SD plot: ", file.path(output_folder, "figure6_openalex_final_SD.png"))
message("Saved SE plot: ", file.path(output_folder, "figure6_openalex_final_SE.png"))