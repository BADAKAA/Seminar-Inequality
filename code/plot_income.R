# ============================================================================ #
# ------------------------- WAGE DECOMPOSITION PLOTS ------------------------- #
# ============================================================================ #

library(tidyverse)
library(fixest)
library(modelsummary)

source("code/utils.R")


start_year <- 2004
end_year <- 2024


message("Loading C-AIOE data from out/c_aioe.csv ...")
caioe <- load_caioe()

message(sprintf("  %d occupations loaded | treated: %d (%.1f%%)", nrow(caioe), sum(caioe$high_caioe, na.rm = TRUE), 100 * mean(caioe$high_caioe, na.rm = TRUE))) # nolint


soep <- load_soep() |>
  filter(syear >= start_year & syear <= end_year) |>
  filter(!is.na(isco08)) |>
  filter(pgemplst %in% c(1, 2))

analysis <- soep |>
  left_join(
    caioe, by = c("isco08" = "isco08")
  ) |>
  filter(
    !is.na(log_wage),
  )

colors <- c(COLORS[1], COLORS[2], 1)
line_types <- c(1, 1, 1)


save_plot <- function(data, filename, ylab = "Median Gross Labour Income") {
  years <- start_year:end_year

  all_values <- unlist(lapply(data, function(df) df$value))
  min_value <- min(all_values, na.rm = TRUE)
  max_value <- max(all_values, na.rm = TRUE)

  png(file = file.path(DIRS$plots, filename), 1600, 1200, pointsize = 50) # nolint
  if (.Platform$OS.type == "windows") {
    par(family = "A", mar = c(5, 5, 1, 2))
  } else {
    par(mar = c(5, 5, 1, 2))
  }
  plot(years, rep(NA_real_, length(years)),
    type = "l", col = 1,
    ylim = c(min_value, max_value),
    xlab = "Year", ylab = ylab
  )

  for (i in seq_along(data)) {
    d <- data[[i]]
    lines(d$syear, d$value, col = colors[i], lty = line_types[i], lwd = 2) # nolint
    points(d$syear, d$value, col = colors[i], pch = 19, cex = 0.7)
  }
  legend("topleft", pch = 20, legend = sub("\\.", " ", names(data)), col = colors, lty = line_types) # nolint
  dev.off()
}


plot_data <- list()
plot_data$HEHC <- analysis |>
  filter(hehc) |>
  group_by(syear) |>
  summarise(value = median(pglabgro, na.rm = TRUE))
plot_data$HELC <- analysis |>
  filter(helc) |>
  group_by(syear) |>
  summarise(value = median(pglabgro, na.rm = TRUE))
plot_data$Low.Exposure <- analysis |>
  filter(le) |>
  group_by(syear) |>
  summarise(value = median(pglabgro, na.rm = TRUE))

save_plot(plot_data, "wage_ec.png")

plot_data <- list()
plot_data$`High C-AIOE` <- analysis |>
  filter(c_aioe > median(c_aioe, na.rm = TRUE)) |>
  group_by(syear) |>
  summarise(value = median(pglabgro, na.rm = TRUE))

plot_data$`Low C-AIOE` <- analysis |>
  filter(c_aioe <= median(c_aioe, na.rm = TRUE)) |>
  group_by(syear) |>
  summarise(value = median(pglabgro, na.rm = TRUE))

save_plot(plot_data, "wage_caioe.png")


plot_data <- list()
plot_data$HELC <- analysis |>
  filter(helc == 1) |>
  group_by(syear) |>
  summarise(value = median(pglabgro, na.rm = TRUE))

plot_data$Control <- analysis |>
  filter(helc == 0) |>
  group_by(syear) |>
  summarise(value = median(pglabgro, na.rm = TRUE))

save_plot(plot_data, "wage_helc.png")

plot_data <- list()
plot_data$High.AIOE <- analysis |>
  filter(aioe > median(aioe, na.rm = TRUE)) |>
  group_by(syear) |>
  summarise(value = median(pglabgro, na.rm = TRUE))

plot_data$Low.AIOE <- analysis |>
  filter(aioe <= median(aioe, na.rm = TRUE)) |>
  group_by(syear) |>
  summarise(value = median(pglabgro, na.rm = TRUE))

save_plot(plot_data, "wage_aioe.png")