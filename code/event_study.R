# ============================================================================ #
# ----------------------- PARALLEL TRENDS EVENT-STUDY ------------------------ #
# ============================================================================ #

library(dplyr)
library(tibble)
library(readr)
library(fixest)

source("code/utils.R")
source("code/did_base.R")

event_study_vars <- list(
list(var = "c_aioe_z",   label = "C-AIOE (z-score)"),
  list(var = "high_caioe", label = "Above-Median C-AIOE")
)
var_labels <- setNames(map_chr(event_study_vars, "label"), map_chr(event_study_vars, "var")) # nolint

fe_vars     <- c("pid", "syear")
fe_vars_alt <- c("isco08", "syear")

include_controls <- TRUE
control_vars <- c("age_sq", "factor(pgausb)")

primary_var          <- "high_caioe"
alpha_sig            <- 0.05

dir.create(DIRS$plots,  recursive = TRUE, showWarnings = FALSE)
dir.create(DIRS$tables, recursive = TRUE, showWarnings = FALSE)

analysis <- load_did_data(FALSE)

fit_event_study <- function(data, treat_var, ref = treatment_year,
                             fe = fe_vars, controls = character(0)) {
  rhs <- c(sprintf("i(syear, %s, ref = %d)", treat_var, ref), controls)
  fml <- as.formula(paste(
    "log_wage ~", paste(rhs, collapse = " + "),
    "|", paste(fe, collapse = " + ")
  ))
  feols(fml, data = data, cluster = ~isco08)
}

tidy_event_coefs <- function(model, treat_var, ref = treatment_year) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  names(ct) <- c("estimate", "std.error", "statistic", "p.value", "term")

  ci <- as.data.frame(confint(model))
  ci$term <- rownames(ci)
  names(ci)[1:2] <- c("conf.low", "conf.high")

  pattern <- paste0("^syear::([0-9]{4}):", treat_var, "$")
  ct |>
    left_join(ci[c("term", "conf.low", "conf.high")], by = "term") |>
    filter(grepl(pattern, term)) |>
    mutate(
      year = as.integer(sub(pattern, "\\1", term)),
      event_time = year - ref,
      period = if_else(event_time < 0, "Pre-treatment", "Post-treatment")
    ) |>
    arrange(event_time)
}


# ── Plotting (base R graphics -- no ggplot2, no plot titles) ─────────────────

save_event_study_plot <- function(coef_df, path, width = 7, height = 4.5) {
  ylim <- range(c(coef_df$conf.low, coef_df$conf.high), na.rm = TRUE)
  png(path, 1600, 1200, pointsize = 50) 
  if (.Platform$OS.type == "windows") {
    par(family = "A", mar = c(5, 5, 1, 2))
  } else {
      par(mar = c(5, 5, 1, 2))
  }
  plot(
    coef_df$event_time, coef_df$estimate, type = "n",
    xlab = sprintf("Years relative to treatment (%d)", treatment_year),
    ylab = "Year x exposure coefficient (95% CI)",
    ylim = ylim, main = ""
  )
  abline(h = 0, lty = 2, col = "grey50")
  abline(v = -0.5, lty = 3, col = "grey30")

  # Construct CI handlebars
  cap_width <- 0.08
  segments(
    coef_df$event_time, coef_df$conf.low,
    coef_df$event_time, coef_df$conf.high,
    col = 1, lwd = 1.4
  )
  segments(
    coef_df$event_time - cap_width, coef_df$conf.low,
    coef_df$event_time + cap_width, coef_df$conf.low,
    col = 1, lwd = 1.4
  )
  segments(
    coef_df$event_time - cap_width, coef_df$conf.high,
    coef_df$event_time + cap_width, coef_df$conf.high,
    col = 1, lwd = 1.4
  )
  pt_col <- ifelse(coef_df$p.value < alpha_sig, COLORS[2], COLORS[1])
  points(coef_df$event_time, coef_df$estimate, pch = 19, col = pt_col, cex = 1.2) # nolint
  legend(
    "topleft", legend = c("p < 0.05", "p >= 0.05"), pch = 19,
    col = c(COLORS[2], COLORS[1]), bty = "n", cex = 0.8
  )
  dev.off()
}

models      <- list()
all_coefs   <- list()
diagnostics <- list()

for (spec in event_study_vars) {
  tv  <- spec$var
  lbl <- spec$label
  message(sprintf("-- Event study: %s --", lbl))

  m  <- fit_event_study(analysis, tv,
    controls = if (include_controls) control_vars else character(0)
  )
  cd <- tidy_event_coefs(m, tv)

  save_event_study_plot(cd, file.path(DIRS$plots, sprintf("eventstudy_%s.png", tv))) # nolint

  models[[tv]]      <- m
  all_coefs[[tv]]   <- cd
}