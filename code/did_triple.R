# ============================================================================ #
# ---------------- TRIPLE DIFFERENCE-IN-DIFFERENCES ANALYSIS ----------------- #
# ============================================================================ #

library(tidyverse)
library(fixest)
library(modelsummary)

source("code/utils.R")
source("code/did_base.R")

console_output <- TRUE

table_captions <- c(
  "AI Exposure, Complementarity, and Log Wages",
  "Difference-in-Differences with ISCO-08 Fixed Effects",
  "Difference-in-Differences with Z-Standardized Exposure and Complementarity"
)
table_paths <- c(
  file.path(DIRS$tables, "did.tex"),
  file.path(DIRS$tables, "did_isco_fe.tex"),
  file.path(DIRS$tables, "did_z.tex")
)
table_labels <- c(
  "tab:did",
  "tab:did_isco_fe",
  "tab:did_z"
)

fixed_effects <- list(
  ~ pid + syear,
  ~ isco08 + syear,
  ~ pid + syear
)
did_controls <- list(
  ~ age_sq + factor(pgausb), # optional: pgexpft, pgexppt
  ~ age + age_sq + factor(pgausb),
  ~ age_sq + factor(pgausb)
)
independent_vars <- list(
  list(
    c("int_high_aioe"),
    c("int_high_caioe"),
    c("int_high_aioe", "int_low_compl", "int_aioe_compl_post")
  ),
  list(
    c("int_high_aioe"),
    c("int_high_caioe"),
    c("int_high_aioe", "int_low_compl", "int_aioe_compl_post")
  ),
  list(
    c("int_aioe_z"),
    c("int_caioe_z"),
    c("int_aioe_z", "int_theta_z", "int_aioe_theta_post")
  )
)

table_label <- "tab:results"
table_notes <- list(
  paste(
    "Standard errors clustered at the ISCO-08 level.",
    "Post = 1 for survey waves 2016 onward (treatment year: 2015).",
    "AI Occupational Exposure (AIOE) follows \\cite{felten2021occupational}.",
    "Low-Complementarity is a binary indicator for below-median AI complementarity.", # nolint
    "The triple interaction term (AIOE $\\times$ Low-Complementarity $\\times$ Post) is the coefficient of interest ($\\hat{\\beta}_3$).", # nolint
    sep = "\\\\\n"
  ), paste(
    "Standard errors clustered at the ISCO-08 level.",
    sep = "\\\\\n"
  ), paste(
    "Standard errors clustered at the ISCO-08 level.",
    sep = "\\\\\n"
  )
)

analysis <- load_did_data()

message(sprintf(
  " Analysis sample: %d person-year observations, %d individuals",
  nrow(analysis),
  n_distinct(analysis$pid)
))

message("Running Regressions ...")

run_regression <- function(i) {
  fixed_effects <- fixed_effects[[i]]
  path <- table_paths[i]
  caption <- table_captions[i]
  label <- table_labels[i]
  controls <- did_controls[[i]]
  x <- independent_vars[[i]]
  aioe_vars <- x[[1]]   # nolint
  caioe_vars <- x[[2]]  # nolint
  triple_vars <- x[[3]] # nolint
  did_aioe <- feols(
    log_wage ~ .[aioe_vars] + .[controls] | .[fixed_effects],
    data    = analysis,
    cluster = ~isco08
  )
  did_caioe <- feols(
    log_wage ~ .[caioe_vars] + .[controls] | .[fixed_effects],
    data    = analysis,
    cluster = ~isco08
  )
  did_triple <- feols(
    log_wage ~ .[triple_vars] + .[controls] | .[fixed_effects],
    data = analysis,
    cluster = ~isco08
  )

  modelsummary(
    list(
      "(1) AIOE"          = did_aioe,
      "(2) C-AIOE"        = did_caioe,
      "(3) Triple DiD"    = did_triple
    ),
    coef_map = coef_map,
    gof_map = gof_map,
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    fmt = 4,
    escape = FALSE,
    title = caption,
    notes = table_notes,
    output = path
  )
  add_table_label(path, label)
  if (!console_output) {
    return()
  }
  print(etable(
    did_aioe, did_caioe, did_triple,
    title = "DiD Estimates: AI Exposure and Log Wages",
    keep = c(
      "int_high_caioe", "int_high_aioe",
      "int_low_compl", "int_aioe_compl_post",
      "int_caioe_z", "int_aioe_z",
      "int_theta_z", "int_aioe_theta_post"
    ),
    se.below = TRUE,
    digits = 4
  ))
}

for (i in seq_along(table_paths)) {
  run_regression(i)
}

message("DiD results saved to ", DIRS$tables)