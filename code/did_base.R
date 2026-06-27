# ============================================================================ #
# ------------------------- DID SETUP AND UTILITIES -------------------------- #
# ============================================================================ #

library(dplyr)
source("code/utils.R")

start_year <- 2004
end_year <- 2024
treatment_year <- 2015

missing_year_tolerance <- 0

include_marginal_employment <- FALSE

if (include_marginal_employment) {
  allowed_pgemplst <- c(1, 2, 4, 5, 7)
} else {
  allowed_pgemplst <- c(1, 2, 5)
}

treatment_label <- "Above-Median AI Exposure $\\times$ Post"
coef_map <- c(
  "int_high_aioe" = treatment_label,
  "int_high_caioe" = treatment_label,
  "int_low_compl" = "Low-Complementarity $\\times$ Post",
  "int_helc" = "HELC $\\times$ Post",
  "int_caioe_z" = "C-AIOE $\\times$ Post",
  "int_aioe_z" = "AIOE $\\times$ Post",
  "int_theta_z" = "Complementarity $\\times$ Post",
  "int_aioe_compl_post" = "Triple DiD",
  "int_aioe_theta_post" = "Triple DiD",
  "age" = "Age",
  "age_sq" = "Age$^2$",
  "I(pgausb)" = "Type of education",
  "pgerwzeit" = "Firm tenure",
  "part_time" = "Part-time employment"
)

gof_map <- tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "Observations", 0,
  "r.squared", "$R^2$", 3,
  "FE: isco08", "ISCO-08 FE", 0,
  "FE: pid", "Individual FE", 0,
  "FE: syear", "Survey Year FE", 0,
  "r2.within", "Within $R^2$", 3
)

remove_unemployed <- function(dat) {
  unemployed_pids <- dat |>
    filter(syear > treatment_year) |>
    filter(unemployed) |>
    select(pid) |>
    distinct() |>
    pull(pid)
  dat <- dat |> filter(!pid %in% unemployed_pids)
  dat
}

load_did_data <- function(fix_exposure = TRUE) {
  message("Loading SOEP data ...")
  soep <- load_soep() |>
  filter(pgemplst %in% allowed_pgemplst) |>
  filter(syear >= start_year & syear <= end_year)
  if (fix_exposure) {
    message("Calculating Baseline ISCO-08 ...")
    soep_baseline <- soep |>
      rename(dynamic_isco08 = isco08)

    baseline_isco <- soep_baseline |>
      filter(syear <= treatment_year) |> # avoid endogeneity (self-selection)
      filter(syear >= treatment_year - missing_year_tolerance) |>
      group_by(pid) |>
      filter(all(pgemplst %in% c(1, 2))) |>
      filter(!is.na(dynamic_isco08)) |>
      arrange(abs(syear - treatment_year), .by_group = TRUE) |>
      slice(1) |>
      ungroup() |>
      select(pid, dynamic_isco08) |>
      rename(baseline_isco08 = dynamic_isco08)

    message("Merging Baseline SOEP, ISCO-08 and C-AIOE ...")
    soep <- soep_baseline |>
      left_join(baseline_isco, by = "pid") |>
      rename(isco08 = baseline_isco08) |>
      filter(!is.na(isco08)) |>
      ungroup()
  } else {
    soep <- soep |> filter(!is.na(isco08))
  }
  soep |>
    left_join(load_caioe(), by = c("isco08" = "isco08")) |>
    filter(
      !is.na(log_wage)
    ) |>
    # ── Construct Post indicator and all interaction terms ──────────────────────
    mutate(
      post = as.integer(syear > treatment_year),
      int_aioe_z = aioe_z * post,
      int_caioe_z = c_aioe_z * post,
      int_theta_z = theta_z * post,
      int_aioe_theta_post = aioe_z * theta_z * post,
      int_high_aioe = high_aioe * post,
      int_high_caioe = high_caioe * post,
      int_helc = helc * post,
      int_low_compl = low_compl * post,
      int_aioe_compl_post = high_aioe * low_compl * post,
    )
}