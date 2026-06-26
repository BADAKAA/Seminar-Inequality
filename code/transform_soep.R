# ============================================================================ #
#                         SOEP DATA TRANSFORMATION                             #
# ---------------------------------------------------------------------------- #
#
# Loads, merges and transforms the raw SOEP files into a single panel dataset.
#
# Output
#   single file consumed by all downstream scripts (path see utils.R)
#
# Column inventory of the output dataset:
#   pid           — person identifier (join key)
#   syear         — survey year
#   age           — derived integer age  (syear − gebjahr)
#   age_sq        — age squared (regression control)
#   male          — binary indicator for sex (1 = male, 0 = female)
#   pglabgro      — gross monthly labor income (€)
#   log_wage      — log(pglabgro), NA where pglabgro <= 0
#   pgisco08      — ISCO-08 four-digit occupation code (join key for C-AIOE)
#   pgausb        — education level
#   voc_train     — binary indicator for vocational training (pgpbbil01)
#   pgtatzeit     — actual weekly working hours (control variable; if present)
#   pgerwzeit     — tenure in current job (years)
#   pgexpft       — full-time working experience (years)
#   pgexppt       — part-time working experience (years)
#   part_time     — binary indicator for part-time employment
#   unemployed    — binary indicator for unemployment
#                   (1 = unemployed, 0 = employed)
#   pgemplst      — employment status
#                   (1 = full-time, 2 = part-time, 4 = marginal, 5 = unemployed)
# ============================================================================ #

library(dplyr)
source("code/utils.R")

start_year <- 2000
end_year   <- 2024

min_age <- 25
max_age <- 60

exclude_self_employed <- FALSE

## Verify inputs
required_files <- c("pgen.rds", "biobirth.rds")
missing_files  <- required_files[!file.exists(file.path(DIRS$soep, required_files))] # nolint
if (length(missing_files) > 0) {
  stop(
    "Required SOEP files missing from '", DIRS$soep, "': ",
    paste(missing_files, collapse = ", ")
  )
}

message("Loading biobirth.rds ...")
biobirth <- readRDS(file.path(DIRS$soep, "biobirth.rds")) |>
  select(pid, gebjahr, sex) |>
  mutate(across(everything(), unclass)) |>
  filter(gebjahr >= 0, sex > 0, sex < 3)
biobirth$male <- biobirth$sex == 1
biobirth$sex <- NULL

message(sprintf("  biobirth: %d persons loaded", nrow(biobirth)))

message("Loading pgen.rds ...")
pgen <- readRDS(file.path(DIRS$soep, "pgen.rds")) |>
  select(
    "pid", "syear",
    "pglabgro",
    "pgisco08", "pgisco88", "pgstib",
    "pgerwzeit", "pgexpft", "pgexppt",
    "pgausb", "pgemplst", "pgtatzeit", "pgpbbil01"
  ) |>
  mutate(across(everything(), unclass)) |>
  mutate(pgtatzeit = ifelse(pgtatzeit < 0, NA, pgtatzeit)) |>
  filter(syear >= start_year)

pgen$voc_train <- ifelse(pgen$pgpbbil01 < 0, NA, pgen$pgpbbil01 > 0)
pgen$pgpbbil01 <- NULL

message("Transforming isco88 → isco 08 to access pre-2013 observations ...")
isco_conversion <- read.csv(PATHS$isco_88to08) |>
  select(ISCO.88.code, ISCO.08.code) |>
  rename(isco88 = "ISCO.88.code", isco08 = "ISCO.08.code")

pgen$pgisco08[pgen$pgisco08 <= 0] <- isco_conversion$isco08[
  match(pgen$pgisco88[pgen$pgisco08 <= 0], isco_conversion$isco88)
]

pgen$pgisco88 <- NULL
pgen <- pgen |>
  rename(isco08 = "pgisco08") |>
  mutate(isco08 = standardize_isco(isco08))

message(sprintf("  pgen: %d person-year observations loaded", nrow(pgen)))

message("Merging datasets ...")

pgen <- pgen[!duplicated(pgen[, c("pid", "syear")]), ]
biobirth <- biobirth[!duplicated(biobirth$pid), ]

panel <- merge(pgen, biobirth, by = "pid", all.x = TRUE)

message(sprintf("  After merge: %d person-year observations", nrow(panel)))


## Add derived variables
panel$age    <- panel$syear - panel$gebjahr
panel$age_sq <- panel$age^2

panel$unemployed <- panel$pgemplst == 5
panel$part_time <- panel$pgemplst == 2

panel$log_wage <- ifelse(
  !is.na(panel$pglabgro) & panel$pglabgro > 0,
  log(panel$pglabgro),
  NA_real_
)

## Apply sample restrictions
n_raw <- nrow(panel)
message(sprintf("\nApplying sample restrictions (starting from %d rows) ...", n_raw)) # nolint

panel <- filter(panel, syear >= start_year, syear <= end_year)
message(sprintf(
  "  [1] Year filter %d–%d: %d rows remaining",
  start_year, end_year, nrow(panel)
))

panel <- filter(panel, !is.na(age) & age >= min_age & age <= max_age)
message(sprintf(
  "  [2] Age filter %d–%d:  %d rows remaining",
  min_age, max_age, nrow(panel)
))

## (pgstib 410–433 covers all self-employment categories:
# freelancers, employers, contributing family workers)
if ("pgstib" %in% names(panel) && exclude_self_employed) {
  is_self_empl <- !is.na(panel$pgstib) &
    panel$pgstib >= 410L &
    panel$pgstib <= 433L
  panel <- panel[!is_self_empl, , drop = FALSE]
  message(sprintf(
    "  [4] Exclude self-employed (pgstib 410–433): %d rows remaining",
    nrow(panel)
  ))
}

message(sprintf(
  "\nTotal: %d → %d rows retained (%.1f%%)",
  n_raw, nrow(panel), 100 * nrow(panel) / max(n_raw, 1)
))


## Final ordering and saving
panel <- panel[order(panel$pid, panel$syear), , drop = FALSE]
rownames(panel) <- NULL

output_path <- PATHS$soep
saveRDS(panel, output_path, compress = "xz")
output_info <- file.info(output_path)

n_persons <- length(unique(panel$pid))
n_obs     <- nrow(panel)
n_years   <- length(unique(panel$syear))

message(sprintf(
  "\n✓ Transformation complete.\n  Output  : %s (%.1f MB)\n  Persons : %d unique individuals\n  Obs     : %d person-years\n  Years   : %d (%d – %d)", # nolint
  output_path,
  output_info$size / 1024^2,
  n_persons,
  n_obs,
  n_years,
  min(panel$syear, na.rm = TRUE),
  max(panel$syear, na.rm = TRUE)
))

cat("\nVariable completeness:\n")
cat(sprintf("  %-16s  %8s  %8s\n", "Variable", "Non-NA", "% complete"))
cat(strrep("-", 40), "\n")
for (v in names(panel)) {
  n_valid <- sum(!is.na(panel[[v]]))
  cat(sprintf("  %-16s  %8d  %7.1f%%\n", v, n_valid, 100 * n_valid / n_obs))
}