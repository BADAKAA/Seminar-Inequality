# ── Paths and Directories ─────────────────────────────────────────────────────
PROJECT_ROOT <- normalizePath(getwd(), winslash = "/")                  # nolint

DIRS <- list(                                                           # nolint
  out =           file.path(PROJECT_ROOT, "out"),
  data =          file.path(PROJECT_ROOT, "data"),
  plots =         file.path(PROJECT_ROOT, "out", "plots"),
  tables =        file.path(PROJECT_ROOT, "out", "tables"),
  soep =          file.path(PROJECT_ROOT, "data/private/soep")
)

PATHS <- list(                                                          # nolint
  aioe =          file.path(DIRS$data, "aioe.csv"),
  caioe_isco =    file.path(DIRS$data, "private", "caioe_isco.csv"),
  caioe_soc =     file.path(DIRS$data, "private", "caioe_soc.csv"),
  soep =          file.path(DIRS$out, "soep_transformed.rds"),
  crosswalk =     file.path(DIRS$data, "soc_isco_1to1.csv"),
  isco_88to08 =   file.path(DIRS$data, "isco_88to08.csv"),
  job_zones =     file.path(DIRS$out, "onet_job_zones.rds"),
  work_context =  file.path(DIRS$out, "onet_work_context.rds"),
  employment =    file.path(DIRS$data, "employment.csv"),
  kldb_isco =     file.path(DIRS$data, "kldb_isco.csv")
)

# ── Plot Font ────────────────────────────────────────────────────────────────
if (.Platform$OS.type == "windows") {
  windowsFonts(A = windowsFont("TeXGyreAdventor"))
}

COLORS <- c("#1A476F", "#90353B")                                   # nolint

# ── Occupation code standardisation ──────────────────────────────────────────

standardize_isco <- function(x) {
  x <- as.character(x)
  x <- gsub("[^0-9]", "", x)
  x[nchar(x) < 4] <- NA_character_
  x <- substr(x, 1, 4)
  stringr::str_pad(x, width = 4, side = "left", pad = "0")
}

# ── SOEP data loader ──────────────────────────────────────────────────────────
load_soep <- function() {
  path <- PATHS$soep
  if (!file.exists(path)) {
    stop(
      "Transformed SOEP dataset not found at: ", path,
      "\nRun transform_soep.R first."
    )
  }
  message("Loading transformed SOEP data ...")
  dat <- readRDS(path)
  message(sprintf(
    "  %d person-year observations | %d persons | years %d–%d",
    nrow(dat),
    length(unique(dat$pid)),
    min(dat$syear, na.rm = TRUE),
    max(dat$syear, na.rm = TRUE)
  ))
  dat
}

# ── CAIOE utility ──────────────────────────────────────────────────────────
library(readr)
library(dplyr)

load_caioe <- function() {
  read_csv(
    PATHS$caioe_isco,
    show_col_types = FALSE,
    col_types = cols(isco08 = col_character())
  ) |>
    select(isco08, le, helc, hehc, complementarity_theta, c_aioe, aioe_all) |>
    rename(theta = complementarity_theta) |>
    mutate(
      aioe        = aioe_all,
      le          = as.logical(le),
      helc        = as.logical(helc),
      hehc        = as.logical(hehc),
      high_aioe   = as.logical(aioe > median(aioe, na.rm = TRUE)),
      high_caioe  = as.logical(c_aioe > median(c_aioe, na.rm = TRUE)),
      c_aioe_z    = as.numeric(scale(c_aioe)),
      aioe_z      = as.numeric(scale(aioe_all)),
      theta_z     = as.numeric(scale(theta)),
      isco08      = standardize_isco(isco08),
      low_compl   = as.logical(theta < median(theta, na.rm = TRUE)),
    )
}

# ── Table utility ──────────────────────────────────────────────────────────
add_table_label <- function(tex_path, label) {
  tex <- readLines(tex_path)
  tex <- sub("\\\\begin\\{table\\}(?!\\[)",  "\\\\begin{table}[!ht]", tex, perl = TRUE) # nolint
  tex <- sub("(caption=\\{[^}]+\\})",
    sprintf("\\1,\nlabel=\\{%s\\}", label),
    tex
  )
  writeLines(tex, tex_path)
}