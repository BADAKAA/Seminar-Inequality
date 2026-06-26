source("code/did_base.R")
library(dplyr)
library(tinytable)

output_path <- file.path(DIRS$tables, "soep_summary.tex")

soep_tab <- load_did_data()

tab <- data.frame(
  Characteristic = c(
    "Unique individuals",
    "Person-year observations",
    "Survey years",
    "Age Range",
    "Mean Age",
    "Male Individuals",
    "Mean Monthly gross wage in \\EUR{}",
    "Mean Weekly working hours",
    "Full-time employment share"
),
  `Value (SD)` = c(
    format(n_distinct(soep_tab$pid), big.mark = ","),
    format(nrow(soep_tab), big.mark = ","),
    "2004--2024",
    "25--60",
    sprintf("%.2f (%.2f)",
            mean(soep_tab$age, na.rm = TRUE),
            sd(soep_tab$age, na.rm = TRUE)),
    sprintf("%.1f\\%%",
            100 * mean(soep_tab$male, na.rm = TRUE)),
    sprintf("%.0f (%.0f)",
            mean(soep_tab$pglabgro[soep_tab$pglabgro > 0], na.rm = TRUE),
            sd(soep_tab$pglabgro[soep_tab$pglabgro > 0], na.rm = TRUE)),
    sprintf("%.2f (%.2f)",
            mean(soep_tab$pgtatzeit[soep_tab$pgtatzeit > 0], na.rm = TRUE),
            sd(soep_tab$pgtatzeit[soep_tab$pgtatzeit > 0], na.rm = TRUE)),
    sprintf("%.1f\\%%",
            100 * mean(soep_tab$pgemplst == 1, na.rm = TRUE))
  ),
  check.names = FALSE
)

tt(tab,
   caption = "Summary of the SOEP Analysis Sample") |>
  save_tt(output_path, overwrite = TRUE)

add_table_label(output_path, "tab:soep")

sprintf("Exluded, %d observations with missing working hours",
    sum(soep_tab$pgtatzeit <= 0, na.rm = TRUE)
)