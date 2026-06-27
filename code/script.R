# ============================================================================ #
#                        INEQUALITY AND REDISTRIBUTION                         #
# ---------------------------------------------------------------------------- #
#
# This script file is the main entry point that executes all other scripts.
# Author: Bassit Agbéré
#
# ============================================================================ #

source("code/setup.R")
if (!file.exists(PATHS$soep)) source("code/transform_soep.R")
source("code/plot_income.R")
source("code/event_study.R")
source("code/did_triple.R")