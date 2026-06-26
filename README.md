# Seminar Paper
This repository is associated with my paper in the seminar "Empirical Research on Inequality and Redistribution".

## Getting Started

A successful replication of my results requires data which is not tracked by version control for legal or performance reasons. To successfully execute all the scripts, please download the following data sources:

| Source   | Status   | Destination | Last Updated |
| -------- | -------- | ----------- | ------------ |
| [AIOE](https://github.com/AIOE-Data/AIOE) (by occupation) | ✅ included | data/aioe.csv | 12.06.2026 |
| [C-AIOE](https://www.imf.org/en/publications/wp/issues/2023/10/04/labor-market-exposure-to-ai-cross-country-differences-and-distributional-implications-539656) | ❌ not included | data/private/caioe_isco.csv | 12.06.2026 |
| [SOEP](https://www.diw.de/de/diw_01.c.412809.de/sozio-oekonomisches_panel__soep.html)* (.rds) | ❌ not included | data/private/soep | 12.06.2026 |

***biobirth.rds** and **pgen.rds** are the only SOEP files required for this project. 

Please contact [Carlo Pizzinelli](https://sites.google.com/site/carlopizzinelli/) or his co-authors for access to the C-AIOE.

## Project Structure

All script files are called in `code/script.R`. It will load all required libraries and validate the existence of the data sources. *Do not* call other script files manually without first running `code/setup.R`.

All tables and plots can be found in the `out` folder after `script.R` has been executed successfully.