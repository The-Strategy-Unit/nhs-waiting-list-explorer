# NHS Waiting list explorer

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)


## Requirements

The following R packages are needed to run the full pipeline

requirements = c("dplyr", "httr", "htmltools",
 "htmlwidgets", "reactable", "readxl", "rvest", "tidyr")

install.packages(requirements)

To install Quarto, follow these steps:
1. Visit the Quarto website: https://quarto.org/
2. Download the installer for your operating system (Windows, macOS, or Linux).
3. Run the installer and follow the on-screen instructions.
4. Verify the installation by running `quarto check` in your terminal.
5. For R integration, ensure you have the latest version of R and RStudio.
6. You can also install Quarto from R using:
    install.packages("quarto")

## Running the package:

To run the package, execute the following scripts in order:
1. prep_national_table.R
2. prep_specialties_table.R
3. prep_waiting_times_table.R
4. dashboard.qmd

**Note:** Step 1 will take some time (e.g. 15-30mins) to download and format NHS data.