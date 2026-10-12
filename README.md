# Vehicle Fuel Efficiency Analyzer

An R Shiny dashboard for an SDS mini project. It demonstrates six statistical methods using a reproducible **simulated educational dataset**.

## Modules
1. P4: one-sided one-sample z approximation for mean mileage > 18 km/L.
2. P8: exact two-sided sign test for median mileage = 18 km/L.
3. P6: simple linear regression predicting mileage from engine capacity.
4. P3: 95% confidence interval for the population mean (t interval by default; optional z interval if population SD is known).
5. P9: Mann–Whitney/Wilcoxon rank-sum test comparing petrol and diesel mileage distributions.
6. P7: chi-square test of independence between fuel type and low/high mileage category.

## Data and limitations
`generate_data.R` creates 240 simulated records with a fixed random seed. The data are not actual measurements from a manufacturer or a published source. The simulation intentionally embeds associations for teaching and demonstration. Do not use the results to validate a real manufacturer's claim.

The assignment requests a z-test. A textbook one-sample z-test assumes known population standard deviation. Since the simulation does not supply a real known population SD, the app labels its use of sample SD as an approximation and notes that a one-sample t-test is usually appropriate when the population SD is unknown.

## Requirements
R and RStudio, plus the R packages `shiny`, `ggplot2`, `dplyr`, `bslib`, `DT`, `bsicons`, and `scales`.

## Run locally
1. Open this folder as an RStudio project (or set the working directory to this folder).
2. If packages are missing, run:
   ```r
   install.packages(c("shiny", "ggplot2", "dplyr", "bslib", "DT", "bsicons", "scales"))
   ```
3. Run `source("generate_data.R")` once to create `data/vehicles.csv`.
4. Open `app.R` and click **Run App**.

## Deployment
Publish from RStudio using the `rsconnect` package and a shinyapps.io account. Follow the current instructions at https://www.shinyapps.io/ and do not commit account tokens or secrets to GitHub.

## Suggested repository contents
- `app.R`
- `generate_data.R`
- `data/vehicles.csv`
- `report/project-report-template.md`
- `README.md`
- `.gitignore`
