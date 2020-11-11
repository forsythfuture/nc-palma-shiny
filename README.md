# North Carolina Palma ratios

The Palma ratio is a measure of income inequality and represents the ratio of incomes shares of the top 10% to the bottom 40%. It is derived by summing all incomes in the top 10% and bottom 40%, and then dividing the the summed income of the top 10% by the bottom 40%.

Income data comes from the US Census's [Public Use Microdata Areas (PUMA)](https://www.census.gov/programs-surveys/acs/data/pums.html). Estimated federal and state income taxes, and FICA (payroll) taxes were removed from income. These taxes were estimated using the NBER's [tax simulation model](https://users.nber.org/~taxsim/taxsim27/).

We also adjuted post-tax household incomes based on household size. For the adjustments, we used the US Census's [equivalency scale](https://www.census.gov/topics/income-poverty/income-inequality/about/metrics/equivalence.html).

This repo contains all the files necessary to run the R shiny app located at: https://shanejorr.shinyapps.io/nc-palma-shiny/.  The `app.R` file is the only file the needs to be ran.
