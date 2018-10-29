##################################################################
#
# This script creates income and shapefiles datasets and saves them
# as R objects. This allows for quicker import
#
##################################################################

library(tidyverse)
library(DBI)
library(data.table)
library(tigris)
#library(gridExtra)
#library(grid)

source('palma_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "pums_db.db")

### create dataset of PUMA level incomes
### used for plotting income distributions

# import household tax liabilities for the given year
taxes <- readRDS('nc_tax_liability.Rda')

# initialize dataframe
incomes <- data.frame()

for (yr in seq(2015, 2016, 2017)) {
  
  incomes <- household_incomes_dist(con, yr, 37, taxes) %>%
      bind_rows(incomes, .)
}

# don't need serial number or state, since state is NC for all
incomes <- incomes %>%
  filter(pre_tax_income >= 500) %>%
  select(-SERIALNO, -ST, -pre_tax_income)

# adjust incomes for inflation
incomes_post <- inflation_adjust(incomes, wages_col = post_tax_income, year_adjust = 2017, error = FALSE) %>%
  select(post_tax_income)%>%
  .[[1]]

# add inflation adjusted columns to dataset
incomes$post_tax_income <- incomes_post

# replace PUMA number with name
# import list of PUMA numbers and names, and merge with income dataset
puma_names <- read_csv('puma_counties.csv') %>%
  select(puma12, PUMA12name) %>%
  distinct()

# vector of counties that have PUMAs all within the county
# allows us to filter for PUMA by county
counties <- c('Alamance', 'Brunswick', 'Buncombe', 'Cabarrus', 'Catawba', 'Craven', 'Cumberland', 'Davidson',
              'Durham', 'Forsyth', 'Gaston', 'Guilford', 'Harnett', 'Iredell', 'Johnston', 'Mecklenburg',
              'Orange', 'Pitt', 'Randolph', 'Rowan', 'Wake', 'Wayne')

incomes <- left_join(incomes, puma_names, by = c('PUMA' = 'puma12')) %>%
  # remove PUMA numbers
  select(-PUMA) %>%
  # rename puma description column
  rename(PUMA = PUMA12name) %>%
  # only keep counties that have PUMAs filly within counties
  filter(group %in% !!counties)
  
# saveRDS(incomes, file="incomes.Rda")

# read in full palma data, but only keep needed counties
palma_full <- read_csv('palmas_full.csv') %>%
  mutate_at(vars(estimate, se, moe, cv), funs(round(., 2))) %>%
  filter(!(type == 'county' & !(subtype %in% counties))) %>%
  select(-se, -cv)

# saveRDS(palma_full, 'palma_full.Rda')

# import shapefiles and create datasets
nc_puma <- pumas(state = 'NC', cb = TRUE)
# saveRDS(nc_puma, 'nc_puma.Rda')
nc_county <- counties(state = 'NC', cb = TRUE)
# saveRDS(nc_county, 'nc_county.Rda')
