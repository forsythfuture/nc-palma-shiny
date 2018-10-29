#######################################################
#
# This file contains functions to calculate the Palma
#
#
# The functions are divided into various processes:
#   - calculate palma for one year
#   - calcualte Palma for weights
#   _ calculate the palma for mutliple years
#
########################################################
library(DBI)
library(tidyverse)


palma_cal <- function (income_vec) {
  
  # This function takes as input a vector of replicate weighted household incomes
  # and outputs the Palma
  
  # total number of households; used to find percentiles
  n_income <- length(income_vec)
  
  # total earnings of bottom 40% and top 10%
  bottom <- income_vec[1:floor(n_income*.40)] %>% sum()
  top <- income_vec[floor(n_income*.90):n_income] %>% sum()

  return(top / bottom)
  
}

palma_st_error <- function(palmas_full) {
  
  # This function takes as input a dataframe of Palma values; each column is a different weght and each row
  # is a different geographic unit
  # its output is the standard error of the Palma
  # reference: 
  #   Public Use Microdata Sample (PUMS), Accuracy of the Data (2016), Census Bureau, p. 16
  
  # squared difference
  sq_diff <- lapply(palmas_full[3:length(palmas_full)], function(x) (palmas_full[2] - x)^2) %>%
    as.data.frame() %>%
    rowSums()
    
  # multiply by 4/80 and take square root
  sq_diff <- sqrt((4/80)*sq_diff)
    
  return(sq_diff)
}


equivalence_scale <- function(num_adults, num_children) {
  
  # create function to adjust for age based on equivalence scale
  
  ifelse(# if num adults is one or two, and no children
    num_adults <= 2 & num_children == 0, num_adults^0.5,
    ifelse(# if single parent
      num_adults == 1 & num_children >= 1,
      (num_adults + 0.8 * 1 + 0.5 * (num_children-1))^0.7,
      # other families
      (num_adults + 0.5 * num_children)^0.7
    )
  )
}


house_incomes <- function(con, year, state = NA, area_code = NA) {
  
  # This function returns a dataframe of household incomes for the given year
  # taxes are subtracted from income; income is adjusted for household size
  # results from this function are used to calculate Palmas
  
  # import household tax liabilities for the given year
  taxes <- readRDS('nc_tax_liability.Rda') %>%
    filter(year == !!year)
  
  # create table names
  yr <-str_extract(as.character(year), '[0-9][0-9]$')
  house_table <- paste0('h_', yr)
  pop_table <- paste0('p_', yr)
  
  # establish connection to tables
  housing <- tbl(con, house_table)
  population <- tbl(con, pop_table)
  
  # import these PUMS variables
  house_vars <- c('TYPE', 'SERIALNO', 'PUMA', 'ST', 'HINCP')
  pop_vars <- c('SERIALNO', 'ST', 'AGEP') # population variables
  
  # import population data
  population <- population %>%
    select(!!pop_vars) %>%
    filter(if (!is.na(!!state)) ST == !!state) %>%
    select(-ST)
  
  house <- housing %>%
    select(!!house_vars) %>%
    # filter for state
    # we cannot filter for PUMA now because cannot use %in% operator until collected
    filter(if (!is.na(!!state)) ST == !!state,
           TYPE == 1, # housing units only
           (!is.na(HINCP) & HINCP >= 0)) %>% # positive household income
    select(-TYPE) %>%
    # merge with population data
    left_join(population, by = 'SERIALNO') %>%
    # convert age to either adult or femal
    mutate(AGEP = ifelse(AGEP >= 18, 'adult', 'child')) 
  
  # find number of adults and children for each family
  # will be merge with the primary dataset
  adults <- house %>%
    filter(AGEP == 'adult') %>%
    group_by(SERIALNO) %>%
    # count number of adults per household
    summarise(number_adults = n())
  
  child <- house %>%
    filter(AGEP == 'child') %>%
    group_by(SERIALNO) %>%
    # count number of children per household
    summarise(number_child = n())
  
  # merge primary dataset and number of adults and children
  house <- house %>%
    left_join(adults, by = 'SERIALNO') %>%
    left_join(child, by = 'SERIALNO') %>%
    collect()
  
  # filter for PUMA only if needed
  if (all(!is.na(area_code))) {
    
    house <- house %>%
      filter(PUMA %in% !!area_code)
    
  }
  house <- house %>%
    # convert SERIALNO to same data format as SERIALNO in taxes
    mutate(SERIALNO = as.numeric(SERIALNO)) %>%
    # add taxes
    left_join(., taxes, by = 'SERIALNO') %>%
    # calcualte income net of taxes
    mutate(income = HINCP - tax_liability,
           # if negative, make a small number so program works
           #post_tax_income = ifelse(.$post_tax_income < 0, 0.01, .$post_tax_income),
           # convert income to integer to save RAM
           income = as.integer(income)) %>%
    # replace NA values in numbers of aduls and children to 0
    mutate_at(vars(c('number_adults', 'number_child')), funs(replace_na(., 0))) %>%
    mutate(income = as.integer( income / equivalence_scale(number_adults, number_child) )) %>%
    select(SERIALNO, PUMA, ST, income) %>%
    # dataframe has one row for each person in the household;
    # but since only household variables were kept, all rows for the same family will be duplicates
    # remove duplicates, which will leave one row per household
    distinct() %>%
    # convert to datatable
    as.data.table()
  
  return(house)
  
}


palmas_complete <- function(con, year, level, state = NA, area_code = NA) {
  
  #### create palma from household incomes
  
  # run function to pull in household incomes
  household_incomes <- house_incomes(con, year, state = state, area_code = area_code) %>%
    filter(!is.na(income))
  
  # create groupings that are needed to calculate Palma
  household_incomes <- groupings(household_incomes, level, year)
  
  # start incorporating replciate weights
  
  # replciate weight variable names are lower case until 2017 and upper case starting in 2017
  weight_names <- ifelse(year >= 2017, 'WGTP', 'wgtp')
  # replicate weight variables
  house_weights <- c('WGTP', paste0(weight_names, seq(1, 80)))
  
  # create housing table name based on year
  tbl_name <- as.character(year) %>%
    str_extract(., '[0-9][0-9]$') %>%
    paste0('h_', .)
  
  # create connection with housing replicate weights
  weights_tbl <- tbl(con, tbl_name) %>%
    filter(ST == !!state) %>%
    select(SERIALNO, !!house_weights)
  
  # initialize list to store palma values
  # each item in list will contain dataframe of Palma values for all grouped
  # geographies and one replicate weight
  palmas <- list()
  
  # iterate through each replicate weight
  for (weight in house_weights) {
    
    # bring into memory dataframe with serialno and just one weight column
    wgt <- weights_tbl %>%
      select(SERIALNO, !!weight) %>%
      collect() %>%
      # convert to data table
      as.data.table()
    
    # this provides dataframe with income, weights, and geography
    # it can then be filtered by geography
    household_incomes_wgt <- wgt[household_incomes, on = 'SERIALNO']
    
    # change name of column so that it is the same of each iteration
    setnames(household_incomes_wgt, weight, 'wgt')
    
    palmas[[weight]] <- household_incomes_wgt[
      # filter out replicate weight values less than 1
      wgt > 0][
        # add additional rows based on the number of replicate weights
        rep(seq(.N), wgt), !"wgt"][
          # order based on income
          order(income, group)][
            # calculate palma for each group
            # use 'get' to convert string to object name
            ,.(palma_cal(income)), by = 'group']
    
  }
  
  rm(wgt)
  
  # create list of column names for dataframe that contains all Palmas
  col_names <- append('group', house_weights)
  
  # currently, each weight is in a different dataframe
  # combine them all into one dataframe where each column is a replicate weight and each row is a geography
  palmas_full <- reduce(palmas, left_join, by = 'group')
  # rename variables
  colnames(palmas_full) <- col_names

  # create dataframe
  complete <- data.frame(geography = palmas_full$group,
                         level = level,
                         year = year,
                         palma = palmas_full$WGTP,
                         se = palma_st_error(palmas_full))
  
  print(head(complete))
  
  return(complete)
  
}

palma_complete_years <- function(con, years, level, state = NA, area_code = NA) {
  
  # this function uses the palma_complete function to calculate the Palma for multiple years
  
  # initiate list to store all Palmas
  palma_full <- data.frame()
  
  for (yr in years) {
    
    print(yr)
    print(level)
    
    palma_full <- palmas_complete(con = con, 
                                year = yr, 
                                level = level, 
                                state = state, 
                                area_code = area_code) %>%
      bind_rows(palma_full, .)
    
  }
  
  return(palma_full)
  
}

household_incomes_dist <- function(con, year, state, tax_liabilities) {
  
  # This function calculates household incomes for use in distribution plot
  
  # create table names
  yr <-str_extract(as.character(year), '[0-9][0-9]$')
  house_table <- paste0('h_', yr)
  
  # establish connection to tables
  housing <- tbl(con, house_table)
  
  # import these PUMS variables
  house_vars <- c('TYPE', 'SERIALNO', 'PUMA', 'ST', 'HINCP', 'WGTP')
  
  house <- housing %>%
    select(!!house_vars) %>%
    # filter for state
    filter(if (!is.na(!!state)) ST == !!state,
           TYPE == 1, # housing units only
           (!is.na(HINCP) & HINCP >= 0)) %>% # positive household income
    select(-TYPE) %>%
    collect() %>%
    # create year column
    mutate(year = !!year,
           # ensure SERIALNO is same datatype
           SERIALNO = as.numeric(SERIALNO),
           # cut weight values by 2/3
           # this should not impact distrubution since all values are reduced by same percentage
           # use ceiling to avoid 0 values
           WGTP = as.integer( ceiling( WGTP * .33) )) %>%
    # add tax liabilities
    left_join(tax_liabilities, by = c('SERIALNO', 'year')) %>%
    # ensure any NA tax liabilities are zero
    mutate(tax_liability = replace_na(tax_liability, 0),
           # calculate post-tax income          
           post_tax_income = HINCP - tax_liability) %>%
    rename(pre_tax_income = HINCP) %>%
    # don't need tax liability
    select(-tax_liability)
  
  # add county names
  house <- groupings(house, 'county', year) %>%
    as.data.table()
  
  house <- house[
    # add additional rows based on the number of replicate weights
    rep(seq(.N), WGTP), !"WGTP"][
      # order based on income
      order(post_tax_income)]
  
  return(house)
}

groupings <- function(household_incomes, level, year) {
  
  # this function determines what groupings to use in calculating Palma
  # Input:
  #     household_incomes: dataframe of household incomes created by house_incomes
  #     levels: 'US', 'state', 'county', 'puma'
  
  # convert level to lowercase
  level <- str_to_lower(level)
  
  # replace PUMA codes with county names if true
  if (level %in% c('county', 'counties')) {
    
    # dataframe of all NC puma codes and county names
    # codes are different starting in 2012, so ensure we are pulling in the right year
    if (year > 2011) {
      
      nc_codes <- puma_area_code(letters, 'puma_counties.csv') %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    } else {
      
      nc_codes <- puma_area_code(letters, 'puma_counties.csv', puma12 = FALSE) %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    }
    
    # add county name to income dataframe
    household_incomes <- household_incomes %>%
      left_join(nc_codes, by = 'PUMA') %>%
      rename(group = cntyname)
    
  } else if (level %in% c('united states', 'us')) {
    
    # make integer to conserve ram
    household_incomes$group <- 0
    
  } else if (level %in% c('state', 'states')) {
    
    household_incomes$group <- household_incomes$ST
    
  } else if (level %in% c('puma', 'pums')) {
    
    household_incomes$group <- household_incomes$PUMA
    
  }
  
  return(household_incomes)
  
}

puma_area_code <- function(counties, names_file, puma12 = TRUE) {
  
  # This function takes as input a vector of county names
  # and return all PUMA area codes within a county
  
  # convert vector of county names into single RE string
  counties <- paste(counties, collapse = '|')
  
  # create columns to select based on whether user is selcting PUMA10 or PUMA2K
  cols <- if(puma12 == TRUE)  c('puma12', 'cntyname') else c('puma2k', 'cntyname')
  
  # read in table of PUMA names and filter for counties of interest
  puma_names <- read_csv(names_file) %>%
    # filter for needed counties
    filter(str_detect(cntyname, counties)) %>%
    # extract area code for either 2000 or 2010
    select(cols)
  
  # rename first column so that it has the same name, whether from 2K or 2010
  # this is also the same name as the PUMA area code column in the PUMA datasets
  names(puma_names)[1] <- 'PUMA'
  
  # sort by area code
  puma_names <- arrange(puma_names, PUMA)
  
  return(puma_names)
}

inflation_adjust <- function(df, wages_col, se_col, year_adjust, error = FALSE) {
  
  library(xts)
  library(lubridate)
  #########################################################################
  # This function takes as input a dataframe that contains dollar amounts
  # that must be adjusted for inflation and returns the same dataframe, 
  # but with an additional column for inflation ajusted dollar amounts
  #
  # Input:
  #   df: name of dataframe that contains dollar amounts
  #   wages_col: column name of column in dataframe containing dollar amounts
  #              entered as object name (no quotes), not string 
  #              (example: as wages and not "wages")
  #   se_col: column that contains standard error
  #   year_adjust: adjust all dollar amounts to this year
  #   errors: whether dataset contains standard errors that also need to be adjusted
  #
  # Output:
  #   The same dataframe, but with an additional column called 'estimate_adj'
  #
  #   !!!! Important Note: the column that contains years must be called 'year'
  #
  # Reference: US Census Bureau, A Compass for Understanding and Using ACS Data, 
  #             October 2008, A-22 
  #
  #########################################################################
  
  wages_col <- enquo(wages_col)
  
  if (error == TRUE) {
    se_col <- enquo(se_col)
  }
  
  # import CPI All items index data
  monthly_cpi <- read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
                            skip = 53, header = TRUE)
  
  # extract year and place in its own column
  monthly_cpi$year <- year(monthly_cpi$DATE)
  
  # calculate mean CPI for the year
  yearly_cpi <- monthly_cpi %>% 
    group_by(year) %>% 
    summarize(cpi = mean(VALUE))
  
  # calculate inflation rate compared to adjustment year
  yearly_cpi$adj_factor <- yearly_cpi$cpi[yearly_cpi$year == year_adjust]/yearly_cpi$cpi
  
  # combine inflation adjusted wages to wages dataset
  df <- left_join(df, yearly_cpi, by = 'year') %>%
    # adjust income in the given year for inflation since the base year
    # multiply the wage amount in the current year by the adjustment factor
    mutate(estimate_adj = round( !! wages_col * adj_factor, 0 ))
  # recalculate adjusted se, moe , and cv only if needed
  if (error == TRUE) {
    df <- df %>%
      mutate(se_adj = round( !! se_col / adj_factor), 0 ) %>%
      mutate(moe_adj = round( se_adj*1.96, 0),
             cv_adj = round( (se_adj / estimate_adj)*100, 2 ))
  }
  
  df <- df %>%
    select(-cpi, -adj_factor)
  
  return(df)
}