root <- "F:/Early_Childhood/04_Tarrant_County"
config <- yaml::read_yaml("./config.yaml")

#@title Demand using table B23008
#@param acs_year is the last two numbers of the desired year for the data (2015=15) 
#@param acs_type is the acs type of data collection (e.g. 1,5)
df_b23008 <- function(tbl=childcare_db(census_tbls = config$census$B23008,
                                       root = root)){
  
  pov_data <- tbl %>%
    dplyr::mutate(tracts = gsub("Census Tract ", "", Geography),
                  tracts = gsub(", Harris County, Texas", "", tracts))
  
  pov_data[,4:ncol(pov_data)] <- sapply(pov_data[,4:ncol(pov_data)], as.numeric)

  df <- pov_data %>%
    dplyr::select(-c(Id, Id2, Geography)) %>%
    dplyr::rename(n_kids_lt6 = Estimate..Total....Under.6.years.) %>%
    dplyr::mutate(n_kids_lt5 = (5/6)*(n_kids_lt6),
           n_kids_working_parents_lt6 = Estimate..Total....Under.6.years....Living.with.two.parents....Both.parents.in.labor.force + 
             Estimate..Total....Under.6.years....Living.with.two.parents....Father.only.in.labor.force + 
             Estimate..Total....Under.6.years....Living.with.two.parents....Mother.only.in.labor.force + 
             Estimate..Total....Under.6.years....Living.with.one.parent....Living.with.father....In.labor.force + 
             Estimate..Total....Under.6.years....Living.with.one.parent....Living.with.mother....In.labor.force,
           n_kids_working_parents_lt5 = (5/6)*(n_kids_working_parents_lt6)) %>%
    dplyr::select(tracts, n_kids_lt6, n_kids_lt5, n_kids_working_parents_lt6, n_kids_working_parents_lt5)
  
  assertthat::assert_that(length(unique(df$tracts)) == nrow(df))
  assertthat::assert_that(all(df$n_kids_lt5<=df$n_kids_lt6))
  assertthat::assert_that(all(df$n_kids_working_parents_lt5<= df$n_kids_working_parents_lt6))
  assertthat::assert_that(all(df$n_kids_working_parents_lt6<=df$n_kids_lt6))
  
  df
}

#@title Demand using table B17024
#@param acs_year. Last two numbers of the desired year for the data (2015=15) 
#@param acs_type. ACS type of data collection (e.g. 1,3,5)
df_b17024 <- function(tbl = childcare_db(census_tbls = config$census$B17024,
                                         root = root)){
  pov_data <- tbl %>%
    dplyr::mutate(tracts = gsub("Census Tract ", "", Geography),
                  tracts = gsub(", Harris County, Texas", "", tracts))
  
  vars <- names(pov_data)[grep("Estimate..Under.6.years.", names(pov_data))][1:9]
  vars_under_200 <- vars[2:9]
  
  df <- pov_data %>%
    dplyr::select(tracts, dplyr::one_of(vars)) %>% 
    dplyr::rename(n_kids_lt6 = Estimate..Under.6.years.) %>% 
    dplyr::mutate(n_kids_lt6_under200pct = rowSums(.[vars_under_200]),
                  n_kids_lt5 = (5/6)*n_kids_lt6,
                  n_kids_lt5_under200pct = (5/6)*n_kids_lt6_under200pct,
                  pct_kids_lt6_under200_pct = (n_kids_lt6_under200pct/n_kids_lt6)*100,
                  pct_kids_lt5_under200_pct = (n_kids_lt5_under200pct/n_kids_lt5)*100) %>% 
    dplyr::select(-dplyr::one_of(vars_under_200))
  
  assertthat::assert_that(max(df$pct_kids_lt5_under200_pct, na.rm = TRUE) <= 100)
  assertthat::assert_that(all(df$n_kids_lt5_under200pct <= df$n_kids_lt5))
  assertthat::assert_that(all(df$n_kids_lt6_under200pct <= df$n_kids_lt6))
  assertthat::assert_that(all(df$n_kids_lt5 <= df$n_kids_lt6))
  
  df
}

#@title Children in poverty with Working Parents
#@param b17024 B17024 table from census
#@param b23008 B23008 table from census
#@param pov_rate children w/ working parents are (1-pov_rate) more likely to be under 200% of the poverty line. current estimate 85%

df_demand <- function(b17024 = childcare_db(census_tbls = config$census$B17024, root = root), 
                      b23008 = childcare_db(census_tbls = config$census$B23008, root = root), 
                      pov_rate) {
  
  df_poverty_ratios <- df_b17024(b17024)
  df_working_parents <- df_b23008(b23008)
  
  df <- merge(df_poverty_ratios, df_working_parents)
  
  df2 <- df %>%
    dplyr::mutate(working_pov_rate = pov_rate * pct_kids_lt6_under200_pct) %>%
    dplyr::mutate(n_kids_lt6_working_under200_pct = (working_pov_rate/100) * n_kids_working_parents_lt6) %>%
    dplyr::mutate(n_kids_lt5_working_under200_pct = (working_pov_rate/100) * n_kids_working_parents_lt5)
  
  assertthat::assert_that(working_pov_rate <= 100)
  assertthat::assert_that(all(n_kids_lt5_working_under200_pct <= n_kids_lt6_working_under200_pct))
  
  df2
}
