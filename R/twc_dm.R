#' @title Parse TWC date
#' @param format string. The format to use to parse the string. Default is
#' '%m/%Y'.
#' @return date
parse_date.twc <- function(x, format = "%m/%Y"){
  
  date <- lubridate::parse_date_time(x, format)
}

#' @title Data management for subsidy_provider column
#' @description Manage subsidy status
#' @param df
#' @return data.frame
col.subsidy_provider <- function(df){
  
  df <- df %>% 
    dplyr::mutate(subsidy_provider = tolower(twc) == "y") %>% 
    dplyr::select(-twc)
}

#' @title Data management for trs_provider column
#' @description Manage trs status: binary status, convert star level to num
#' @param df
#' @return data.frame
col.trs_provider <- function(df){
  
  df <- df %>% 
    dplyr::mutate(trs_provider = ifelse(trs_flag == "Regular", F, T),
                  trs_star_level = as.numeric(gsub("\\D", "", trs_flag))) %>% 
    dplyr::select(-trs_flag)
}

#' @title Data management for open status
#' @description Manage status: filter to only open providers
#' @param df
#' @return data.frame
col.open_status <- function(df){
  
  df <- df %>% 
    dplyr::filter(status == "Open") %>% 
    dplyr::select(-status)
}

#' @title Data management steps to clean the capacity column
#' @param df
#' @return data.frame
col.total_capacity_twc <- function(df) {
  
  df <- df %>%
    dplyr::rename(licensed_capacity = reported_capacity)
  
  assertthat::assert_that(is.numeric(df$licensed_capacity),
                          msg = "Capacity not numeric")
  return(df)
}

#' @title Data management steps to clean the provider_type column
#' @param df
#' @return data.frame
col.provider_type <- function(df) {
  
  df <- df %>% 
    dplyr::mutate(home_prvdr = ifelse(grepl("home", 
                                            tolower(provider_type)), TRUE, FALSE),
                  center_prvdr = ifelse(grepl("center", 
                                              tolower(provider_type)), TRUE, FALSE),
                  licensedhome_prvdr = ifelse(home_prvdr & grepl("licensed", tolower(provider_type)), TRUE, FALSE),
                  registeredhome_prvdr = ifelse(home_prvdr & grepl("registered", tolower(provider_type)), TRUE, FALSE)
    ) %>% 
    dplyr::select(-provider_type)
  
  assertthat::assert_that(all(c(df$home_prvdr, df$center_prvdr, df$licensedhome_prvdr, df$registeredhome_prvdr) %in% c(TRUE, FALSE)),
                          msg = "Operation type not binary")
  
  return(df)
}


#' @title Data management steps for the county column
#' @param df
#' @return data.frame
col.county_twc <- function(df, state_fips) {

  df <- df %>%
    dplyr::mutate(county = tolower(gsub("[^[:alnum:]]", "", county))) %>% 
    dplyr::inner_join(tigris::fips_codes %>% 
                        dplyr::filter(state_code == state_fips) %>%
                        dplyr::mutate(county = tolower(gsub("[^[:alnum:]]", "", county)),
                                      county = gsub("county", "", county),
                                      county_code = paste0(state_code, county_code)) %>% 
                        dplyr::select(county, county_code)) %>% 
    dplyr::select(-county)
  
  return(df)
}

#' @title Data management steps to clean the operation name column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.operation_name_twc <- function(df) {

  df <- df %>% 
    dplyr::rename(operation_name = provider_name) %>% 
    dplyr::mutate(head_start = ifelse(grepl("head start",
                                            tolower(operation_name)), TRUE, FALSE))
  
  assertthat::assert_that(all(c(df$head_start) %in% c(TRUE, FALSE)),
                          msg = "Operation characteristics not binary")
  return(df)
}

#' @title Download TWC provider data
#' @description TWC provider data is emailed by Shay when we request
dwnld.twc <- function(raw_pth, 
                      name = "Provider Closure Report - April 2022.xlsx",
                      sheet = "All Provider Level") {
  
  df <- readxl::read_xlsx(file.path(raw_pth, name), sheet = sheet, na = "NA") %>% 
    dplyr::mutate(twc_date = parse_date.twc(name))
}

#' @title Data management for TWC provider data
#' @description Manage TWC data, specifically TRS status and level, subsidy status, and
#' number of subsidy kids served
dm.twc <- function(df,
                   input_columns = list(`License Number` = "numeric",
                                        `TWC`= "character",
                                        `TRS Flag` = "character",
                                        `Number of Current Referrals` = "numeric",
                                        `Status`  = "character",
                                        `Reported Capacity` = "numeric",
                                        `Provider Type` = "character",
                                        `County` = "character",
                                        `Provider Name` = "character",
                                        `Board Name` = 'character',
                                        twc_date = "Date"),
                   state_fips){

  df <- df %>% 
    test_input(input_columns) %>% 
    dplyr::select_all(~gsub(" ", "_", tolower(.))) %>%    
    dplyr::rename(operation_number = license_number,
                  n_subsidy_kids = number_of_current_referrals) %>% 
    col.operation_number() %>% 
    col.operation_name_twc() %>% 
    col.trs_provider() %>% 
    col.subsidy_provider() %>% 
    col.open_status() %>% 
    col.total_capacity_twc() %>% 
    col.provider_type() %>% 
    col.county_twc(state_fips = state_fips)
  
}

#' @title Process the TWC data
process.twc <- function(raw_pth,
                        state_fips) {

  df <- dwnld.twc(raw_pth) %>% 
    dm.twc(state_fips = state_fips)
}
