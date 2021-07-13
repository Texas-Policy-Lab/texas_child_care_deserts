#' @title Parse TWC date
#' @param format string. The format to use to parse the string. Default is
#' '%m.%d.%y'.
#' @return date
parse_date.twc <- function(x, format = "%m.%d.%y"){
  
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

#' @title Download TWC provider data
#' @description TWC provider data is emailed by Shay when we request
dwnld.twc <- function(raw_pth, 
                      name = "Closure Report 05.17.21.xlsx",
                      sheet = "All Provider Level") {
  
  df <- readxl::read_xlsx(file.path(raw_pth, name), sheet = sheet, na = "NA") %>% 
    dplyr::mutate(date = parse_date.twc(name))
}

#' @title Data management for TWC provider data
#' @description Manage TWC data, specifically TRS status and level, subsidy status, and
#' number of subsidy kids served
dm.twc <- function(df,
                   input_columns = list(`License Number` = "numeric",
                                        `TWC`= "character",
                                        `TRS Flag` = "character",
                                        `Number of Current Referrals` = "numeric",
                                        date = "Date")){
  
  df <- df %>% 
    test_input(input_columns) %>% 
    dplyr::select_all(~gsub(" ", "_", tolower(.))) %>%
    col.trs_provider() %>% 
    col.subsidy_provider() %>% 
    dplyr::mutate(operation_number = as.character(license_number)) %>% 
    dplyr::select(-license_number) %>% 
    dplyr::rename(n_subsidy_kids = number_of_current_referrals)
  
}

#' @title Process the TWC data
process.twc <- function(raw_pth) {
  
  df <- dwnld.twc(raw_pth) %>% 
    dm.twc()
}