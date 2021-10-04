#' @title Frontline attributes
#' @param name string. Name of the data to use.
#' @param sheet string. Name of the sheet in the excel file to use.
#' @param na. string. Values to assign to NA when reading in data.
#' @param input_columns. The columns to expect and keep from the data.
#' @return object
attr.twc <- function(pth, 
                     name = "Weekly Closure Report 9.7.21.xlsx",
                     sheet = "All Provider Level",
                     na = "NA",
                     input_columns = list(`License Number` = "numeric",
                                          `TWC`= "character",
                                          `TRS Flag` = "character",
                                          `Number of Current Referrals` = "numeric",
                                          twc_date = "Date")) {

  list(pth = pth,
       name = name,
       sheet = sheet,
       na = na,
       input_columns = input_columns)
}

#' @title Data management for subsidy_provider column
#' @description Manage subsidy status
#' @param x
#' @return object
col.subsidy_provider <- function(x) {
  
  x$df <- x$df %>% 
    dplyr::mutate(subsidy_provider = tolower(twc) == "y") %>% 
    dplyr::select(-twc)

  return(x)
}

#' @title Data management for trs_provider column
#' @description Manage trs status: binary status, convert star level to num
#' @param x
#' @return object
col.trs_provider <- function(x) {

  x$df <- x$df %>%
    dplyr::mutate(trs_provider = ifelse(trs_flag == "Regular", F, T),
                  trs_star_level = as.numeric(gsub("\\D", "", trs_flag))) %>%
    dplyr::select(-trs_flag)

  return(x)
}

#' @title Confirm subsidy provider if provider is TRS status
#' @description Function replaces any provider who has a TRS status to also take
#' subsidy. This step is done because of data quality issue from TWC. In order 
#' to be a TRS provider, the provider must take the subsidy, so we know this is t
#' true.
col.confirm_sub <- function(x) {

  x$df <- x$df %>%
    dplyr::mutate(subsidy_provider = ifelse(trs_provider, TRUE, subsidy_provider))
  
  assertthat::assert_that(all(x$df %>%
                                dplyr::filter(trs_provider) %>%
                                dplyr::pull(subsidy_provider)))

  return(x)
}

#' @title Select and rename TWC columns
#' @param x
#' @return object
col.twc_select <- function(x) {

  x$df <- x$df %>%
    test_input(x$input_columns) %>%
    dplyr::select_all(~gsub(" ", "_", tolower(.))) %>%
    dplyr::rename(operation_number = license_number,
                  n_subsidy_kids = number_of_current_referrals)

  return(x)
}

#' @title Data management for TWC provider data
#' @description Manage TWC data, specifically TRS status and level, subsidy status, and
#' number of subsidy kids served
dm.twc <- function(x) {

  x <- x %>%
    col.twc_select() %>%
    col.operation_number() %>%
    col.trs_provider() %>%
    col.subsidy_provider() %>%
    col.confirm_sub()

  return(x$df)
}

#' @title Parse TWC date
#' @param format string. The format to use to parse the string. Default is
#' '%m.%d.%y'.
#' @return date
parse_date.twc <- function(x, date_format = "%m.%d.%y"){
  
  lubridate::parse_date_time(x, date_format)
}

#' @title Download TWC provider data
#' @description TWC provider data is emailed by Shay when we request
dwnld.twc <- function(x) {

  x$df <- readxl::read_xlsx(file.path(x$pth, x$name), 
                          sheet = x$sheet, 
                          na = x$na) %>% 
    dplyr::mutate(twc_date = parse_date.twc(x$name))

  return(x)
}

#' @title Process the TWC data
process.twc <- function(pth) {

  attr.twc(pth) %>%
    dwnld.twc() %>%
    dm.twc()
}
