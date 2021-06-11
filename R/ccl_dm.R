#' @title Data management steps for the operation number column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.operation_number <- function(df) {

  assertthat::assert_that(length(unique(df$operation_number)) == nrow(df),
                          msg = "Data frame is not unique on operation number")
  assertthat::assert_that(sum(is.na(df$operation_number)) == 0,
                          msg = "NAs in the operation_number")

  df <- df %>%
    dplyr::mutate(operation_number = gsub("-.*", "", operation_number),
                  operation_number = stringr::str_pad(operation_number,
                                                      side = "left", 
                                                      width = 15,
                                                      pad = "0"))

  assertthat::assert_that(all(!grepl("-", df$operation_number)))
  assertthat::assert_that(length(unique(df$operation_number)) == nrow(df),
                          msg = "Data frame is not unique on operation number")

  return(df)
}

#' @title Data management steps for the county column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.county <- function(df, state_fips) {

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

#' @title Data management steps to clean the licensed to serve ages column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.licensed_to_serve_ages <- function(df) {

  df <- df %>%
    dplyr::mutate(infant = ifelse(grepl("infant",
                                        tolower(licensed_to_serve_ages)),
                                  TRUE, FALSE),
                  toddler = ifelse(grepl("toddler",
                                         tolower(licensed_to_serve_ages)),
                                   TRUE, FALSE),
                  prek = ifelse(grepl("pre-kindergarten",
                                      tolower(licensed_to_serve_ages)),
                                TRUE, FALSE),
                  school = ifelse(grepl("school",
                                        tolower(licensed_to_serve_ages)),
                                  TRUE, FALSE),
    ) %>%
    dplyr::select(-licensed_to_serve_ages)

  assertthat::assert_that(all(c(df$infant, df$toddler, df$prek, df$school) 
                              %in% c(TRUE, FALSE)),
                          msg = "Licensed to serve age not binary")
  return(df)
}

#' @title Data management steps to clean the operation type column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.location_address_geo <- function(df, state_fips) {
  
  bb <- tx_bounding_box(state_fips = state_fips)
  
  df <- df %>%
    tidyr::separate(location_address_geo,
                    into = c("address", "lat", "long"),
                    sep = "([(,)])") %>% 
    dplyr::mutate(address = gsub("\n", "", address),
                  lat = stringr::str_trim(lat, "both"),
                  long = stringr::str_trim(long, "both"),
                  tract = NA) %>%
    dplyr::ungroup() %>% 
    check_tx_bounds(bb = bb) %>%
    dplyr::left_join(DF_HHSC_CCL %>%
                       dplyr::select(operation_number, lat, long, tract
                                     ) %>%
                       dplyr::rename(lat2 = lat, long2 = long, tract2 = tract
                                   )) %>%
    dplyr::mutate(lat = ifelse(is.na(lat), lat2, lat),
                  long = ifelse(is.na(long), long2, long),
                  tract = ifelse(is.na(tract), tract2, tract),
                  address = stringr::str_to_title(address)) %>%
    dplyr::select(-c(lat2, long2, tract2)) %>%
    dm.geocode_address(bb = bb) %>%
    dm.reverse_geocode() %>%
    check_tx_bounds(bb = bb)

  return(df)
}

#' @title Data management steps to clean the location address geo column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.operation_type <- function(df) {

  df <- df %>% 
    dplyr::mutate(home_prvdr = ifelse(grepl("home", 
                                            tolower(operation_type)), TRUE, FALSE),
                  center_prvdr = ifelse(grepl("center", 
                                              tolower(operation_type)), TRUE, FALSE)
    ) %>% 
    dplyr::select(-operation_type)

  assertthat::assert_that(all(c(df$home_prvdr, df$center_prvdr) %in% c(TRUE, FALSE)),
                          msg = "Operation type not binary")

  return(df)
}

#' @title Data management steps to clean the operation name column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.operation_name <- function(df) {

  df <- df %>% 
    dplyr::mutate(head_start = ifelse(grepl("head start",
                                            tolower(operation_name)), TRUE, FALSE))
  
  assertthat::assert_that(all(c(df$head_start) %in% c(TRUE, FALSE)),
                          msg = "Operation characteristics not binary")
  return(df)
}

#' @title Data management steps to clean the programs provided column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.programs_provided <- function(df) {

  df <- df %>%
    dplyr::mutate(after_school = ifelse(grepl("after school care", 
                                              tolower(programs_provided)), TRUE, FALSE),
                  after_school_only = grepl("afterschool|after school|after-school|school age", tolower(operation_name)),
                  school_age_only = ifelse(!infant & !toddler & !prek & school, TRUE, FALSE),
                  after_school_school_age_only = ifelse(after_school_only | school_age_only, TRUE, FALSE)) %>%
    dplyr::select(-programs_provided)

  assertthat::assert_that(all(c(df$after_school) %in% c(TRUE, FALSE)),
                          msg = "Operation characteristics not binary") 
  return(df)
}

#' @title Data management steps to clean the accepts child care subsidies column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.accepts_child_care_subsidies <- function(df) {

  if (all(unique(df$accepts_child_care_subsidies) %in% c("Y", "N")) == F) {
    cat("Additional values besides 'Y' and 'N' in 'Accepts child care subsidies' column")
  }

  df <- df %>%
    dplyr::mutate(subsidy = dplyr::case_when(accepts_child_care_subsidies == "Y" ~ TRUE,
                                             accepts_child_care_subsidies == "N" ~ FALSE,
                                             TRUE ~ NA)) %>% 
    dplyr::select(-accepts_child_care_subsidies)

  assertthat::assert_that(all(c(df$subsidy) %in% c(TRUE, FALSE, NA)),
                          msg = "Operation characteristics not binary")
  return(df)
}

#' @title Data management steps to clean the total capacity column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.total_capacity <- function(df) {

  df <- df %>%
    dplyr::rename(licensed_capacity = total_capacity)

  assertthat::assert_that(is.numeric(df$licensed_capacity),
                          msg = "Capacity not numeric")
  return(df)
}

#' @title Assign deserts
col.assign_deserts <- function(df, trs_pth) {

  trs <- readr::read_csv(trs_pth) %>% 
    dplyr::select(operation_number, trs_provider, subsidy_provider, trs_star_level) %>% 
    dplyr::mutate(operation_number = stringr::str_pad(operation_number, 
                                                      side = "left",
                                                      width = 15,
                                                      pad = "0"))

  df <- df %>%
    dplyr::left_join(trs) %>%
    dplyr::mutate(all_provider = ifelse(!after_school_school_age_only, TRUE, FALSE),
                  sub_provider = ifelse(all_provider & subsidy_provider, TRUE, FALSE),
                  sub_provider = ifelse(trs_provider, TRUE, sub_provider),
                  sub_trs_provider = ifelse(sub_provider & trs_provider, TRUE, FALSE),
                  sub_trs4_provider = ifelse(sub_trs_provider & trs_star_level == 4, TRUE, FALSE))

}

#' @title HHSC CCL data management
#' @description Clean CCL download data, convert key variables to binary and 
#' select variables
#' @param df data.frame. The dataframe
#' @param input_columns. List. List of the columns to keep.
#' @param county_fips. Integer. The FIPS code for the county.
#' @param pth. string. Path to store the processed data.
#' @param name. string. Name of the data.
#' @return data.frame
dm.hhsc_ccl <- function(df,
                        input_columns = list(OPERATION_NUMBER = "character",
                                             OPERATION_NAME = "character",
                                             OPERATION_TYPE = "character",
                                             Location_address_geo = "character",
                                             COUNTY= "character",
                                             TOTAL_CAPACITY = "numeric",
                                             LICENSED_TO_SERVE_AGES= "character",
                                             PROGRAMS_PROVIDED= "character",
                                             ACCEPTS_CHILD_CARE_SUBSIDIES= "character",
                                             email_address = "character",
                                             PHONE_NUMBER = "character"),
                        county_fips = NULL,
                        name,
                        state_fips,
                        trs_pth,
                        ...) {

  df <- df %>%
    test_input(input_columns) %>%
    dplyr::rename_all(tolower) %>%
    col.operation_number() %>%
    col.county(state_fips = state_fips) %>%
    col.location_address_geo(state_fips = state_fips) %>%
    col.licensed_to_serve_ages() %>%
    col.operation_type() %>%
    col.operation_name() %>%
    col.programs_provided() %>%
    col.accepts_child_care_subsidies() %>%
    col.total_capacity() %>% 
    col.assign_deserts(trs_pth) %>%
    dplyr::mutate(download_date = Sys.Date())

  return(df)
}

#' @title HHSC CCL Population
#' @description Creates a dataframe unique on operation number and download date
#' to be able to track when child care providers enter and leave the CCL database
#' @return data.frame
pop.hhsc_ccl <- function(new, old) {

  new %>%
    dplyr::bind_rows(old) %>%
    dplyr::distinct(operation_number, download_date)
}

#' @title HHSC CCL Population
#' @description Keeps to most recent attributes for each provider by download
#' date
#' @return data.frame
pop.hhsc_ccl_most_recent_attr <- function(new, old) {

  new %>%
    dplyr::bind_rows(old) %>%
    dplyr::group_by(operation_number) %>%
    dplyr::slice(which.max(download_date))
}

#' @title Process the CCL data
process.hhsc_ccl <- function(cls) {

  cls$df <- do.call(dwnld.hhsc_ccl, cls)
  df <- do.call(dm.hhsc_ccl, cls)

}
