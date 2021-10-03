#' @title Frontline attributes
#' @param input_columns list. The columns to expect and keep from the data.
#' @param pth. string. Path to store the processed data.
#' @param name. string. Name of the data.
#' @param state_code string. The state fips code
#' @param df_twc data.frame. TWC data
#' @param naeyc_pth1 string.
#' @param naeyc_pth2 string.
#' @return object
attr.ccl <- function(pth,
                     state_code,
                     df_twc,
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
                     naeyc_pth1 = "data/raw/BP4K - NAEYC List - 5.26.21.xlsx",
                     naeyc_pth2 = "data/raw/NAEYC Providers - NAEYC - 5.26.21.xlsx",
                     name = "HHSC_CCL") {

  list(pth = pth,
       input_columns = input_columns,
       naeyc_pth1 = naeyc_pth1,
       naeyc_pth2 = naeyc_pth2,
       name = name,
       state_fips = state_code,
       df_twc = df_twc)
}

#' @title Data management steps for the operation number column
#' @param x
#' @return data.frame
col.operation_number <- function(x) {

  assertthat::assert_that(length(unique(x$df$operation_number)) == nrow(x$df),
                          msg = "Data frame is not unique on operation number")
  assertthat::assert_that(sum(is.na(x$df$operation_number)) == 0,
                          msg = "NAs in the operation_number")

  x$df <- x$df %>%
    dplyr::mutate(operation_number = gsub("-.*", "", operation_number),
                  operation_number = stringr::str_pad(operation_number,
                                                      side = "left", 
                                                      width = 15,
                                                      pad = "0"))
  
  assertthat::assert_that(all(!grepl("-", x$df$operation_number)))
  assertthat::assert_that(length(unique(x$df$operation_number)) == nrow(x$df),
                          msg = "Data frame is not unique on operation number")
  
  return(x)
}

#' @title Data management steps for the county column
#' @param x object.
#' @return objet
col.county <- function(x) {

  x$df <- x$df %>%
    dplyr::mutate(county = tolower(gsub("[^[:alnum:]]", "", county))) %>%
    dplyr::inner_join(tigris::fips_codes %>%
                        dplyr::filter(state_code == x$state_fips) %>%
                        dplyr::mutate(county = tolower(gsub("[^[:alnum:]]", "", county)),
                                      county = gsub("county", "", county),
                                      county_code = paste0(state_code, county_code)) %>%
                        dplyr::select(county, county_code)) %>%
    dplyr::select(-county)

  return(x)
}

#' @title Data management steps to clean the licensed to serve ages column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.licensed_to_serve_ages <- function(x) {

  x$df <- x$df %>%
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

  assertthat::assert_that(all(c(x$df$infant, x$df$toddler, x$df$prek, x$df$school) 
                              %in% c(TRUE, FALSE)),
                          msg = "Licensed to serve age not binary")
  return(x)
}

#' @title Data management steps to clean the operation type column
#' @param x object.
#' @return object
col.location_address_geo <- function(x) {

  bb <- tx_bounding_box(state_fips = x$state_fips)

  x$df <- x$df %>%
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
                       dplyr::select(operation_number, lat, long, tract) %>%
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

  return(x$df)
}

#' @title Data management steps to clean the operation_type column
#' @param x object
#' @return object
col.operation_type <- function(x) {
  
  x$df <- x$df %>% 
    dplyr::mutate(home_prvdr = ifelse(grepl("home", 
                                            tolower(operation_type)), TRUE, FALSE),
                  center_prvdr = ifelse(grepl("center", 
                                              tolower(operation_type)), TRUE, FALSE),
                  licensedhome_prvdr = ifelse(home_prvdr & grepl("licensed", tolower(operation_type)), TRUE, FALSE),
                  registeredhome_prvdr = ifelse(home_prvdr & grepl("registered", tolower(operation_type)), TRUE, FALSE)
    ) %>% 
    dplyr::select(-operation_type)
  
  assertthat::assert_that(all(c(x$df$home_prvdr, x$df$center_prvdr, 
                                x$df$licensedhome_prvdr, x$df$registeredhome_prvdr) %in% c(TRUE, FALSE)),
                          msg = "Operation type not binary")
  
  return(x)
}

#' @title Data management steps to clean the operation name column
#' @param x object.
#' @return object
col.operation_name <- function(x) {

  x$df <- x$df %>% 
    dplyr::mutate(head_start = ifelse(grepl("head start",
                                            tolower(operation_name)), TRUE, FALSE))

  assertthat::assert_that(all(c(x$df$head_start) %in% c(TRUE, FALSE)),
                          msg = "Operation characteristics not binary")
  return(x)
}

#' @title Data management steps to clean the programs provided column
#' @param x object.
#' @return object
col.programs_provided <- function(x) {

  x$df <- x$df %>%
    dplyr::mutate(after_school = ifelse(grepl("after school care", 
                                              tolower(programs_provided)), TRUE, FALSE),
                  after_school_only = grepl("afterschool|after school|after-school|school age", tolower(operation_name)),
                  school_age_only = ifelse(!infant & !toddler & !prek & school, TRUE, FALSE),
                  after_school_school_age_only = ifelse(after_school_only | school_age_only, TRUE, FALSE)) %>%
    dplyr::select(-programs_provided)
  
  assertthat::assert_that(all(c(x$df$after_school) %in% c(TRUE, FALSE)),
                          msg = "Operation characteristics not binary") 
  return(x)
}

#' @title Data management steps to clean the accepts child care subsidies column
#' @param x object.
#' @return object
col.accepts_child_care_subsidies <- function(x) {
  
  if (all(unique(x$df$accepts_child_care_subsidies) %in% c("Y", "N")) == F) {
    cat("Additional values besides 'Y' and 'N' in 'Accepts child care subsidies' column")
  }
  
  x$df <- x$df %>%
    dplyr::mutate(subsidy = dplyr::case_when(accepts_child_care_subsidies == "Y" ~ TRUE,
                                             accepts_child_care_subsidies == "N" ~ FALSE,
                                             TRUE ~ NA)) %>% 
    dplyr::select(-accepts_child_care_subsidies)
  
  assertthat::assert_that(all(c(x$df$subsidy) %in% c(TRUE, FALSE, NA)),
                          msg = "Operation characteristics not binary")
  return(x)
}

#' @title Data management steps to clean the total capacity column
#' @param x object.
#' @return object
#' @title Assign deserts
#' @description Assign deserts based on provider types. Note Head Start is 
#' included as a subsidy provider, a TRS and TRS 4 star provider.
col.assign_deserts <- function(x) {

  naeyc <- readxl::read_excel(x$naeyc_pth1) %>%
    dplyr::filter(`32566_NAEYC` == "Yes") %>%
    dplyr::select(operation_number = `OP Number`) %>% 
    dplyr::bind_rows(readxl::read_excel(x$naeyc_pth2) %>%
                       dplyr::select(operation_number = `Program ID`) %>% 
                       dplyr::mutate(operation_number = as.character(operation_number))) %>% 
    dplyr::distinct() %>% 
    col.operation_number() %>% 
    dplyr::mutate(naeyc = TRUE)

  x$df <- x$df %>%
    dplyr::left_join(x$df_twc) %>%
    dplyr::left_join(naeyc) %>%
    dplyr::mutate(naeyc = ifelse(is.na(naeyc), FALSE, naeyc),
                  all_provider = ifelse(!after_school_school_age_only, TRUE, FALSE),
                  sub_provider = ifelse(all_provider & (subsidy_provider | head_start | naeyc), TRUE, FALSE),
                  sub_provider = ifelse(trs_provider, TRUE, sub_provider),
                  sub_provider = ifelse(is.na(sub_provider), subsidy, sub_provider),
                  sub_trs_provider = ifelse((sub_provider & trs_provider) | head_start | naeyc, TRUE, FALSE),
                  sub_trs4_provider = ifelse((sub_trs_provider & trs_star_level == 4) | head_start | naeyc, TRUE, FALSE))

  qual_type <- df %>%
    dplyr::select(operation_number, naeyc, trs_provider, head_start) %>% 
    tidyr::pivot_longer(names_to = "quality_type", values_to = "quality",
                        -operation_number) %>% 
    tidyr::drop_na() %>%
    dplyr::filter(quality) %>%
    dplyr::select(-quality) %>%
    dplyr::mutate(quality_desc = dplyr::case_when(quality_type == "head_start" ~ "Head Start",
                                                  quality_type == "trs_provider" ~ "TRS",
                                                  quality_type == "naeyc" ~ "NAEYC")) %>%
    dplyr::group_by(operation_number) %>%
    dplyr::summarise(quality_desc = paste(quality_desc, collapse = ", "))

  x$df <- x$df %>%
    dplyr::left_join(qual_type) %>%
    dplyr::mutate(quality = ifelse(is.na(quality_desc), FALSE, TRUE))
  
  return(x)
}

#' @title HHSC CCL data management
#' @description Clean CCL download data, convert key variables to binary and 
#' select variables
#' @return data.frame
dm.hhsc_ccl <- function(x) {
  
  df <- df %>%
    test_input(input_columns) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename(licensed_capacity = total_capacity) %>%
    col.operation_number() %>%
    col.county() %>%
    col.location_address_geo() %>%
    col.licensed_to_serve_ages() %>%
    col.operation_type() %>%
    col.operation_name() %>%
    col.programs_provided() %>%
    col.accepts_child_care_subsidies() %>%
    col.assign_deserts() %>%
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
process.hhsc_ccl <- function(pth, state_code) {
  
  attr.ccl(pth, state_code) %>%
    dwnld.hhsc_ccl() %>%
    dm.hhsc_ccl()
}
