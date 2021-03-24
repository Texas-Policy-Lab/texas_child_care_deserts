#' @title Get HHSC CCL data
#' @description Returns the most recent HHSC CCL Daycare and Residential 
#' Operations 
#' Data. Link to data: https://data.texas.gov/resource/bc5r-88dy.csv
#' @param name string. The name to write the raw data to.
#' @param raw_pth string. The path to write the raw to.
#' @param url string. The url to the data.
#' @export
dwnld.hhsc_ccl <- function(name,
                           raw_pth,
                           url = "https://data.texas.gov/api/views/bc5r-88dy/rows.csv?accessType=DOWNLOAD",
                           ext = "csv",
                           ...) {

  dwnld_pth <- file.path(raw_pth, paste(name, ext, sep = "."))

  download.file(url, destfile = dwnld_pth, mode = "wb")

  df <- readr::read_csv(dwnld_pth)

  return(df)
}

#' @title Data management steps for the operation number column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.operation_number <- function(df) {

  assertthat::assert_that(length(unique(df$operation_number)) == nrow(df),
                          msg = "Data frame is not unique on operation number")
  assertthat::assert_that(sum(is.na(df$operation_number)) == 0,
                          msg = "NAs in the operation_number")

  df <- df %>%
    dplyr::mutate(operation_number = gsub("-.*", "", operation_number))

  assertthat::assert_that(all(!grepl("-", df$operation_number)))
  assertthat::assert_that(length(unique(df$operation_number)) == nrow(df),
                          msg = "Data frame is not unique on operation number")

  return(df)
}

#' @title Data management steps for the county column
#' @inheritParams dm.hhsc_ccl
#' @return data.frame
col.county <- function(df, county_fips) {

  df <- df %>%
    dplyr::mutate(county = tolower(gsub("[^[:alnum:]]", "", county))) %>% 
    dplyr::inner_join(tigris::fips_codes %>% 
                        dplyr::mutate(county = tolower(gsub("[^[:alnum:]]", "", county)),
                                      county = gsub("county", "", county),
                                      county_code = paste0(state_code, county_code)) %>% 
                        dplyr::select(county, county_code)) %>% 
    dplyr::select(-county)

  if (!is.null(county_fips)) {

    assertthat::assert_that(!is.na(as.numeric(county_fips)),
                            msg = "Expecting 5-digit county FIPS code.")
    df <- df %>%
      dplyr::filter(county_code == as.character(county_fips)) %>% 
      dplyr::select(-county)

    if (nrow(df) == 0) {
      cat("Number of rows is 0, nothing to return. Is the county FIPS code correct?")
    }

  } else {
    df <- df
  }

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
col.location_address_geo <- function(df) {

  df <- df %>%
    tidyr::separate(location_address_geo,
                    into = c("address", "lat", "long"),
                    sep = "([(,)])")

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
                                              tolower(programs_provided)), TRUE, FALSE)) %>%
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
                        processed_pth,
                        name,
                        ...) {

  df <- df %>%
    test_input(input_columns) %>%
    dplyr::rename_all(tolower) %>%
    col.operation_number() %>%
    col.county(county_fips = county_fips) %>%
    col.location_address_geo() %>%
    col.licensed_to_serve_ages() %>%
    col.operation_type() %>%
    col.operation_name() %>%
    col.programs_provided() %>%
    col.accepts_child_care_subsidies() %>%
    col.total_capacity() %>%
    dplyr::mutate(download_date = Sys.Date())

  readr::write_csv(df, file.path(processed_pth, paste(name, "csv", sep = ".")))

  return(df)
}

#' @title Process the CCL data
process.hhsc_ccl <- function(hhsc_ccl) {

  hhsc_ccl$df <- do.call(dwnld.hhsc_ccl, hhsc_ccl)
  df <- do.call(dm.hhsc_ccl, hhsc_ccl)  
}
