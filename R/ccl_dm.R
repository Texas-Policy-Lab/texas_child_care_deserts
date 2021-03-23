#' @title Get HHSC CCL data
#' @description Returns the most recent HHSC CCL Daycare and Residential Operations 
#' Data. Link to data: https://data.texas.gov/resource/bc5r-88dy.csv
#' @param name string. The name to write the raw data to.
#' @param pth string. The path to write the raw to.
#' @export
dwnld.hhsc_ccl <- function(name,
                           pth,
                           url,
                           ...) {

  dwnld_pth <- file.path(pth, name)

  download.file(url, destfile = dwnld_pth, mode = "wb")

  df <- readr::read_csv(dwnld_pth)

  return(df)
}

#' @title Data management steps for the county column
#' @inherit dm.hhsc_ccl
#' @return data.frame
dm.county_col <- function(df, county) {

  msg <- "Expecting 5-digit county FIPS code."

  assertthat::assert_that(!is.na(as.numeric(county)),
                          msg = msg)

  df <- df %>%
    dplyr::mutate(county = tolower(gsub("[^[:alnum:]]", "", county))) %>% 
    dplyr::inner_join(tigris::fips_codes %>% 
                        dplyr::mutate(county = tolower(gsub("[^[:alnum:]]", "", county)),
                                      county = gsub("county", "", county),
                                      county_code = paste0(state_code, county_code)) %>% 
                        dplyr::select(county, county_code)) %>%
    dplyr::select(-county)

  if (!is.null(county)) {

    df <- df %>%
      dplyr::filter(county_code == as.character(county))

  } else {
    df <- df
  }

  assertthat::assert_that(all(df$county_code == county))
  assertthat::assert_that(sum(is.na(df$county_code)) == 0)

  return(df)
}

#' @title HHSC CCL data management
#' @description Clean CCL download data, convert key variables to binary and select
#' @param df data.frame. The dataframe
#' @param input_columns. List. List of the columns to keep.
#' @param county. Integer. The FIPS code for the county.
#' @param pth. string. Path to store the processed data.
#' @param name. string. Name of the data.
#' @return data.frame
dm.hhsc_ccl <- function(df,
                        input_columns,
                        county = NULL,
                        pth,
                        name,
                        ...) {

  df <- df %>%
    test_input(input_columns)

  df <- df %>%
    dplyr::rename_all(tolower) %>%
    tidyr::separate(location_address_geo,
                    into = c("address", "lat", "long"),
                    sep = "([(,)])")

  df <- df %>%
    dplyr::mutate(operation_number = gsub("-.*", "", operation_number),
                  infant = ifelse(grepl("infant", tolower(licensed_to_serve_ages)), 1, 0),
                  toddler = ifelse(grepl("toddler", tolower(licensed_to_serve_ages)), 1, 0),
                  prek = ifelse(grepl("pre-kindergarten", tolower(licensed_to_serve_ages)), 1, 0),
                  school = ifelse(grepl("school", tolower(licensed_to_serve_ages)), 1, 0),
                  home_prvdr = ifelse(grepl("home", tolower(operation_type)), 1, 0),
                  center_prvdr = ifelse(grepl("center", tolower(operation_type)), 1, 0),
                  after_school = ifelse(grepl("after school care", tolower(programs_provided)), 1, 0),
                  head_start = ifelse(grepl("head start", tolower(operation_name)), 1, 0),
                  subsidy = ifelse(accepts_child_care_subsidies == "Y", 1, 0),
                  download_date = Sys.Date()) %>% 
    dplyr::select(operation_number,
                  operation_name,
                  licensed_capacity = total_capacity,
                  county,
                  address,
                  lat,
                  long,
                  infant,
                  toddler,
                  prek,
                  school,
                  home_prvdr,
                  center_prvdr,
                  after_school,
                  head_start,
                  subsidy,
                  phone_number,
                  email_address,
                  download_date) %>% 
    dm.county_col(county = county)

  assertthat::assert_that(length(unique(df$operation_number)) == nrow(df),
                          msg = "Data frame is not unique on operation number")

  assertthat::assert_that(is.numeric(df$licensed_capacity),
                          msg = "Capacity not numeric")
  
  assertthat::assert_that(all(c(df$infant, df$toddler, df$prek, df$school) %in% c(1,0)),
                          msg = "Licensed to serve age not binary")
  
  assertthat::assert_that(all(c(df$home_prvdr, df$center_prvdr) %in% c(1,0)),
                          msg = "Operation type not binary")
  
  assertthat::assert_that(all(c(df$after_school, df$head_start, df$subsidy) %in% c(1,0)),
                          msg = "Operation characteristics not binary")
  
  readr::write_csv(df, file.path(pth, name))

  return(df)
}

#' @title Process the CCL data
process.hhsc_ccl <- function(hhsc_ccl,
                             raw_pth,
                             processed_pth) {

  hhsc_ccl$pth <- raw_pth
  hhsc_ccl$df <- do.call(dwnld.hhsc_ccl, hhsc_ccl)
  hhsc_ccl$pth <- processed_pth
  do.call(dm.hhsc_ccl, hhsc_ccl)

}
