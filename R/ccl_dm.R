#' @title Frontline attributes
#' @param input_columns list. The columns to expect and keep from the data.
#' @param name string. The name to write the raw data to.
#' @param pth string. The path to write the raw to.
#' @param url string. The url to the data.
#' @param state_code string. The state fips code
#' @param df_twc data.frame. TWC data
#' @param naeyc_pth1 string.
#' @param naeyc_pth2 string.
#' @param bb_url string. Bounding box url.
#' @return object
attr.ccl <- function(pth,
                     state_code,
                     df_twc,
                     url = "https://data.texas.gov/api/views/bc5r-88dy/rows.csv?accessType=DOWNLOAD",
                     ext = "csv",
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
                     naeyc_pth1 = "BP4K - NAEYC List - 5.26.21.xlsx",
                     naeyc_pth2 = "NAEYC Providers - NAEYC - 5.26.21.xlsx",
                     name = "HHSC_CCL",
                     bb_url = "https://gist.githubusercontent.com/a8dx/2340f9527af64f8ef8439366de981168/raw/81d876daea10eab5c2675811c39bcd18a79a9212/US_State_Bounding_Boxes.csv") {

  list(pth = pth,
       input_columns = input_columns,
       naeyc_pth1 = naeyc_pth1,
       naeyc_pth2 = naeyc_pth2,
       name = name,
       state_fips = state_code,
       df_twc = df_twc,
       url = url,
       ext = ext,
       bb_url = bb_url)
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

  x$df <- x$df %>%
    tidyr::separate(location_address_geo,
                    into = c("address", "lat", "long"),
                    sep = "([(,)])") %>% 
    dplyr::mutate(address = gsub("\n", "", address),
                  lat = stringr::str_trim(lat, "both"),
                  long = stringr::str_trim(long, "both"),
                  tract = NA) %>%
    dplyr::ungroup()

  x <- x %>%
    check_tx_bounds()

  # x$df <- x$df %>%
  #   dplyr::left_join(DF_HHSC_CCL %>%
  #                      dplyr::select(operation_number, lat, long, tract) %>%
  #                      dplyr::rename(lat2 = lat, long2 = long, tract2 = tract
  #                      )) %>%
  #   dplyr::mutate(lat = ifelse(is.na(lat), lat2, lat),
  #                 long = ifelse(is.na(long), long2, long),
  #                 tract = ifelse(is.na(tract), tract2, tract),
  #                 address = stringr::str_to_title(address)) %>%
  #   dplyr::select(-c(lat2, long2, tract2)) %>%
  #   # dm.geocode_address() %>%
  #   # dm.reverse_geocode() %>%
  #   
  # x <- x %>%
  #   check_tx_bounds()

  return(x)
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

#' @title Data management steps to add the naeyc column
#' @description Assigns NAEYC status to providers from different sources 
#' provided by Tarrant County. Note: we are not sure how often these will be 
#' updated and the code may have to change since there likely is not a 
#' consistent file format for these files. 
#' @param x object.
#' @return object
col.dm_naeyc <- function(x) {

  naeyc1 <- readxl::read_excel(file.path(x$pth, x$naeyc_pth1)) %>%
    dplyr::filter(`32566_NAEYC` == "Yes") %>%
    dplyr::select(operation_number = `OP Number`)

  naeyc2 <- readxl::read_excel(file.path(x$pth, x$naeyc_pth2)) %>%
    dplyr::select(operation_number = `Program ID`) %>% 
    dplyr::mutate(operation_number = as.character(operation_number))

  x2 <- list(df = naeyc1 %>% 
               dplyr::bind_rows(naeyc2) %>% 
               dplyr::distinct() %>% 
               dplyr::mutate(naeyc = TRUE)) %>%
    col.operation_number()

  x$df <- x$df %>%
    dplyr::left_join(x2$df) %>%
    dplyr::mutate(naeyc = ifelse(is.na(naeyc), FALSE, naeyc))

  return(x)
}

#' @title Assign deserts
#' @description Assign deserts based on provider types.
#' @param x object.
#' @return object
col.assign_deserts <- function(x) {

  x$df <- x$df %>%
    dplyr::left_join(x$df_twc) %>%
    dplyr::mutate(all_provider = !after_school_school_age_only,
                  sub_provider = all_provider & (subsidy_provider | head_start | naeyc),
                  sub_provider = ifelse(trs_provider, TRUE, sub_provider),
                  sub_provider = ifelse(is.na(sub_provider), subsidy, sub_provider),
                  sub_trs_provider = (sub_provider & trs_provider) | head_start | naeyc,
                  sub_trs4_provider = (sub_trs_provider & trs_star_level == 4) | head_start | naeyc
                  )
  return(x)
}

#' @title Data management quality of the provider
#' @description Assigns a quality for the provider. Note Head Start is 
#' included as a subsidy provider, a TRS and TRS 4 star provider.
col.quality <- function(x) {
browser()
  qual_type <- x$df %>%
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

#' @title Select and rename CCLHHSC columns
#' @param x
#' @return object
col.ccl_select <- function(x) {

  x$df <- x$df %>%
    test_input(x$input_columns) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename(licensed_capacity = total_capacity) %>%
    dplyr::mutate(download_date = Sys.Date())
  return(x)
}

#' @title HHSC CCL data management
#' @description Clean CCL download data, convert key variables to binary and 
#' select variables
#' @return data.frame
dm.hhsc_ccl <- function(x) {

  x <- tx_bounding_box(x)

  x <- x %>%
    col.ccl_select() %>%
    col.operation_number() %>%
    col.county() %>%
    col.location_address_geo() %>%
    col.licensed_to_serve_ages() %>%
    col.operation_type() %>%
    col.operation_name() %>%
    col.programs_provided() %>%
    col.accepts_child_care_subsidies() %>%
    col.dm_naeyc() %>%
    col.assign_deserts() %>%
    col.quality()

  return(x$df)
}

#' @title Get HHSC CCL data
#' @description Returns the most recent HHSC CCL Daycare and Residential 
#' Operations 
#' Data. Link to data: https://data.texas.gov/resource/bc5r-88dy.csv
#' @export
dwnld.hhsc_ccl <- function(x) {

  dwnld_pth <- file.path(x$pth, paste(x$name, x$ext, sep = "."))
  download.file(x$url, destfile = dwnld_pth, mode = "wb")
  x$df <- readr::read_csv(dwnld_pth)

  return(x)
}

#' @title Process the CCL data
process.hhsc_ccl <- function(pth, state_code, df_twc) {

  attr.ccl(pth, state_code, df_twc) %>%
    dwnld.hhsc_ccl() %>%
    dm.hhsc_ccl()
}
