#' @title Get HHSC CCL data
#' @description Returns the most recent HHSC CCL Daycare and Residential Operations 
#' Data. Link to data: https://data.texas.gov/resource/bc5r-88dy.csv
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.hhsc_ccl_data <- function(data_in_name,
                              data_in_pth,
                              url = "https://data.texas.gov/api/views/bc5r-88dy/rows.csv?accessType=DOWNLOAD",
                              download_date = Sys.Date()) {
  
  fl_name <- glue::glue(as.character(download_date), data_in_name, .sep = "_")
  
  dwnld_pth <- file.path(data_in_pth, fl_name)
  
  download.file(url, destfile = dwnld_pth, mode = "wb")
  
}

#' @title HHSC CCL data management
#' @description Clean CCL download data, convert key variables to binary and select
#' @param ccl_data_in_pth string. The path to read the ccl data in from.
#' @param ccl_data_in_name string. The name of the raw data to read in. Note ccl file name starts with download date in "yyyy-mm-dd" format.
#' @param ccl_data_out_pth string. The path to write the cleaned ccl data to.
#' @param ccl_data_out_name string. The name of the data to write out. 
#' @param county_name string. The name of the county to subset to. 

dm.ccl <- function(ccl_data_in_pth,
                   ccl_data_in_name,
                   ccl_data_out_pth,
                   ccl_data_out_name,
                   county_name) {
  
  df <- read.csv(file.path(ccl_data_in_pth, ccl_data_in_name), stringsAsFactors = F)
  
  df <- df %>% 
    dplyr::rename_all(tolower) %>% 
    tidyr::separate(location_address_geo, 
                    into = c("address", "lat", "long"),
                    sep = "([(,)])")
  
  assertthat::assert_that(all(c("operation_number", "licensed_to_serve_ages", "operation_type", 
                                "programs_provided", "operation_name", "accepts_child_care_subsidies") %in% names(df)),
                          msg = "CCL data frame missing needed variables")
  
  df <- df %>% 
    dplyr::mutate(operation_number = gsub("-.*", "", operation_number),
                  infant = ifelse(grepl("Infant", licensed_to_serve_ages), 1, 0),
                  toddler = ifelse(grepl("Toddler", licensed_to_serve_ages), 1, 0),
                  prek = ifelse(grepl("Pre-Kindergarten", licensed_to_serve_ages), 1, 0),
                  school = ifelse(grepl("School", licensed_to_serve_ages), 1, 0),
                  home_prvdr = ifelse(grepl("Home", operation_type), 1, 0),
                  center_prvdr = ifelse(grepl("Center", operation_type), 1, 0),
                  after_school = ifelse(grepl("After School Care", programs_provided), 1, 0),
                  head_start = ifelse(grepl("Head Start", operation_name), 1, 0),
                  subsidy = ifelse(accepts_child_care_subsidies == "Y", 1, 0),
                  download_date = as.Date(gsub("_.*", "", ccl_data_in_name))) %>% 
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
                  download_date) 
  
  if(!is.null(county_name)) {
    
    df <- df %>% 
      dplyr::filter(county == county_name)
    
    assertthat::assert_that(all(df$county == county_name))
  }
  
  write.csv(df, file.path(ccl_data_out_pth, ccl_data_out_name), row.names = FALSE)
  
  return(df)
}

#' @title Test ccl data management function
#' @description Tests that data management function works correctly
#' @param df data.frame. A data frame downloaded from get.hhsc_ccl_data

test.ccl_dm <- function(df){
  
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

}
