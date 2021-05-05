#' @title Download ACS
#' @description Passes in a list of parameters to download the ACS census
#' data using functions from the tidycensus package
#' @param tbls list. List of census tables with attributes to download
#' @param raw_pth. Path to save the raw data.
#' @details To find a list of parameters to pass see documentation for 
#' tidycensus::get_acs
#' @examples 
#' \dontrun{
#' tbls <- list(B23008 = list(year = 2019, state = 48, 
#'                                geography = "tract", county = 439))
#' raw_pth <- "C:/"
#' dwnld.acs(tbls = tbls, raw_pth = raw_pth)
#' }
dwnld.acs <- function(tbls, raw_pth, ...) {
  
  check_census_key()
  
  f <- function(name, tbls, pth) {
    
    attr <- tbls[[name]]
    attr$table <- name
    
    test_attr(attr)
    
    df <- do.call(tidycensus::get_acs, attr)
    
    if (!is.null(pth)) {
      readr::write_csv(df, file.path(pth, paste0(name, ".csv")))
    }
    
    attr$df <- df
    return(structure(attr, class = name))
  }
  
  sapply(names(tbls),
         f,
         tbls = tbls,
         pth = raw_pth,
         USE.NAMES = TRUE,
         simplify = FALSE)
}

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

#' @title Download ACF data
#' @description Data are located: 
#' https://www.twc.texas.gov/programs/childcare#dataAndReports
dwnld.acf <- function(raw_pth,
                      endpoint = "https://www.twc.texas.gov{path}",
                      sheet = "ChildrenParentsSettings",
                      ...) {

  apis <- xml2::read_html(glue::glue(endpoint,
                                     path = "/programs/childcare#dataAndReports")) %>%
    rvest::html_nodes("#node-561 > div:nth-child(5) > div > ul:nth-child(16) > li > a") %>%
    rvest::html_attr("href")

  pth <- file.path(raw_pth, "acf")

  if (!dir.exists(pth)) {
    dir.create(pth)
  }

  lapply(apis, function(api) {

    fl <- basename(api)

    dir <- file.path(pth, fl)
    
    if (fl %in% list.files(pth) == F) {

      url <- glue::glue(endpoint, path = api)

      httr::GET(url, httr::write_disk(temp_dir <- tempfile(fileext = ".xlsx")))
      file.copy(from = temp_dir, to = dir)
    }
  })
  return(TRUE)
}

#' @title Get county and ZCTA (zip code) Crosswalk
#' @description Downloads the crosswalk between county fips codes and county 
#' names for Texas.
#' @param pth Link to data: 
#' https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
dwnld.zip_county_xwalk <- function(pth = "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt",
                                   state_fips = 48) {
  
  df <- read.csv(url(pth)) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(state == state_fips) %>%
    dplyr::select(zip = zcta5,
                  county_code = county) %>% 
    dplyr::mutate(county_code = paste0(state_fips,
                                       stringr::str_pad(county_code, side = "left",
                                                 width = 3, pad = "0")))
  
  assertthat::assert_that(all(c("zip", "county_code") %in% colnames(df)),
                          msg = "Zip county xwalk missing column")
  
  return(df)
}

#' @title Get ZCTA (Zip Code) and Latitude/Longitude Coordinate Crosswalk
#' @description Downloads the crosswalk between zip codes and latitude longitude
#' coordinates of the center for Texas
#' @export
dwnld.geo_zip <- function(state_fips = 48) {

  tigris::zctas(state = state_fips) %>% 
    dplyr::mutate(zip = as.numeric(ZCTA5CE10),
                  lat = as.numeric(INTPTLAT10),
                  lon = as.numeric(INTPTLON10)) %>% 
    dplyr::select(zip, lat, lon)
}
