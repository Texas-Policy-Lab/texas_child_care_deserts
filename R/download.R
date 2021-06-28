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

  get_key.census()

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
dwnld.xwalk_zip_county <- function(pth = "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt",
                                   state_fips) {

  df <- read.csv(url(pth)) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(state == state_fips) %>%
    dplyr::select(zip = zcta5,
                  county_code = county) %>%
    dplyr::mutate(county_code = paste0(state_fips,
                                       stringr::str_pad(county_code, 
                                                        side = "left",
                                                        width = 3, 
                                                        pad = "0")),
                  zip = as.character(zip))

  assertthat::assert_that(all(c("zip", "county_code") %in% colnames(df)),
                          msg = "Zip county xwalk missing column")
  assertthat::assert_that(all(nchar(df$zip) == 5))
  assertthat::assert_that(all(nchar(df$county_code) == 5))
  
  return(df)
}

#' @title Get ZCTA (Zip Code) and Latitude/Longitude Coordinate Crosswalk
#' @description Downloads the crosswalk between zip codes and latitude longitude
#' coordinates of the center for Texas
#' @export
dwnld.geo_zip <- function() {

  tigris::zctas() %>% 
    dplyr::mutate(zip = as.character(ZCTA5CE10),
                  lat = as.numeric(INTPTLAT10),
                  lon = as.numeric(INTPTLON10)) %>% 
    dplyr::select(zip, lat, lon)
}

#' @title Get State FIPS and State Name Crosswalk
#' @description Downloads the crosswalk between county fips codes and county 
#' names for Texas
#' @export
dwnld.lu_county_code <- function(state_fips) {

  df <- tigris::fips_codes %>%
    dplyr::filter(state_code == state_fips) %>%
    dplyr::select(county_code, county) %>% 
    dplyr::mutate(county_code = paste0(state_fips, county_code))

  assertthat::assert_that(nrow(df) == 254)
  assertthat::assert_that(all(nchar(df$county_code) == 5))

  return(df)
}

#' @title Get tract by latitude and longitude
#' @description Downloads shape file for Texas (48) using the tigris package, 
#' which pulls the most recent shape from the United States Census Bureau.
#' @export
dwnld.geo_tracts <- function(state_fips) {

  geo <- tigris::tracts(state = state_fips, cb = TRUE) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename(tract = geoid) %>%
    dplyr::mutate(county_code = paste0(statefp, countyfp)) %>%
    dplyr::select(tract, county_code, geometry) %>% 
    dplyr::mutate(id = seq(1, dplyr::n(), 1))

  x <- geo %>%
    sf::st_coordinates(geometry) %>% 
    data.frame() %>%
    dplyr::rename(id = L3) %>% 
    dplyr::inner_join(geo) %>% 
    dplyr::select(X, Y, tract, county_code)
  
}

#' @title Get tract by latitude and longitude
#' @description Downloads shape file for Texas (48) using the tigris package, 
#' which pulls the most recent shape from the United States Census Bureau.
#' @export
dwnld.geo_county <- function(state_fips) {
  
  geo <- tigris::counties(state = "48", cb = TRUE) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(county_code = paste0(statefp, countyfp)) %>%
    dplyr::select(county_code, geometry) %>%
    dplyr::mutate(id = seq(1, dplyr::n(), 1))
  
  x <- geo %>%
    sf::st_coordinates(geometry) %>% 
    data.frame() %>%
    dplyr::rename(id = L3) %>% 
    dplyr::inner_join(geo) %>% 
    dplyr::select(X, Y, L2, county_code)
}

#' @title Get the Harris County Neighborhood to census tract data
#' @description Download the neighbordhood to census tract cross walk create by 
#' the Kinder Institute. 
#' https://www.arcgis.com/apps/MapSeries/index.html?appid=95320b06677c438d91027cb5feb241bf
#' @return data.frame
dwnld.harris_neighborhood <- function(url = "https://www.datahouston.org/cta_crosswalk.txt",
                                      name = "HARRIS_NEIGHBORHOOD",
                                      ext = "csv",
                                      raw_pth) {

  dwnld_pth <- file.path(raw_pth, paste(name, ext, sep = "."))
  
  if (!file.exists(dwnld_pth)) {
    download.file(url, destfile = dwnld_pth, mode = "wb") 
  }

  df <- readr::read_csv(dwnld_pth)
}

#' @title Get the Tarrant County Neighborhood data
#' @return data.frame
dwnld.tarrant_neighborhood <- function(url = "https://data.fortworthtexas.gov/api/views/ruhd-2sjc/rows.csv?accessType=DOWNLOAD",
                                       name = "TARRANT_NEIGHBORHOOD",
                                       ext = "csv",
                                       raw_pth) {

  dwnld_pth <- file.path(raw_pth, paste(name, ext, sep = "."))

  if (!file.exists(dwnld_pth)) {
    download.file(url, destfile = dwnld_pth, mode = "wb") 
  }

  df <- readr::read_csv(dwnld_pth)
}

