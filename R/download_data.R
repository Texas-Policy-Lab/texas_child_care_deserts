#' @title Read file by extension
#' @inheritParams read_file_from_dwnld
#' @param sheet. integer. The sheet to read in. If it's a csv then sheet is set 
#' to NULL
#' @param skip. integer. The number of rows to skip when reading in 
#' @return dataframe
#' @export
read_fl_ext <- function(pth, ...) {

  ext <- tools::file_ext(pth)

  if (ext == "csv") {
    df <- readr::read_csv(pth, ...)
  } else if (ext == "xlsx") {
    df <- readxl::read_xlsx(pth, ...)
  } else if (ext == "xls") {
    df <- readxl::read_xls(pth, ...)
  } else {
    df <- read.delim(pth)
  }
}

#' @title Unzip file
#' @description Unzips the file and writes it to the path
#' #@export
unzip_fl <- function(unzip_fl, temp, pth, df_name) {

  assertthat::assert_that(is.logical(unzip_fl),
                          msg = "Unzip file parameter should be logical")

  if (unzip_fl) {
    unzip(temp, exdir=pth)
    ext <- tools::file_ext(df_name)
    temp <- file.path(pth, df_name)
  } else {
    temp
  }
}

#' @title Read file from download
#' @param temp. Object. The temporary file which is downloaded
#' @param unzip_fl. logical. Unzips a file if the file is zipped.
#' @return data.frame
#' @export
read_file_from_dwnld <- function(temp,
                                 unzip_fl,
                                 pth,
                                 df_name,
                                 ...) {

  unzip_fl(unzip_fl = unzip_fl,
           temp = temp,
           pth = pth,
           df_name = df_name)

  df <- read_fl_ext(pth = temp, ...)

  file.copy(from = temp, to = file.path(pth, df_name))

  return(df)
}

#' @title Verify 200 status.
#' @description Verify that the status of the request is 200.
#' @param status integer. The status of the request.
#' @param pth. string. The path to write the data to.
#' @param df_name. string. The name of the file to write the data to.
#' @param unzip. logical. TRUE if the file needs to be unzipped.
#' @inheritParams read_file_from_dwnld
#' @export
verify_200_status <- function(status, temp,
                              pth, df_name, unzip_fl, ...) {

  if(status == 200) {

    df <- read_file_from_dwnld(temp = temp,
                               unzip_fl = unzip_fl,
                               pth = pth,
                               df_name = df_name,
                               ...)

  } else {
    warning(paste("Data not found and data cannot be retrieved from the url", url))
  }
}


#' @title Downloads data from a url and saves data
#' @param url string. The URL to get the data from
#' @param pth string. The path to write the data to
#' @param df_name string. The name to save the data to.
#' @param ext string. File extension.
#' will skip the first x (specified rows).
#' @param update. boolean. Indicates if the csv file should be updated.
#' @param unzip. logical. TRUE if the file needs to be unzipped.
dwnld_df <- function(url,
                     pth,
                     df_name = NULL,
                     update = FALSE,
                     unzip_fl = FALSE,
                     ...) {

  if (is.null(df_name)) {
    df_name <- basename(url)
  }

  ext <- tools::file_ext(df_name)

  if (!file.exists(file.path(pth, df_name)) | update) {

    r <- httr::GET(url, httr::write_disk(temp <- tempfile(fileext = ext)))

    df <- verify_200_status(status = r$status,
                            temp = temp,
                            pth = pth,
                            df_name = df_name,
                            unzip_fl = unzip_fl,
                            ...)

  } else {

    df <- read_fl_ext(pth = file.path(pth, df_name), ...)
  }

  return(df)
}

#' @title Get NBER Tract data
#' @description Link to data: http://data.nber.org/distance/2010/sf1/tract/sf12010tractdistance25miles.csv
#' @details 25 mile radius is used rather then the 5 miles radius because the 5 mile radius was missing two counties in Harris County
#' @param data_in_name string. The name of the file to download.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.nber_tract_data <- function(data_in_name,
                                data_in_pth,
                                url = "http://data.nber.org/distance/2010/sf1/tract/{fl}") {

  dwnld_pth <- file.path(data_in_pth, data_in_name)

  download.file(glue::glue(url, fl = data_in_name), dwnld_pth)
}

#' @title Get ACF data
#' @description Link to data: https://www.twc.texas.gov/programs/childcare#dataAndReports
#' @param name string. The name to of the data to read in.
#' @param raw_pth string. The path to read the data in from.
#' @param year string. The year to select to read data from.
#' @export
dwnld.acf_data <- function(name = "acf-801-q{qtr}-{year}-twc.xlsx",
                         raw_pth,
                         url = "https://www.twc.texas.gov/files/partners/{fl}",
                         qtr = 1:4,
                         year) {
  
  lapply(qtr, function(q) {
    
    fl_name <- glue::glue(name, qtr = q, year = year)
    
    dwnld_pth <- file.path(raw_pth, fl_name)
    
    download.file(glue::glue(url, fl = fl_name), destfile = dwnld_pth, mode = "wb")
    
  })
}
  

#' @title Get the neighborhood to census tract data
#' @description Download the neighbordhood to census tract cross walk create by the Kinder Institute. https://www.arcgis.com/apps/MapSeries/index.html?appid=95320b06677c438d91027cb5feb241bf
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.kinder_neighborhood_tract_xwalk <- function(data_in_name = NULL,
                                                data_in_pth = NULL,
                                                pth = "https://www.datahouston.org/cta_crosswalk.txt") {

  df <- read.csv(url(pth)) %>%
    dplyr::rename(anchor_tract = GEOID10) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(-id)

  # write.csv(df, file.path(data_in_pth, data_in_name), row.names = FALSE)
}

#' @title Get tract by latitude and longitude
#' @description Downloads shape file for Harris County (201) Texas (48) using the tigris package, which pulls the most recent shape from the United States Census Bureau.
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.tract_shape <- function(data_in_name,
                            data_in_pth,
                            state_fips = 48,
                            county_fips = 201) {

  geo <- tigris::tracts(state = state_fips, county = county_fips, cb = TRUE)

  geo <- geo %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename(anchor_tract = geoid)

  write.csv(geo, file.path(data_in_pth, data_in_name), row.names = FALSE)
}

#' @title Get State FIPS and State Name Crosswalk
#' @description Downloads the crosswalk between county fips codes and county names for Texas
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.state_fips_state_name_xwalk <- function(data_in_name,
                                            data_in_pth,
                                            state_fips = 48) {

  cnty <- tigris::counties(state = state_fips) %>%
    dplyr::select(NAME, NAMELSAD, COUNTYFP) %>%
    dplyr::rename(COUNTY_FIPS = COUNTYFP) %>%
    dplyr::rename_all(tolower)

  sf::st_geometry(cnty) <- NULL

  assertthat::assert_that(nrow(cnty) == 254)

  write.csv(cnty, file.path(data_in_pth, data_in_name), row.names = FALSE)
}

