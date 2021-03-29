#' @title Download ACS
#' @description Passes in a list of parameters to download the ACS census
#' data using functions from the tidycensus package
#' @param tbls list. List of census tables with attributes to download
#' @param pth string. The path to download the data to.
#' @details To find a list of parameters to pass see documentation for 
#' tidycensus::get_acs
#' @examples 
#' \dontrun{
#' census_tbls <- list(B23008 = list(year = 2019, state = 48, 
#'                                   geography = "tract", county = 439))
#' pth <- "C:/"
#' dwnld.acs(tbls = census_tbls, pth = pth)
#' }
dwnld.acs <- function(tbls,
                      pth) {
  check_census_key()
  
  lapply(names(tbls),
         function(name, tbls, pth) {
           attr <- tbls[[name]]
           attr$table <- name
           
           test_attr(attr)
           
           df <- do.call(tidycensus::get_acs, attr)
           
           if (!is.null(pth)) {
             readr::write_csv(df, file.path(pth, paste0(name, ".csv")))
           }
           return(TRUE)
         },
         tbls = tbls,
         pth = pth)
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