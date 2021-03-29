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