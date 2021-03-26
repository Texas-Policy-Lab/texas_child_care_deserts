#' @title Checks to make sure the census API key is set as an environment variable
#' @param env_var. String. The environment variable to check for. Default is CENSUS_API_KEY
#' @examples 
#' \dontrun{
#' key <- "X"
#' tidycensus::census_api_key(key)
#' check_census_key()
#' }
#' @return TRUE if a census api key exists, error if it does not exist
check_census_key <- function(env_var = "CENSUS_API_KEY") {
  msg <- "Please sign up for a census API key at:
          'https://api.census.gov/data/key_signup.html'. Then install the key 
          using tidycensus::census_api_key(key = 'X'), where 'X' is the key
          you received from the Census."
  
  assertthat::assert_that(Sys.getenv(env_var) != "",
                          msg = msg)
}

#' @title Test attributes
#' @description Test's that the attributes of the parameters passed in are 
#' correct
#' @title attr. List of attributes to test
test_attr <- function(attr) {
  assertthat::assert_that(c("geography") %in% names(attr),
                          msg = "Table configuration is missing geography parameter")
  assertthat::assert_that(any(c("variables", "table") %in% names(attr)),
                          msg = "Table configuration is missing variables or table parameter")
  if(all(c("variables", "table") %in% names(attr))) {
    assertthat::assert_that(all(attr$table == sub("_.*", "", attr$variables)),
                            msg = "Table name differs from variable root")  
  }
  return(TRUE)
}

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