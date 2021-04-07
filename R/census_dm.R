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
