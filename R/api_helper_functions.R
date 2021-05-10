#' @title Set Mapquest API Key
mapquest_api_key <- function(key) {
  Sys.setenv(MAPQUEST_API_KEY = key)
}

#' @title Get key
#' @description Checks to make sure API exists
get_key <- function(env_var) UseMethod("check_key")

get_key.default <- function(env_var,
                            title,
                            url,
                            install,
                            msg = "Please sign up for a {title} API key at: '{url}'. 
                            Then install the key using {install}, where 'X' is the key you received from {title}.") {

  if (Sys.getenv(env_var) != "") {
    Sys.getenv(env_var)
  } else {
    assertthat::assert_that(FALSE,
                            msg = glue::glue(msg, 
                                             title = title, 
                                             url = url, 
                                             install = install))
  }
}

#' @title Checks to make sure the census API key is set as an environment variable
#' @examples 
#' \dontrun{
#' tidycensus::census_api_key(key = "X")
#' texascc::get_key.census()
#' }
#' @return TRUE if a census api key exists, error if it does not exist
get_key.census <- function(env_var = "CENSUS_API_KEY",
                           title = "Census",
                           url = "https://api.census.gov/data/key_signup.html",
                           install = "tidycensus::census_api_key(key = 'X')") {

  get_key.default(env_var = env_var,
                  title = title, 
                  url = url, 
                  install = install)
}

#' @title Checks to make sure the census API key is set as an environment variable
#' @param env_var. String. The environment variable to check for. 
#' @examples 
#' \dontrun{
#' texascc::mapquest_api_key(key = "X")
#' texascc::check_key.mapquest()
#' }
#' @return TRUE if a census api key exists, error if it does not exist
check_key.mapquest <- function(env_var = "MAPQUEST_API_KEY",
                               title = "MAPQUEST",
                               url = "https://developer.mapquest.com/",
                               install = "texascc::mapquest_api_key(key = 'X')") {

  get_key.default(env_var = env_var,
                  title = title, 
                  url = url, 
                  install = install)
}

