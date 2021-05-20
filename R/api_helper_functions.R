#' @title Set API Key
#' @param key. String. The API key credential. 
api_key <- function(key) UseMethod("api_key")

#' @title Set Census API Key
#' @inheritParams api_key
api_key.census <- function(key) {
  Sys.setenv(CENSUS_API_KEY = key)
}

#' @title Set Mapquest API Key
#' @inheritParams api_key
api_key.mapquest <- function(key) {
  Sys.setenv(MAPQUEST_API_KEY = key)
}

#' @title Set Google API Key
#' @inheritParams api_key
api_key.google <- function(key) {
  Sys.setenv(GOOGLE_API_KEY = key)
}

#' @title Set Geocodio API Key
#' @inheritParams api_key
api_key.geocodio <- function(key) {
  Sys.setenv(GEOCODIO_API_KEY = key)
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
#' texascc::api_key.census(key = "X")
#' texascc::get_key.census()
#' }
#' @return TRUE if a census api key exists, error if it does not exist
get_key.census <- function(env_var = "CENSUS_API_KEY",
                           title = "Census",
                           url = "https://api.census.gov/data/key_signup.html",
                           install = "texascc::api_key.census(key = 'X')") {

  get_key.default(env_var = env_var,
                  title = title, 
                  url = url, 
                  install = install)
}

#' @title Checks to make sure the mapquest API key is set as an environment 
#' variable
#' @param env_var. String. The environment variable to check for. 
#' @examples 
#' \dontrun{
#' texascc::api_key.mapquest(key = "X")
#' texascc::get_key.mapquest()
#' }
#' @return TRUE if a census api key exists, error if it does not exist
get_key.mapquest <- function(env_var = "MAPQUEST_API_KEY",
                             title = "MAPQUEST",
                             url = "https://developer.mapquest.com/",
                             install = "texascc::api_key.mapquest(key = 'X')") {

  get_key.default(env_var = env_var,
                  title = title, 
                  url = url, 
                  install = install)
}

#' @title Checks to make sure the google API key is set as an environment 
#' variable
#' @param env_var. String. The environment variable to check for. 
#' @examples 
#' \dontrun{
#' texascc::api_key.google(key = "X")
#' texascc::get_key.google()
#' }
#' @return TRUE if a census api key exists, error if it does not exist
get_key.google <- function(env_var = "Google_API_KEY",
                           title = "Goggle",
                           url = "https://console.cloud.google.com/project/_/apiui/credential",
                           install = "texascc::api_key.google(key = 'X')") {

  get_key.default(env_var = env_var,
                  title = title, 
                  url = url, 
                  install = install)
}

#' @title Checks to make sure the geocodio API key is set as an environment 
#' variable
#' @param env_var. String. The environment variable to check for. 
#' @examples 
#' \dontrun{
#' texascc::api_key.geocodio(key = "X")
#' texascc::get_key.geocodio()
#' }
#' @return TRUE if a census api key exists, error if it does not exist
get_key.geocodio <- function(env_var = "Geocodio_API_KEY",
                             title = "Geocodio",
                             url = "https://dash.geocod.io/apikey/create",
                             install = "texascc::api_key.geocodio(key = 'X')") {
  
  get_key.default(env_var = env_var,
                  title = title, 
                  url = url, 
                  install = install)
}
