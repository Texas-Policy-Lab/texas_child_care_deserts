#' @title Create data folder structure
#' @param root string. The root directory
#' @param pth list. A list of paths to create in the root directory
create_folder_str <- function(pths) {

  lapply(pths, function(pth) {

    if(!dir.exists(pth)) {
      dir.create(pth, recursive = TRUE)
    }
  })
}

#' @title Load environment
#' @description Loads the saved environment
#' @param pth string. Path the the environment
load_env <- function (pth) {

  load(pth)

  for (i in names(env)) {
    assign(i, env[[i]], envir = .GlobalEnv)
  }

}

#' @title Check key
#' @description Checks to make sure API exists
check_key <- function(env_var) UseMethod("check_key")

check_key.default <- function(env_var,
                              msg = "Please sign up for a {title} API key at:
                              '{url}'. Then install the key using {install}, 
                              where 'X' is the key you received from {title}.",
                              title,
                              url,
                              install) {

  assertthat::assert_that(Sys.getenv(env_var) != "",
                          msg = glue::glue(msg, 
                                           title = title, 
                                           url = url, 
                                           install = install))
}

#' @title Checks to make sure the census API key is set as an environment variable
#' @param env_var. String. The environment variable to check for. Default is CENSUS_API_KEY
#' @examples 
#' \dontrun{
#' key <- "X"
#' tidycensus::census_api_key(key)
#' check_key()
#' }
#' @return TRUE if a census api key exists, error if it does not exist
check_key.census <- function(env_var = "CENSUS_API_KEY",
                             title = "Census",
                             url = "https://api.census.gov/data/key_signup.html",
                             install = "tidycensus::census_api_key(key = 'X')") {

  check_key.default(env_var = env_var,
                    title = title, 
                    url = url, 
                    install = install)
}

#' @title Checks to make sure the census API key is set as an environment variable
#' @param env_var. String. The environment variable to check for. Default is CENSUS_API_KEY
#' @examples 
#' \dontrun{
#' key <- "X"
#' tidycensus::census_api_key(key)
#' check_key()
#' }
#' @return TRUE if a census api key exists, error if it does not exist
check_key.mapquest <- function(env_var = "MAPQUEST_API_KEY",
                               title = "MAPQUEST",
                               url = "https://developer.mapquest.com/",
                               install = "texascc::mapquest_api_key(key = 'X')") {

  check_key.default(env_var = env_var,
                    title = title, 
                    url = url, 
                    install = install)
}
