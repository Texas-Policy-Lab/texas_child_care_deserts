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

#' @title Data management
#' @description Data management steps for a specific data table
#' @param x object. The data object
dm <- function(x) UseMethod("dm")

#' @title Demand using table B23008
#' @description Creates variables: Estimated number of children under 5,
#' estimated number of children less than 6 with working parents, and
#' estimated number of children less than 5 with working parents
#' @inheritParams dm
dm.B23008 <- function(x) {

  lt6 <- paste(x$table, "002", sep = "_")
  lt6_working_parents <- paste(x$table, c("004", "005", "006", "010", "013"), sep = "_")
  
  assertthat::assert_that(all(lt6_working_parents %in% x$df$variable), msg = "Missing expected variables to create those with working parents")

  df <- x$df %>%
    dplyr::rename(tract = GEOID) %>%
    dplyr::select(tract, variable, estimate) %>%
    dplyr::mutate(n_kids_lt6 = ifelse(variable %in% lt6, TRUE, FALSE),
                  n_kids_working_parents_lt6 = ifelse(variable %in% lt6_working_parents, TRUE, FALSE)
    ) %>%
    tidyr::gather(variable2, value2, -c(tract, variable, estimate)) %>%
    dplyr::filter(value2) %>%
    dplyr::group_by(tract, variable2) %>%
    dplyr::summarise(estimate = sum(estimate)) %>%
    tidyr::spread(variable2, estimate) %>%
    dplyr::mutate(n_kids_lt5 = 5/6*n_kids_lt6,
                  n_kids_working_parents_lt5 = 5/6*n_kids_working_parents_lt6)

  assertthat::assert_that(length(unique(df$tract)) == nrow(df))
  assertthat::assert_that(all(df$n_kids_lt5 <= df$n_kids_lt6))
  assertthat::assert_that(all(df$n_kids_working_parents_lt5 <= df$n_kids_working_parents_lt6))
  assertthat::assert_that(all(df$n_kids_working_parents_lt6 <= df$n_kids_lt6))

  return(df)
}

#' @title Demand using table B17024
#' @inheritParams dm
dm.B17024 <- function(x) {
  
  lt6 <- paste(x$table, "002", sep = "_")
  lt6_under200_pct <- paste(x$table, c("003", "004", "005", "006",
                                       "007", "008", "009", "010"), sep = "_")
  
  assertthat::assert_that(all(lt6_under200_pct %in% x$df$variable), msg = "Missing expected variables to create less than 200 pct")

  df <- x$df %>%
    dplyr::rename(tract = GEOID) %>%
    dplyr::select(tract, variable, estimate) %>%
    dplyr::mutate(n_kids_lt6 = ifelse(variable %in% lt6, TRUE, FALSE),
                  n_kids_lt6_under200pct = ifelse(variable %in% lt6_under200_pct, TRUE, FALSE)
    ) %>%
    tidyr::gather(variable2, value2, -c(tract, variable, estimate)) %>%
    dplyr::filter(value2) %>%
    dplyr::group_by(tract, variable2) %>%
    dplyr::summarise(estimate = sum(estimate)) %>% 
    tidyr::spread(variable2, estimate) %>%
    dplyr::mutate(n_kids_lt5 = 5/6*n_kids_lt6,
                  n_kids_lt5_under200pct = 5/6*n_kids_lt6_under200pct,
                  pct_kids_lt6_under200_pct = (n_kids_lt6_under200pct/n_kids_lt6)*100,
                  pct_kids_lt5_under200_pct = (n_kids_lt5_under200pct/n_kids_lt5)*100)

  assertthat::assert_that(max(df$pct_kids_lt5_under200_pct, na.rm = TRUE) <= 100)
  assertthat::assert_that(all(df$n_kids_lt5_under200pct <= df$n_kids_lt5))
  assertthat::assert_that(all(df$n_kids_lt6_under200pct <= df$n_kids_lt6))
  assertthat::assert_that(all(df$n_kids_lt5 <= df$n_kids_lt6))

  return(df)
}

#' @title Data management steps to create the demand table
#' @description Children in poverty with working parents
#' @param B17024 data.frame. Census table B17024
#' @param B23008 data.frame. Census table B23008
#' @param processed_pth pth. Path to the data
#' @param name. The name to write the data out as.
#' @param pov_rate children w/ working parents are (1-pov_rate) more likely 
#' to be under 200% of the poverty line. Default is .85.
dm.demand <- function(B17024,
                      B23008,
                      processed_pth,
                      name = "demand",
                      pov_rate = .85,
                      ...) {

  df <- B17024 %>%
    dplyr::inner_join(B23008) %>%
    dplyr::mutate(working_pov_rate = pov_rate * pct_kids_lt6_under200_pct) %>%
    dplyr::mutate(n_kids_lt6_working_under200_pct = (working_pov_rate/100) * n_kids_working_parents_lt6) %>%
    dplyr::mutate(n_kids_lt5_working_under200_pct = (working_pov_rate/100) * n_kids_working_parents_lt5)

  assertthat::assert_that(all(df$working_pov_rate <= 100, na.rm = TRUE))
  assertthat::assert_that(all(df$n_kids_lt5_working_under200_pct <= df$n_kids_lt6_working_under200_pct, na.rm = TRUE))

  readr::write_csv(df, file.path(processed_pth, paste(name, "csv", sep = ".")))

  return(df)
}

#' @title Process acs data
#' @description Process acs data and create demand dataframe
process.acs <- function(acs_year,
                        acs_state_code,
                        acs_geography,
                        acs_county,
                        raw_pth,
                        processed_pth) {

  acs_tbls <- acs_tables(acs_year = acs_year,
                         acs_state_code = acs_state_code,
                         acs_geography = acs_geography,
                         acs_county = acs_county,
                         raw_pth = raw_pth,
                         processed_pth = processed_pth)

  tbls <- do.call(dwnld.acs, acs_tbls)
  acs_tbls <- c(acs_tbls, lapply(tbls, dm))
  do.call(dm.demand, acs_tbls)
}
