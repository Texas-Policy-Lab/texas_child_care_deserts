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
#' @param pth. Path to save the raw data.
#' @details To find a list of parameters to pass see documentation for 
#' tidycensus::get_acs
#' @examples 
#' \dontrun{
#' x <- list(B23008 = list(year = 2019, state = 48, 
#'                                geography = "tract", county = 439))
#' dwnld.acs(x = x)
#' }
dwnld.acs <- function(x, ...) {

  test_attr(x)

  x$df <- do.call(tidycensus::get_acs, x)

  if (!is.null(x$pth)) {
    readr::write_csv(x$df, 
                     file.path(x$pth,
                               paste0(x$table, ".csv")))
  }

  return(x)
}

#' @title Choose geography
#' @description Applied different data management steps depending on which 
#' geography is selected
dm_acs_geo <- function(x) UseMethod("dm_acs_geo")

#' @title Data management steps for census tracts geos
dm_acs_geo.tract <- function(x) {

  assertthat::assert_that(all(nchar(x$df$GEOID) == 11))

  x$df <- x$df %>%
    dplyr::rename(tract = GEOID) %>%
    dplyr::mutate(county_code = substr(tract, 1, 5))

  assertthat::assert_that(all(nchar(x$df$county_code) == 5))
  assertthat::assert_that(length(unique(x$df$tract)) == nrow(x$df))

  return(x)
}

#' @title Data management steps for ZCTA geos
dm_acs_geo.zcta <- function(x) {

  assertthat::assert_that(all(nchar(x$df$GEOID)) == 5)

  x$df <- x$df %>% 
    dplyr::rename(zip = GEOID)

  assertthat::assert_that(length(unique(x$df$zip)) == nrow(x$df))

  return(x)  
}

#' @title Data management
#' @description Data management steps for a specific data table
#' @param x object. The data object
dm_acs <- function(x) UseMethod("dm_acs")
#' @title Default data managment steps
dm_acs.default <- function(x) {

  assertthat::assert_that(all(x$other_lt6 %in% x$df$variable), 
                          msg = "Missing expected variables to create those with working parents")

  x$df <- x$df %>%
    dplyr::select(GEOID, variable, estimate) %>%
    dplyr::mutate(n_kids_lt6 = variable %in% x$lt6,
                  n_kids_other_lt6 = variable %in% x$other_lt6) %>%
    tidyr::gather(n_kids, value2, -c(GEOID, variable, estimate)) %>%
    dplyr::filter(value2) %>%
    dplyr::group_by(GEOID, n_kids) %>%
    dplyr::summarise(estimate = sum(estimate)) %>%
    tidyr::spread(n_kids, estimate) %>%
    dplyr::mutate(n_kids_lt5 = 5/6*n_kids_lt6,
                  n_kids_lt4 = 4/6*n_kids_lt6)

  assertthat::assert_that(all(x$df$n_kids_lt5 <= x$df$n_kids_lt6))
  assertthat::assert_that(all(x$df$n_kids_lt4 <= x$df$n_kids_lt6))

  return(x)
}

#' @title Demand using table B23008
#' @description Creates variables: Estimated number of children under 5,
#' estimated number of children less than 6 with working parents, and
#' estimated number of children less than 5 with working parents
#' @inheritParams dm_acs
dm_acs.B23008 <- function(x) {

  x$df <- x$df %>%
    dplyr::rename(n_kids_working_parents_lt6 = n_kids_other_lt6) %>%
    dplyr::mutate(n_kids_working_parents_lt5 = 5/6*n_kids_working_parents_lt6,
                  n_kids_working_parents_lt4 = 4/6*n_kids_working_parents_lt6)
  
  assertthat::assert_that(all(x$df$n_kids_working_parents_lt5 <= x$df$n_kids_working_parents_lt6))
  assertthat::assert_that(all(x$df$n_kids_working_parents_lt4 <= x$df$n_kids_working_parents_lt6))
  assertthat::assert_that(all(x$df$n_kids_working_parents_lt6 <= x$df$n_kids_lt6))

  return(x)
}

#' @title Demand using table B17024
#' @inheritParams dm
dm_acs.B17024 <- function(x) {

  x$df <- x$df %>%
    dplyr::rename(n_kids_lt6_under200pct = n_kids_other_lt6) %>%
    dplyr::mutate(n_kids_lt4_under200pct = 4/6*n_kids_lt6_under200pct,
                  n_kids_lt5_under200pct = 5/6*n_kids_lt6_under200pct,
                  pct_kids_lt4_under200_pct = (n_kids_lt4_under200pct/n_kids_lt4)*100,
                  pct_kids_lt5_under200_pct = (n_kids_lt5_under200pct/n_kids_lt5)*100,
                  pct_kids_lt6_under200_pct = (n_kids_lt6_under200pct/n_kids_lt6)*100)

  assertthat::assert_that(max(x$df$pct_kids_lt5_under200_pct, na.rm = TRUE) <= 100)
  assertthat::assert_that(max(x$df$pct_kids_lt4_under200_pct, na.rm = TRUE) <= 100)
  assertthat::assert_that(all(x$df$n_kids_lt5_under200pct <= x$df$n_kids_lt5))
  assertthat::assert_that(all(x$df$n_kids_lt6_under200pct <= x$df$n_kids_lt6))
  assertthat::assert_that(all(x$df$n_kids_lt4_under200pct <= x$df$n_kids_lt4))

  return(x)
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

  assertthat::assert_that(pov_rate <= 1, 
                          msg = "Poverty rate should be less than or equal to 1")

  df <- B17024$df %>%
    dplyr::left_join(B23008$df %>% 
                       dplyr::select(-c(n_kids_lt6, n_kids_lt5, n_kids_lt4))) %>%
    dplyr::mutate(working_pov_rate = pov_rate * pct_kids_lt6_under200_pct) %>%
    dplyr::mutate(n_kids_lt6_working_under200_pct = (working_pov_rate/100) * n_kids_working_parents_lt6) %>%
    dplyr::mutate(n_kids_lt5_working_under200_pct = (working_pov_rate/100) * n_kids_working_parents_lt5,
                  n_kids_lt4_working_under200_pct = (working_pov_rate/100) * n_kids_working_parents_lt4)

  assertthat::assert_that(all(df$working_pov_rate <= 100, na.rm = TRUE))
  assertthat::assert_that(all(df$n_kids_lt5_working_under200_pct <= df$n_kids_lt6_working_under200_pct, na.rm = TRUE))
  assertthat::assert_that(all(df$n_kids_lt4_working_under200_pct <= df$n_kids_lt6_working_under200_pct, na.rm = TRUE))

  return(df)
}

#' @title Add default attributes for ACS tables
#' @inheritParams child_care_db
acs_attr <- function(acs_year,
                     acs_state_code,
                     acs_geography,
                     acs_county,
                     lt6 = "002",
                     pth) {

  cls <- function(table, geography, lt6, other_lt6, pth) {
    structure(
      list(table = table,
           year = acs_year,
           state = acs_state_code,
           geography = acs_geography,
           county = acs_county,
           lt6 = paste(table, lt6, sep = "_"),
           other_lt6 = paste(table, other_lt6, sep = "_"),
           pth = pth),
      class = c(table, geography))
  }

  B23008 <- cls(table = "B23008",
                geography = acs_geography,
                lt6 = lt6,
                other_lt6 = c("004", "005", "006", "010", "013"),
                pth = pth)

  B17024 <- cls(table = "B17024",
                geography = acs_geography,
                lt6 = lt6,
                other_lt6 = c("003", "004", "005", "006", "007", 
                              "008", "009", "010"),
                pth = pth)

  list(B17024 = B17024, 
       B23008 = B23008)
}

#' @title Process acs data
#' @description Process acs data and create demand dataframe
#' @export
process.acs <- function(acs_year,
                        acs_state_code,
                        acs_geography,
                        acs_county,
                        raw_pth) {

  get_key.census()

  f <- function(x) {
    x <- x %>%
      dwnld.acs() %>%
      dm_acs.default() %>%
      dm_acs() %>%
      dm_acs_geo() 
  }
  
  tbls <- acs_attr(acs_year = acs_year,
                   acs_state_code = acs_state_code,
                   acs_geography = acs_geography,
                   acs_county = acs_county,
                   pth = raw_pth)

  tbls <- sapply(tbls, f, USE.NAMES = TRUE, simplify = FALSE)
  do.call(dm.demand, tbls)
}
