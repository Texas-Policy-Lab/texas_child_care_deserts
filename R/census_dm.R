#' @title Test attributes
#' @description Test's that the attributes of the parameters passed in are 
#' correct
test_attr <- function(attr) {
  assertthat::assert_that(c("geography") %in% names(attr),
                          msg = "Table configuration is missing geography parameter")
  assertthat::assert_that(any(c("variables", "table") %in% names(attr)),
                          msg = "Table configuration is missing variables or table parameter")
  if(all(c("variables", "table") %in% names(attr))) {
    assertthat::assert_that(all(grepl(attr$table, attr$variables)),
                            msg = "Table name differs from variable root")  
  }
}

#' @title Download ACS
#' @description Passes in a list of parameters to download the ACS census
#' data using functions from the tidycensus package
#' @inheritParams tidycensus::get_acs
#' @param tbls
#' @param pth string. The path to download the data to.
#' @examples 
#' \dontrun{
#' census_tbls <- list(B23008 = list(year = 2019, state = 48, 
#'                                   geography = "tract", county = "48438"))
#' pth <- "C:/"
#' dwnld.acs(tbls = census_tbls, pth = pth)
#' }
dwnld.acs <- function(tbls,
                      pth) {

  lapply(names(tbls),
         function(name, tbls, pth) {

           attr <- tbls[[name]]
           attr$table <- name

           test_attr(attr)

           df <- do.call(tidycensus::get_acs, attr)

           readr::write_csv(df, file.path(pth, paste0(name, ".csv")))
         },
         tbls = tbls,
         pth = pth)
}