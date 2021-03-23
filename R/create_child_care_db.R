#' @title Create child care data base
#' @param census_tbls_config. Configuration for the list of census tables to 
#' pull.
#' @param hhsc_ccl_config. Configuration for the CCL_HHS to pull.
#' @param root string. Path to the root directory to create the DB.
#' @examples
#' \dontrun{
#' census_tbls <- list(B23008 = list(year = 2019, state = 48, 
#'                     geography = "tract", county = 439))
#' root <- "C:/"
#' childcare_db(census_tbls = census_tbls, root = root)
#' }
childcare_db <- function(census_tbls_config,
                         hhsc_ccl_config,
                         root) {

  data_pth <- file.path(root, "data")
  raw_pth <- file.path(data_pth, "raw")
  processed_pth <- file.path(data_pth, "processed")

  pths <- c(data_pth, raw_pth, processed_pth)

  create_folder_str(pths = pths)

  dwnld.acs(tbls = census_tbls,
            pth = raw_pth)

  process.hhsc_ccl(hhsc_ccl = hhsc_ccl_config,
                   raw_pth = raw_pth,
                   processed_pth = processed_pth)

}