#' @title Create child care data base
#' @param root string. Path to the root directory to create the DB
#' @param census_tbls. List of census tables to pull
#' @examples
#' \dontrun{
#' census_tbls <- list(B23008 = list(year = 2019, state = 48, 
#'                     geography = "tract", county = 439))
#' root <- "C:/"
#' childcare_db(census_tbls = census_tbls, root = root)
#' }
childcare_db <- function(census_tbls,
                         root) {

  pth_data <- file.path(root, "data")
  pth_raw <- file.path(pth_data, "raw")
  pth_processed <- file.path(pth_data, "processed")

  pths <- c(pth_data, pth_raw, pth_processed)

  create_folder_str(pths = pths)

  dwnld.acs(tbls = census_tbls,
            pth = pth_raw)

}