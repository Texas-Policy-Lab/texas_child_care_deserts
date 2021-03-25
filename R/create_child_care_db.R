#' @title Create child care data base
#' @param root string. Path to the root directory to create the DB
#' @param acs_tbls. List of census tables to pull
#' @examples
#' \dontrun{
#' census_tbls <- list(B23008 = list(year = 2019, state = 48, 
#'                     geography = "tract", county = 439))
#' root <- "C:/"
#' childcare_db(acs_tbls = acs_tbls, root = root)
#' }
childcare_db <- function(acs_tbls,
                         root) {

  pth_data <- file.path(root, "data")
  pth_raw <- file.path(pth_data, "raw")
  pth_processed <- file.path(pth_data, "processed")

  pths <- c(pth_data, pth_raw, pth_processed)

  create_folder_str(pths = pths)

  acs_tbls <- list(tbls = acs_tbls,
                    pth_raw = pth_raw)

  process.acs(acs_tbls = acs_tbls)

}