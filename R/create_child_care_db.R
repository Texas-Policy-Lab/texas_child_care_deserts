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

  data_pth <- file.path(root, "data")
  raw_pth <- file.path(data_pth, "raw")
  processed_pth <- file.path(data_pth, "processed")

  pths <- c(data_pth, raw_pth, processed_pth)

  create_folder_str(pths = pths)

  acs_tbls <- list(tbls = acs_tbls,
                   raw_pth = raw_pth,
                   processed_pth = processed_pth)

  process.acs(acs_tbls = acs_tbls)

}