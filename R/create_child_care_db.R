#' @title Create child care data base
#' @param census_tbls Configuration for the list of census tables to 
#' pull.
#' @param root string. Path to the root directory to create the DB.
#' @examples
#' \dontrun{
#' census_tbls <- list(B23008 = list(year = 2019, state = 48, 
#'                     geography = "tract", county = 439))
#' root <- "C:/"
#' childcare_db(census_tbls = census_tbls, root = root)
#' }
childcare_db <- function(census_tbls,
                         root) {

  data_pth <- file.path(root, "data")
  raw_pth <- file.path(data_pth, "raw")
  processed_pth <- file.path(data_pth, "processed")

  pths <- c(data_pth, raw_pth, processed_pth)

  create_folder_str(pths = pths)

  if (!is.null(census_tbls)) {
    dwnld.acs(tbls = census_tbls,
              pth = raw_pth)
  }

  hhsc_ccl <- list()
  hhsc_ccl$raw_pth <- raw_pth
  hhsc_ccl$processed_pth <- processed_pth
  hhsc_ccl$name <- "HHSC_CCL"

  process.hhsc_ccl(hhsc_ccl = hhsc_ccl)
  
  tracts_xwalk <- list()
  tracts_xwalk$raw_pth <- raw_pth
  tracts_xwalk$processed_pth <- processed_pth
  tracts_xwalk$name <- "tracts_xwalk"
  
  process.tracts_xwalk(tracts_xwalk = tracts_xwalk)

}
