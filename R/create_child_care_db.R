#' @title Create child care data base
#' @param root string. Path to the root directory to create the DB.
#' @param acf_qtr_years vector. Default is 'Q2-2019'.
#' @examples
#' \dontrun{
#' root <- "C:/"
#' childcare_db(root = root)
#' }
childcare_db <- function(root,
                         acf_qtr_years = "Q2-2019") {

  data_pth <- file.path(root, "data")
  raw_pth <- file.path(data_pth, "raw")
  processed_pth <- file.path(data_pth, "processed")

  pths <- c(data_pth, raw_pth, processed_pth)

  create_folder_str(pths = pths)

  # if (!is.null(census_tbls)) {
  #   dwnld.acs(tbls = census_tbls,
  #             pth = raw_pth)
  # }
  # 
  # hhsc_ccl <- list()
  # hhsc_ccl$raw_pth <- raw_pth
  # hhsc_ccl$processed_pth <- processed_pth
  # hhsc_ccl$name <- "HHSC_CCL"

  # process.hhsc_ccl(hhsc_ccl = hhsc_ccl)

  acf <- list()
  acf$raw_pth <- raw_pth
  acf$processed_pth <- processed_pth
  acf$acf_qtr_years <- acf_qtr_years

  process.acf(acf = acf)
}
