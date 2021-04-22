#' @title Add default parameters for ACS tables
#' @inheritParams childcare_db
acs_tables <- function(acs_year,
                       acs_state_code,
                       acs_geography,
                       acs_county,
                       raw_pth,
                       processed_pth) {

  list(tbls = 
         list(B23008 = list(year = acs_year,
                            state = acs_state_code,
                            geography = acs_geography,
                            county = acs_county),
              B17024 = list(year = acs_year,
                            state = acs_state_code,
                            geography = acs_geography,
                            county = acs_county)
         ),
         raw_pth = raw_pth,
         processed_pth = processed_pth
  )
}

#' @title Create child care data base
#' @param root string. Path to the root directory to create the DB
#' @param acs_year. Integer. ACS year to pull the tables from. Default is 2019.
#' @param acs_state_code. Integer. ACS state_code to pull the data for. Default 
#' is 48 (Texas).
#' @param acs_geography. String. ACS geography. Default is "tract".
#' @param acs_county. Integer. County FIPS code (3-digits). Default is 439 
#' (Tarrant County).
#' @examples
#' \dontrun{
#' root <- "C:/"
#' childcare_db(root = root)
#' }
childcare_db <- function(root,
                         radius = 3,
                         acs_year = 2019,
                         acs_state_code = 48,
                         acs_geography = "tract",
                         acs_county = 439) {

  data_pth <- file.path(root, "data")
  raw_pth <- file.path(data_pth, "raw")
  processed_pth <- file.path(data_pth, "processed")

  pths <- c(data_pth, raw_pth, processed_pth)

  create_folder_str(pths = pths)

  process.acs(acs_year = acs_year,
              acs_state_code = acs_state_code,
              acs_geography = acs_geography,
              acs_county = acs_county,
              raw_pth = raw_pth,
              processed_pth = processed_pth)

  acf <- list(raw_pth = raw_pth,
              processed_pth = processed_pth)
 
  process.acf(acf = acf)
  
  hhsc_ccl <- list(raw_pth = raw_pth,
              processed_pth = processed_pth)
  
  process.hhsc_ccl(hhsc_ccl = hhsc_ccl)
  
  tracts_xwalk <- list(raw_pth = raw_pth,
                       processed_pth = processed_pth,
                       name = "tracts_xwalk",
                       radius=radius)
  
  process.tracts_xwalk(tracts_xwalk = tracts_xwalk)
}
