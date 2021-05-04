#' @title Add default parameters for ACS tables
#' @inheritParams childcare_db
acs_tables <- function(acs_year,
                       acs_state_code,
                       acs_geography,
                       acs_county,
                       raw_pth) {

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
         raw_pth = raw_pth)
}

#' @title Create child care data base
#' @param root string. Path to the root directory to create the DB.
#' @param acf_qtr_years vector. Default is 'Q2-2019'.
#' @param acs_year. Integer. ACS year to pull the tables from. Default is 2019.
#' @param acs_state_code. Integer. ACS state_code to pull the data for. Default 
#' is 48 (Texas).
#' @param acs_geography. String. ACS geography. Default is "tract".
#' @param acs_county. Integer. County FIPS code (3-digits). Default is 439 
#' (Tarrant County).
#' @examples
#' \dontrun{
#' root <- "C:/"
#' child_care_db(root = root)
#' }
child_care_db <- function(root,
                         acf_qtr_years = NULL,
                         acs_year = 2019,
                         acs_state_code = 48,
                         acs_geography = "tract",
                         acs_county = NULL,
                         db_name = "child_care_env.Rdata") {

  env <- new.env()
  data_pth <- file.path(root, "data")
  raw_pth <- file.path(data_pth, "raw")
  processed_pth <- file.path(data_pth, "processed")
  pths <- c(data_pth, raw_pth, processed_pth)

  create_folder_str(pths = pths)

  env$DF_ACF <- process.acf(cls =
                              list(raw_pth = raw_pth,
                                   acf_qtr_years = acf_qtr_years)
  )

  env$DF_DEMAND <- process.acs(acs_year = acs_year,
                               acs_state_code = acs_state_code,
                               acs_geography = acs_geography,
                               acs_county = acs_county,
                               raw_pth = raw_pth)

  env$DF_HHSC_CCL <- process.hhsc_ccl(cls = list(raw_pth = raw_pth,
                                                 name = "HHSC_CCL"))

  env$XWALK_TRACTS <- process.tracts_xwalk(cls = list(raw_pth = raw_pth))

  save(env, file = file.path(processed_pth, db_name))
}

#' @title Save a subset of the child care database
#' @param pth string. Path to the root directory to create the DB.
#' @param county string. County 5-digit FIPS code.
#' @examples
#' \dontrun{
#' pth <- "C:/"
#' county <- "48439"
#' save_subset_child_care_db(pth = pth, county = county)
#' }
save_subset_child_care_db <- function(pth, county) {

  check_type.character(county)

  assertthat::assert_that(nchar(county) == 5,
                          msg = "Please enter a string 5-digit FIPS code")

  if(file.exists(pth)) {

    load(pth)

    for (name in names(env)) {

      i <- grep("anchor_county|family_fips_code", names(env[[name]]))
      n <- names(env[[name]])[i]
      names(env[[name]])[i] <- "county_code"

      env[[name]] <- env[[name]] %>% 
        dplyr::filter(county_code == county)

      names(env[[name]])[i] <- n
    }

    save(env, file = file.path(dirname(pth), paste(county, basename(pth), sep = "_")))
  } else {
    assertthat::assert_that(FALSE, 
                           msg = "Please run child_care_db() function to create
                                  the universe of child care data before subsetting.")
  }
}
