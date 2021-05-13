#' @title Add default parameters for ACS tables
#' @inheritParams child_care_db
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
                          state_code = 48,
                          acf_qtr_years = NULL,
                          acs_year = 2019,
                          acs_geography = "tract",
                          acs_county = NULL,
                          db_name = "child_care_env.Rdata") {

  data_pth <- file.path(root, "data")
  raw_pth <- file.path(data_pth, "raw")
  processed_pth <- file.path(data_pth, "processed")
  pths <- c(data_pth, raw_pth, processed_pth)
  create_folder_str(pths = pths)

  load_env(file.path(processed_pth, db_name))

  env <- new.env()

  env$DF_HHSC_CCL <- process.hhsc_ccl(cls = list(raw_pth = raw_pth,
                                                 processed_pth = processed_pth,
                                                 name = "HHSC_CCL",
                                                 state_fips = state_code))

  env$POP_HHSC_CCL <- pop.hhsc_ccl(new = env$DF_HHSC_CCL, old = DF_HHSC_CCL)

  env$POP_HHSC_CCL_ATTR <- pop.hhsc_ccl_most_recent_attr(new = env$DF_HHSC_CCL,
                                                         old = DF_HHSC_CCL)

  env$DF_ACF <- process.acf(cls =
                              list(raw_pth = raw_pth,
                                   acf_qtr_years = acf_qtr_years)
  )

  env$DF_DEMAND <- process.acs(acs_year = acs_year,
                               acs_state_code = state_code,
                               acs_geography = acs_geography,
                               acs_county = acs_county,
                               raw_pth = raw_pth)

  env$XWALK_TRACTS <- process.tracts_xwalk(cls = list(raw_pth = raw_pth))

  env$XWALK_ZIP_COUNTY <- dwnld.xwalk_zip_county(state_fips = state_code)

  env$GEO_ZIP <- dwnld.geo_zip(state_fips = state_code)

  env$GEO_TRACTS <- dwnld.geo_tracts(state_fips = state_code)

  env$LU_COUNTY_CODE <- dwnld.lu_county_code(state_fips = state_code)

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
save_subset_child_care_db <- function(pth, county, tract_radius) {

  check_type.character(county)

  assertthat::assert_that(all(nchar(county) == 5),
                          msg = "Please enter a string 5-digit FIPS code")

  if(file.exists(pth)) {

    load_env(file.path(pth))

    env <- new.env()

    env$XWALK_TRACTS <- XWALK_TRACTS %>%
      dplyr::filter(anchor_county %in% county) %>%
      dplyr::filter(mi_to_tract <= tract_radius)

    env$GEO_TRACTS <- GEO_TRACTS %>%
      dplyr::filter(county_code %in% county)

    env$DF_DEMAND <- DF_DEMAND %>%
      dplyr::filter(county_code %in% county)

    env$XWALK_TRACT_PRVDR <- process.xwalk_tract_prvdr(xwalk_tracts = env$XWALK_TRACTS,
                                                       df_hhsc_ccl = DF_HHSC_CCL)

    surround_tracts <- env$XWALK_TRACTS %>% 
      dplyr::distinct(surround_tract) %>% 
      dplyr::pull(surround_tract)

    env$DF_HHSC_CCL <- DF_HHSC_CCL %>%
      dplyr::filter(tract %in% surround_tracts)

    save(env, file = file.path(dirname(pth), paste(paste(county, collapse = "_"), 
                                                   basename(pth), sep = "_")))

  } else {
    assertthat::assert_that(FALSE, 
                           msg = "Please run child_care_db() function to create
                                  the universe of child care data before subsetting.")
  }
}
