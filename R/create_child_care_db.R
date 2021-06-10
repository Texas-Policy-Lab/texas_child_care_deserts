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
                          trs_pth,
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

  env$NEIGHBORHOOD_CENTER <- process.neighborhood_center(cls = list(raw_pth = raw_pth))

  env$DF_HHSC_CCL <- process.hhsc_ccl(cls = list(raw_pth = raw_pth,
                                                 processed_pth = processed_pth,
                                                 trs_pth = trs_pth,
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

  env$ADJ_TRACTS <- process.adj_tracts(cls = list(raw_pth = raw_pth))

  env$XWALK_ZIP_COUNTY <- dwnld.xwalk_zip_county(state_fips = state_code)

  env$GEO_ZIP <- dwnld.geo_zip(state_fips = state_code)

  env$GEO_TRACTS <- dwnld.geo_tracts(state_fips = state_code)

  env$GEO_COUNTY <- dwnld.geo_county(state_fips = state_code)

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
save_subset_child_care_db <- function(pth, config) {

  if(file.exists(pth)) {

    config <- config %>%
      dplyr::bind_rows(.id = "county_code")
    
    load_env(file.path(pth))

    env <- new.env()

    env$NEIGHBORHOOD_CENTER <- NEIGHBORHOOD_CENTER

    env$XWALK_TRACTS <- subset_tracts(xwalk_tracts = XWALK_TRACTS,
                                      adj_tracts = ADJ_TRACTS,
                                      config = config)

    env$XWALK_TRACT_DESERT <- xwalk_tract_desert(tracts = env$XWALK_TRACTS)

    surround_tracts <- subset_surround_tracts(xwalk_tracts = env$XWALK_TRACTS)

    surround_county <- env$XWALK_TRACTS %>% 
      dplyr::distinct(surround_county) %>% 
      dplyr::pull(surround_county)

    env$GEO_TRACTS <- GEO_TRACTS %>%
      dplyr::filter(tract %in% surround_tracts) %>% 
      dplyr::mutate(anchor_county = ifelse(county_code %in% names(config), TRUE, FALSE))

    env$GEO_COUNTY <- GEO_COUNTY %>%
      dplyr::filter(county_code %in% surround_county) %>%
      dplyr::mutate(anchor_county = ifelse(county_code %in% names(config), TRUE, FALSE))

    env$DF_TRACT_DEMAND <- create_tract_demand(demand = DF_DEMAND %>%
                                                 dplyr::filter(tract %in% surround_tracts))

    env$DF_MKT_DEMAND <- create_market_demand(tract_demand = env$DF_TRACT_DEMAND, 
                                              tracts = env$XWALK_TRACTS,
                                              xwalk_tract_desert = env$XWALK_TRACT_DESERT)

    env$XWALK_TRACT_PRVDR <- process.xwalk_tract_prvdr(xwalk_tracts = env$XWALK_TRACTS,
                                                       df_hhsc_ccl = DF_HHSC_CCL)

    env$DF_HHSC_CCL <- subset_hhsc_ccl(df_hhsc_ccl = DF_HHSC_CCL,
                                       surround_tracts = surround_tracts)

    env$DF_SUPPLY <- create_supply(df_hhsc_ccl = env$DF_HHSC_CCL,
                                   config = config)

    env$DF_TRACT_SUPPLY <- create_tract_supply(supply = env$DF_SUPPLY)

    env$DF_MKT_SUPPLY <- create_market_supply(tract_supply = env$DF_TRACT_SUPPLY,
                                              tracts = env$XWALK_TRACTS,
                                              xwalk_tract_desert = env$XWALK_TRACT_DESERT)

    env$DF_MKT_RATIO <- create_market_ratio(mkt_supply = env$DF_MKT_SUPPLY,
                                            mkt_demand = env$DF_MKT_DEMAND)
    
    save(env, file = file.path(dirname(pth), paste(paste(names(config), collapse = "_"), 
                                                   basename(pth), sep = "_")))

  } else {
    assertthat::assert_that(FALSE, 
                           msg = "Please run child_care_db() function to create
                                  the universe of child care data before subsetting.")
  }
}
