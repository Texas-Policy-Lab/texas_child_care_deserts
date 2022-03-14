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
                          naeyc_pth1,
                          naeyc_pth2,
                          state_code = 48,
                          acf_qtr_years = NULL,
                          acs_year = 2019,
                          acs_geography = "tract",
                          acs_county = NULL,
                          db_name = "child_care_env.Rdata") {

  data_pth <- file.path(root, "data")
  raw_pth <- file.path(data_pth, "raw")
  processed_pth <- file.path(data_pth, "processed")
  naeyc_pth1 <- file.path(raw_pth, naeyc_pth1)
  naeyc_pth2 <- file.path(raw_pth, naeyc_pth2)
  pths <- c(data_pth, raw_pth, processed_pth)
  create_folder_str(pths = pths)

  load_env(file.path(processed_pth, db_name))

  env <- new.env()

  env$NEIGHBORHOOD_CENTER <- process.neighborhood_center(cls = list(raw_pth = raw_pth))

  env$DF_TWC <- process.twc(raw_pth = raw_pth,
                            state_fips = state_code)

  env$DF_HHSC_CCL <- process.hhsc_ccl(cls = list(raw_pth = raw_pth,
                                                 processed_pth = processed_pth,
                                                 df_twc = env$DF_TWC,
                                                 naeyc_pth1 = naeyc_pth1,
                                                 naeyc_pth2 = naeyc_pth2,
                                                 name = "HHSC_CCL",
                                                 state_fips = state_code))

  env$POP_HHSC_CCL <- pop.hhsc_ccl(new = env$DF_HHSC_CCL, old = DF_HHSC_CCL)

  env$POP_HHSC_CCL_ATTR <- pop.hhsc_ccl_most_recent_attr(new = env$DF_HHSC_CCL,
                                                         old = DF_HHSC_CCL)
  
  env$DF_FRONTLINE <- process.frontline(raw_pth = raw_pth)

  env$DF_ACF <- process.acf(cls =
                              list(raw_pth = raw_pth,
                                   acf_qtr_years = acf_qtr_years)
  )

  env$DF_DEMAND <- process.acs(acs_year = acs_year,
                               acs_state_code = state_code,
                               acs_geography = acs_geography,
                               acs_county = acs_county,
                               raw_pth = raw_pth)

  env$DF_PREK <- process.prek(raw_pth = raw_pth)
  
  env$XWALK_TRACTS <- process.tracts_xwalk(cls = list(raw_pth = raw_pth))

  env$ADJ_TRACTS <- process.adj_tracts(cls = list(raw_pth = raw_pth))

  env$GEO_TRACTS <- dwnld.geo_tracts(state_fips = state_code)

  env$LU_COUNTY_CODE <- dwnld.lu_county_code(state_fips = state_code)

  env$XWALK_NEIGHBORHOOD_TRACT <- process.xwalk_neighborhood_tract(raw_pth = raw_pth)
  
  env$XWALK_ZIP_TRACT <- process.zip(raw_pth = raw_pth)
  
  env$DF_TRACT_SVI <- process.svi(raw_pth = raw_pth)

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
#' @export
save_subset_child_care_db <- function(pth, config) {

  if(file.exists(pth)) {

    load_env(file.path(pth))

    env <- sapply(names(config), function(county_fips) {

      l <- list()

      config <- config[[county_fips]]

      l$COUNTY_FIPS <- county_fips

      l$COUNTY_NAME <- LU_COUNTY_CODE %>% 
        dplyr::filter(county_code %in% county_fips) %>% 
        dplyr::pull(county)

      l$DF_NEIGHBORHOOD_CENTER <- NEIGHBORHOOD_CENTER %>% 
        dplyr::filter(county_code == county_fips)

      l$XWALK_NEIGHBORHOOD_TRACT <- XWALK_NEIGHBORHOOD_TRACT

      l$XWALK_TRACTS <- subset_tracts(xwalk_tracts = XWALK_TRACTS,
                                      adj_tracts = ADJ_TRACTS ,
                                      tract_radius = config$tract_radius,
                                      county_fips = county_fips)
      
      l$XWALK_NEIGHBORHOOD_TRACT <- XWALK_NEIGHBORHOOD_TRACT %>% 
        dplyr::filter(tract %in% l$XWALK_TRACTS$anchor_tract)
      
      l$XWALK_TRACT_DESERT <- xwalk_tract_desert(tracts = l$XWALK_TRACTS)

      l$SURROUND_TRACTS <- subset_surround_tracts(xwalk_tracts = l$XWALK_TRACTS)

      l$ANCHOR_TRACTS <- l$XWALK_TRACTS %>% 
        dplyr::distinct(anchor_tract) %>% 
        dplyr::pull(anchor_tract)
      
      l$SURROUND_COUNTY <- l$XWALK_TRACTS %>% 
        dplyr::distinct(surround_county) %>% 
        dplyr::pull(surround_county)

      l$GEO_TRACTS <- GEO_TRACTS %>%
        dplyr::filter(tract %in% l$SURROUND_TRACTS) %>%
        dplyr::mutate(anchor_county = grepl(l$COUNTY_FIPS, tract)) %>%
        dplyr::select(tract, county_code, anchor_county, geometry)

      l$BB_TRACTS <- sapply(l$ANCHOR_TRACTS, function(t) {

        BB <- l$GEO_TRACTS %>% 
          dplyr::filter(tract == t) %>%
          sf::st_bbox()

        data.frame(tract = t,
                   xmin = BB[[1]],
                   ymin = BB[[2]],
                   xmax = BB[[3]],
                   ymax = BB[[4]])
      }, USE.NAMES = T, simplify = F) %>% dplyr::bind_rows()

      l$LU_COUNTY_CODE <- LU_COUNTY_CODE %>% 
        dplyr::filter(county_code %in% l$SURROUND_COUNTY)
      
      l$GEO_WATERWAY <- get_geo.waterway(county_name = l$COUNTY_NAME)
      
      l$GEO_HIGHWAY <- get_geo.highway(county_name = l$COUNTY_NAME)
      
      l$GEO_CITY <- get_geo.city(county_name = l$COUNTY_NAME)
      
      l$GEO_PARK <- get_geo.park(county_name = l$COUNTY_NAME)
      
      l$DF_TRACT_DEMAND <- create_tract_demand(demand = DF_DEMAND %>%
                                                   dplyr::filter(tract %in% l$SURROUND_TRACTS))

      l$DF_MKT_DEMAND <- create_market_demand(tract_demand = l$DF_TRACT_DEMAND, 
                                              tracts = l$XWALK_TRACTS,
                                              xwalk_tract_desert = l$XWALK_TRACT_DESERT)
  
      l$XWALK_TRACT_PRVDR <- process.xwalk_tract_prvdr(xwalk_tracts = l$XWALK_TRACTS,
                                                       df_hhsc_ccl = DF_HHSC_CCL)

      l$DF_HHSC_CCL <- subset_hhsc_ccl(df_hhsc_ccl = DF_HHSC_CCL,
                                       df_prek = DF_PREK,
                                       surround_tracts = l$SURROUND_TRACTS)

      l$DF_SUPPLY <- create_supply(df_hhsc_ccl = l$DF_HHSC_CCL,
                                   config = config)

      l$DF_TRACT_SUPPLY <- create_tract_supply(supply = l$DF_SUPPLY)

      l$DF_MKT_SUPPLY <- create_market_supply(tract_supply = l$DF_TRACT_SUPPLY,
                                              tracts = l$XWALK_TRACTS,
                                              xwalk_tract_desert = l$XWALK_TRACT_DESERT)

      l$DF_MKT_RATIO <- create_market_ratio(mkt_supply = l$DF_MKT_SUPPLY,
                                            mkt_demand = l$DF_MKT_DEMAND)

      l$AVG_CHILD_MKT <- avg_children_mkt(l$DF_MKT_RATIO)
      l$AVG_SEATS_MKT <- avg_seats_mkt(l$DF_MKT_RATIO)
      l$AVG_PRVDR_MKT <- avg_provider_mkt(l$XWALK_TRACT_PRVDR)

      l$TTL_CHILD <- total_children(l$DF_TRACT_DEMAND)
      l$TTL_SEATS <- total_seats(l$DF_TRACT_SUPPLY)
      l$TTL_CHILD_DSRT <- total_children_desert(df_ratio = l$DF_MKT_RATIO,
                                                df_demand = l$DF_TRACT_DEMAND)

      l$PCT_DESERT <- pct_desert(df = l$DF_MKT_RATIO)

      l$NEIGHBORHOOD_DESERT <- neighborhood_desert(xwalk_neighborhood_tract = XWALK_NEIGHBORHOOD_TRACT,
                                                   df_ratio = l$DF_MKT_RATIO)

      l$NEIGHBORHOOD_DEMAND <- neighborhood_demand(xwalk_neighborhood_tract = XWALK_NEIGHBORHOOD_TRACT,
                                                   tract_demand = l$DF_TRACT_DEMAND)
      
      l$NEIGHBORHOOD_SVI <- neighborhood_svi(xwalk_neighborhood_tract = XWALK_NEIGHBORHOOD_TRACT,
                                             tract_svi = DF_TRACT_SVI %>% dplyr::filter(county_code == county_fips))
      
      l$NEIGHBORHOOD_ATTRS <- neighborhood_attributes(neighborhood_desert = l$NEIGHBORHOOD_DESERT,
                                                      neighborhood_demand = l$NEIGHBORHOOD_DEMAND,
                                                      neighborhood_svi = l$NEIGHBORHOOD_SVI)
      
      l$PCT_DESERT_PRVDR <- create_pct_dsrt_prvdr(mkt_ratio = l$DF_MKT_RATIO,
                                                  df_supply = l$DF_SUPPLY,
                                                  xwalk_tracts = l$XWALK_TRACTS)

      l$DF_MKT_DEMAND <- l$DF_MKT_DEMAND %>% 
        dplyr::rename(desert_type = desert,
                      value = mkt_demand)

      l$DF_MKT_SUPPLY <- l$DF_MKT_SUPPLY %>% 
        dplyr::rename(desert_type = desert,
                      value = mkt_supply)

      l$DF_TRACT_DEMAND <- l$DF_TRACT_DEMAND %>% 
        dplyr::rename(desert_type = desert,
                      value = tract_demand) %>%
        dplyr::ungroup()

      l$DF_TRACT_SUPPLY <- l$DF_TRACT_SUPPLY %>% 
        dplyr::rename(desert_type = desert,
                      value = tract_supply) %>%
        dplyr::ungroup()

      l$DF_SUPPLY <- l$DF_SUPPLY %>% 
        dplyr::rename(desert_type = desert)

      l$PCT_DESERT_PRVDR <- l$PCT_DESERT_PRVDR %>% 
        dplyr::rename(desert_type = desert)

      return(l)
    }, USE.NAMES = TRUE, simplify = FALSE)

    save(env, file = file.path(dirname(pth), paste(paste(names(config), collapse = "_"), 
                                                   basename(pth), sep = "_")))

  } else {
    assertthat::assert_that(FALSE, 
                           msg = "Please run child_care_db() function to create
                                  the universe of child care data before subsetting.")
  }
}

#' @title Save a subset of the child care database for children 0-3
#' @param pth string. Path to the root directory to create the DB.
#' @param config object. Object containing parameters to create the DB.
#' @param dev boolean. St dev = FALSE to save the db to the production environment. Default is TRUE.
#' @examples
#' \dontrun{
#' pth <- "C:/"
#' county <- "48439"
#' save_subset_child_care_db_03(pth = pth, county = county)
#' }
#' @export
save_subset_child_care_db_03 <- function(pth, config, dev = TRUE) {

  if(file.exists(pth)) {

    load_env(file.path(pth))
    
    env <- sapply(names(config), function(county_fips) {

      l <- list()

      config <- config[[county_fips]]
      
      l$COUNTY_FIPS <- county_fips
      
      l$COUNTY_NAME <- LU_COUNTY_CODE %>% 
        dplyr::filter(county_code %in% county_fips) %>% 
        dplyr::pull(county)
      
      l$DF_NEIGHBORHOOD_CENTER <- NEIGHBORHOOD_CENTER %>% 
        dplyr::filter(county_code == county_fips)
      
      l$XWALK_NEIGHBORHOOD_TRACT <- XWALK_NEIGHBORHOOD_TRACT
      
      l$XWALK_TRACTS <- subset_tracts(xwalk_tracts = XWALK_TRACTS,
                                      adj_tracts = ADJ_TRACTS ,
                                      tract_radius = config$tract_radius,
                                      county_fips = county_fips)
      
      l$XWALK_NEIGHBORHOOD_TRACT <- XWALK_NEIGHBORHOOD_TRACT %>% 
        dplyr::filter(tract %in% l$XWALK_TRACTS$anchor_tract)
      
      l$XWALK_ZIP_TRACT <- XWALK_ZIP_TRACT %>% 
        dplyr::filter(tract %in% l$XWALK_TRACTS$anchor_tract)
      
      l$XWALK_TRACT_DESERT <- xwalk_tract_desert(tracts = l$XWALK_TRACTS)
      
      l$SURROUND_TRACTS <- subset_surround_tracts(xwalk_tracts = l$XWALK_TRACTS)
      
      l$ANCHOR_TRACTS <- l$XWALK_TRACTS %>% 
        dplyr::distinct(anchor_tract) %>% 
        dplyr::pull(anchor_tract)
      
      l$SURROUND_COUNTY <- l$XWALK_TRACTS %>% 
        dplyr::distinct(surround_county) %>% 
        dplyr::pull(surround_county)
      
      l$GEO_TRACTS <- GEO_TRACTS %>%
        dplyr::filter(tract %in% l$SURROUND_TRACTS) %>%
        dplyr::mutate(anchor_county = grepl(l$COUNTY_FIPS, tract)) %>%
        dplyr::select(tract, county_code, anchor_county, geometry, cent_lat, cent_long)

      l$GEO_TRACTS <- rmapshaper::ms_simplify(input = as(l$GEO_TRACTS, 'Spatial')) %>%
        sf::st_as_sf()

      l$BB <- l$GEO_TRACTS %>% 
        sf::st_bbox()

      l$BB_TRACTS <- sapply(l$SURROUND_TRACTS, function(t) {
        
        BB <- l$XWALK_TRACTS %>%
          dplyr::filter(anchor_tract == t) %>%
          dplyr::inner_join(l$GEO_TRACTS %>%
                              dplyr::select(-anchor_county), by = c("surround_tract" = "tract"))
        BB <- sf::st_bbox(BB$geometry)

        data.frame(tract = t,
                   xmin = BB$xmin,
                   ymin = BB$ymin,
                   xmax = BB$xmax,
                   ymax = BB$ymax)
      }, USE.NAMES = T, simplify = F) %>% dplyr::bind_rows()

      l$GEO_TRACTS <- get_coords(l$GEO_TRACTS)

      l$LU_COUNTY_CODE <- LU_COUNTY_CODE %>% 
        dplyr::filter(county_code %in% l$SURROUND_COUNTY)

      l$GEO_WATERWAY <- get_geo.waterway(county_name = l$COUNTY_NAME)

      l$GEO_HIGHWAY <- get_geo.highway(county_name = l$COUNTY_NAME)

      l$GEO_CITY <- get_geo.city(county_name = l$COUNTY_NAME)

      l$GEO_PARK <- get_geo.park(county_name = l$COUNTY_NAME)
      
      l$DF_TRACT_DEMAND <- create_tract_demand(demand = DF_DEMAND %>%
                                                 dplyr::filter(tract %in% l$SURROUND_TRACTS),
                                               lt_age = 4)
      
      l$DF_MKT_DEMAND <- create_market_demand(tract_demand = l$DF_TRACT_DEMAND, 
                                              tracts = l$XWALK_TRACTS,
                                              xwalk_tract_desert = l$XWALK_TRACT_DESERT)
      
      l$XWALK_TRACT_PRVDR <- process.xwalk_tract_prvdr(xwalk_tracts = l$XWALK_TRACTS,
                                                       df_hhsc_ccl = DF_HHSC_CCL)

      l$DF_HHSC_CCL <- subset_hhsc_ccl(df_hhsc_ccl = DF_HHSC_CCL,
                                       df_prek = DF_PREK,
                                       surround_tracts = l$SURROUND_TRACTS,
                                       lt_age = 4) 

      l$DF_HHSC_CCL <- l$DF_HHSC_CCL %>% 
        dplyr::filter(lat > l$BB[[2]] & lat < l$BB[[4]] & long > l$BB[[1]] & long < l$BB[[3]])

      l$SUPPLY_ADJUSTMENT_03 <- calc.capacity_adjustment_03(df_hhsc_ccl = l$DF_HHSC_CCL,
                                                            df_frontline = DF_FRONTLINE,
                                                            grouping_vars = c("sub_provider", "sub_trs_provider", "center_prvdr", "prvdr_size_desc"))
      
      l$SUPPLY_ADJUSTMENT_SUB <- calc.capacity_adjustment_sub(df_hhsc_ccl = l$DF_HHSC_CCL,
                                                              df_frontline = DF_FRONTLINE,
                                                              df_supply_adjustment_03 = l$SUPPLY_ADJUSTMENT_03,
                                                              grouping_vars = c("sub_provider", "sub_trs_provider", "center_prvdr", "prvdr_size_desc"))
      
      l$DF_SUPPLY <- create_supply(df_hhsc_ccl = l$DF_HHSC_CCL,
                                   supply_adjustment_sub = l$SUPPLY_ADJUSTMENT_SUB,
                                   supply_adjustment_03 = l$SUPPLY_ADJUSTMENT_03)
      
      l$DF_TRACT_SUPPLY <- create_tract_supply(supply = l$DF_SUPPLY)
      
      l$DF_MKT_SUPPLY <- create_market_supply(tract_supply = l$DF_TRACT_SUPPLY,
                                              tracts = l$XWALK_TRACTS,
                                              xwalk_tract_desert = l$XWALK_TRACT_DESERT)
      
      l$DF_MKT_RATIO <- create_market_ratio(mkt_supply = l$DF_MKT_SUPPLY,
                                            mkt_demand = l$DF_MKT_DEMAND)
      
      l$AVG_CHILD_MKT <- avg_children_mkt(l$DF_MKT_RATIO)
      l$AVG_SEATS_MKT <- avg_seats_mkt(l$DF_MKT_RATIO)
      l$AVG_PRVDR_MKT <- avg_provider_mkt(l$XWALK_TRACT_PRVDR)
      
      l$TTL_CHILD <- total_children(l$DF_TRACT_DEMAND)
      l$TTL_SEATS <- total_seats(l$DF_TRACT_SUPPLY)
      l$TTL_CHILD_DSRT <- total_children_desert(df_ratio = l$DF_MKT_RATIO,
                                                df_demand = l$DF_TRACT_DEMAND)
      
      l$PCT_DESERT <- pct_desert(df = l$DF_MKT_RATIO)
      
      l$NEIGHBORHOOD_DESERT <- neighborhood_desert(xwalk_neighborhood_tract = XWALK_NEIGHBORHOOD_TRACT,
                                                   df_ratio = l$DF_MKT_RATIO)
      
      l$ZIP_DESERT <- zip_desert(xwalk_zip_tract = XWALK_ZIP_TRACT,
                                 df_ratio = l$DF_MKT_RATIO)
      
      l$NEIGHBORHOOD_DEMAND <- neighborhood_demand(xwalk_neighborhood_tract = XWALK_NEIGHBORHOOD_TRACT,
                                                   tract_demand = l$DF_TRACT_DEMAND)
      
      l$ZIP_DEMAND <- zip_demand(xwalk_zip_tract = XWALK_ZIP_TRACT,
                                 tract_demand = l$DF_TRACT_DEMAND)
      
      l$TRACT_SVI <- DF_TRACT_SVI %>% 
        dplyr::filter(county_code == county_fips)
      
      l$NEIGHBORHOOD_SVI <- neighborhood_svi(xwalk_neighborhood_tract = XWALK_NEIGHBORHOOD_TRACT,
                                             tract_svi = DF_TRACT_SVI %>% dplyr::filter(county_code == county_fips))
      
      l$ZIP_SVI <- zip_svi(xwalk_zip_tract = XWALK_ZIP_TRACT,
                           tract_svi = DF_TRACT_SVI %>% dplyr::filter(county_code == county_fips))
      
      l$NEIGHBORHOOD_ATTRS <- neighborhood_attributes(neighborhood_desert = l$NEIGHBORHOOD_DESERT,
                                                      neighborhood_demand = l$NEIGHBORHOOD_DEMAND,
                                                      neighborhood_svi = l$NEIGHBORHOOD_SVI)
      
      l$ZIP_ATTRS <- zip_attributes(zip_desert = l$ZIP_DESERT,
                                    zip_demand = l$ZIP_DEMAND,
                                    zip_svi = l$ZIP_SVI)

      l$HIGH_NEED_TABLE <- high_need_table(county_fips = l$COUNTY_FIPS,
                                           neighborhood_attrs = l$NEIGHBORHOOD_ATTRS,
                                           zip_attrs = l$ZIP_ATTRS,
                                           sub_labels = c("< 5 seats", ">= 5 and < 15", ">= 15 and < 25", ">= 25 and < 33"),
                                           quality_labels = c("< 5 seats", ">= 5 and < 15"),
                                           demand_cutoff = 1000,
                                           svi_cutoff = .8)
      
      l$PRVDRS_SERVING_HIGH_NEED <- prvdrs_serving_high_need(county_fips = l$COUNTY_FIPS,
                                                             high_need = l$HIGH_NEED_TABLE,
                                                             xwalk_neighborhood_tract = l$XWALK_NEIGHBORHOOD_TRACT,
                                                             xwalk_zip_tract = l$XWALK_ZIP_TRACT,
                                                             df_supply = l$DF_SUPPLY,
                                                             providers = l$DF_HHSC_CCL)
      
      l$PCT_DESERT_PRVDR <- create_pct_dsrt_prvdr(mkt_ratio = l$DF_MKT_RATIO,
                                                  df_supply = l$DF_SUPPLY,
                                                  xwalk_tracts = l$XWALK_TRACTS)
      
      l$DF_MKT_DEMAND <- l$DF_MKT_DEMAND %>% 
        dplyr::rename(desert_type = desert,
                      value = mkt_demand)
      
      l$DF_MKT_SUPPLY <- l$DF_MKT_SUPPLY %>% 
        dplyr::rename(desert_type = desert,
                      value = mkt_supply)
      
      l$DF_TRACT_DEMAND <- l$DF_TRACT_DEMAND %>% 
        dplyr::rename(desert_type = desert,
                      value = tract_demand) %>%
        dplyr::ungroup()
      
      l$DF_TRACT_SUPPLY <- l$DF_TRACT_SUPPLY %>% 
        dplyr::rename(desert_type = desert,
                      value = tract_supply) %>%
        dplyr::ungroup()
      
      l$DF_SUPPLY <- l$DF_SUPPLY %>% 
        dplyr::rename(desert_type = desert)
      
      l$PCT_DESERT_PRVDR <- l$PCT_DESERT_PRVDR %>% 
        dplyr::rename(desert_type = desert)
      
      return(l)
    }, USE.NAMES = TRUE, simplify = FALSE)
    
    if (dev) {
      d <- "development"
    } else {
      d <- "production"
    }
    
    save(env, file = file.path(dirname(pth), d, paste("03",
                                                   paste(names(config), collapse = "_"), 
                                                   basename(pth), sep = "_")))
    
  } else {
    assertthat::assert_that(FALSE, 
                            msg = "Please run child_care_db() function to create
                                  the universe of child care data before subsetting.")
  }
}
