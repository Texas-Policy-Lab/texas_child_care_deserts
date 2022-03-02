#' @title Find percent of markets that are a desert for a specific type of desert
#' @export
pct_desert <- function(df) {

  df %>%
    dplyr::group_by(desert_type, desert) %>%
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::mutate(pct = round((n/sum(n))*100,1)) %>%
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = desert_type, values_from = pct) %>%
    tidyr::pivot_longer(names_to = "desert_type", values_to = "pct", cols = -desert) %>%
    dplyr::mutate(pct = ifelse(is.na(pct), 0, pct)) %>%
    tidyr::pivot_wider(names_from = desert_type, values_from = pct) %>%
    dplyr::filter(desert)
}

#' @title Find total number of children in need of care of each type
#' @export
total_children <- function(df) {
  
  df %>%
    dplyr::group_by(desert) %>%
    dplyr::summarise(n_kids = round(sum(tract_demand, na.rm = T))) %>%
    tidyr::pivot_wider(names_from = desert, values_from = n_kids)
}

#' @title Find total number of seats of each type
#' @export
total_seats <- function(df){

  df %>%
    dplyr::group_by(desert) %>%
    dplyr::summarise(n_seats = round(sum(tract_supply, na.rm = T))) %>%
    tidyr::pivot_wider(names_from = desert, values_from = n_seats)
}

#' @title Find average number of children per market
#' @export
avg_children_mkt <- function(df) {

  df %>%
    dplyr::group_by(desert_type) %>%
    dplyr::summarise(n_kids = round(mean(mkt_demand, na.rm = T))) %>%
    tidyr::pivot_wider(names_from = desert_type, values_from = n_kids)
}

#' @title Find average number of seats per market
#' @export
avg_seats_mkt <- function(df){

  df %>%
    dplyr::group_by(desert_type) %>%
    dplyr::summarise(n_seats = round(mean(mkt_supply, na.rm = T))) %>%
    tidyr::pivot_wider(names_from = desert_type, values_from = n_seats)
}

#' @title Find average number of providers per market
#' @export
avg_provider_mkt <- function(xwalk_tract_provider) {

  xwalk_tract_provider %>%
    dplyr::group_by(anchor_tract) %>%
    dplyr::summarise(n_provider = dplyr::n_distinct(operation_number)) %>%
    dplyr::summarise(n_prvdr = mean(n_provider))
}

#' @title Find number of children per desert type
#' @export
total_children_desert <- function(df_ratio,
                                  df_demand) {

  df_ratio %>%
    dplyr::filter(desert) %>%
    dplyr::select(anchor_tract, desert_type) %>%
    dplyr::inner_join(df_demand, by = c("anchor_tract" = "tract",
                                        "desert_type" = "desert")) %>%
    dplyr::group_by(desert_type) %>%
    dplyr::summarise(n_kids = round(sum(tract_demand, na.rm = T))) %>%
    tidyr::pivot_wider(names_from = desert_type, values_from = n_kids)
}

#' @title Percent of deserts each provider serves in
#' @export
create_pct_dsrt_prvdr <- function(mkt_ratio,
                                  df_supply,
                                  xwalk_tracts) {
  df <- df_supply %>%
    dplyr::select(-c(adj_capacity, county_code)) %>%
    dplyr::inner_join(xwalk_tracts, by = c("tract" = "surround_tract")) 

  desert_ttl <- df %>%
    dplyr::inner_join(mkt_ratio %>%
                        dplyr::filter(desert) %>%
                        dplyr::select(-desert) %>%
                        dplyr::rename(desert = desert_type)) %>% 
    dplyr::group_by(operation_number, desert) %>% 
    dplyr::summarise(n_desert = dplyr::n_distinct(anchor_tract))

  mkt_ttl <- df %>%
    dplyr::group_by(operation_number, desert) %>% 
    dplyr::summarise(n_mkt = dplyr::n_distinct(anchor_tract))

  desert_ttl %>%
    dplyr::right_join(mkt_ttl) %>%
    dplyr::mutate(n_desert = ifelse(is.na(n_desert), 0, n_desert),
                  pct_desert = round(n_desert/n_mkt*100, 1)) %>%
    dplyr::select(-n_desert)
}

#' @title Find high need table for each county
#' @export
high_need_table <- function(county_fips,
                            neighborhood_attrs,
                            zip_attrs,
                            sub_labels,
                            quality_labels,
                            demand_cutoff,
                            svi_cutoff){

  if (county_fips == "48201") {
    df <-  neighborhood_attrs %>%
      dplyr::rename(unit_analysis = neighborhood,
                    demand = neighborhood_demand)
  } else {
    df <- zip_attrs %>%
      dplyr::rename(unit_analysis = zip,
                    demand = zip_demand)
  }
  
  high_need <- df %>% 
    dplyr::select(-tract) %>% 
    dplyr::mutate(high_need = dplyr::case_when(desert_type == "all_provider" & label %in% sub_labels & 
                                                 demand > demand_cutoff & med_svi > svi_cutoff ~ TRUE,
                                               desert_type == "sub_provider" & label %in% sub_labels & 
                                                 demand > demand_cutoff & med_svi > svi_cutoff ~ TRUE,
                                               desert_type == "sub_trs_provider" & label %in% quality_labels & 
                                                 demand > demand_cutoff & med_svi > svi_cutoff ~ TRUE,
                                               desert_type == "sub_trs4_provider" & label %in% quality_labels & 
                                                 demand > demand_cutoff & med_svi > svi_cutoff ~ TRUE)) %>% 
    dplyr::filter(high_need)
  
  return(high_need)
  
}

#' @title Find providers serving high need areas
#' @export
prvdrs_serving_high_need <- function(county_fips,
                                     high_need,
                                     xwalk_neighborhood_tract,
                                     xwalk_zip_tract,
                                     df_supply,
                                     providers){
  browser()
  if (county_fips == "48201") {
    df <-  xwalk_neighborhood_tract %>%
      dplyr::rename(unit_analysis = neighborhood)
  } else {
    df <- xwalk_zip_tract %>%
      dplyr::rename(unit_analysis = zip)
  }
  
  df <- df %>% 
    dplyr::filter(unit_analysis %in% high_need$unit_analysis)
  
  providers_in_high_need_nbrhd <- df_supply %>% 
    dplyr::left_join(df) %>% 
    dplyr::inner_join(high_need, by = c("unit_analysis", "desert" = "desert_type")) %>% 
    dplyr::left_join(providers)
  
  return(providers_in_high_need_nbrhd)
  
}