#' @title Find percent of markets that are a desert for a specific type of desert
#' @export
pct_desert <- function(df) {

  df %>%
    dplyr::group_by(desert_type, desert) %>%
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::mutate(pct = round((n/sum(n))*100,1)) %>%
    dplyr::filter(desert) %>%
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = desert_type, values_from = pct)
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

#' @title Find neighborhoods which are deserts
#' @export
neighborhood_desert <- function(xwalk_neighborhood_tract,
                                df_ratio) {
  
  xwalk_neighborhood_tract %>% 
    dplyr::inner_join(df_ratio %>% dplyr::select(anchor_tract,
                                                 desert_type,
                                                 label),
                      by = c("tract" = "anchor_tract"))
}
