#' @title Drop the bottom 1 percent
drop_bottom_1pct <- function(df) {
  
  df <- df %>%
    dplyr::group_by(dsrt_type) %>% 
    dplyr::mutate(mkt_value = ifelse(mkt_value <= quantile(mkt_value, .01), 
                                     NA, mkt_value))
  
  assertthat::assert_that(any(is.na(df$mkt_value)))
  
  return(df)
}

#' @title Create tract demand
create_tract_demand <- function(demand) {
  
  demand %>%
    dplyr::select(tract, county_code, n_kids_working_parents_lt5, n_kids_lt5_working_under200_pct) %>%
    dplyr::rename(all_provider = n_kids_working_parents_lt5,
                  sub_provider = n_kids_lt5_working_under200_pct) %>% 
    dplyr::mutate(sub_trs4_provider = sub_provider,
                  sub_trs_provider = sub_provider) %>% 
    tidyr::pivot_longer(names_to = "desert", values_to = "tract_demand", cols = -c(tract, county_code))

}

#' @title Create Market demand
create_market_demand <- function(demand, tracts) {

  tracts %>%
    dplyr::inner_join(demand, by = c("surround_tract" = "tract")) %>% 
    dplyr::group_by(anchor_county, anchor_tract, desert) %>% 
    dplyr::summarise(mkt_demand = sum(tract_demand, na.rm = T)) %>%
    dplyr::ungroup()
}

#' @title Create market supply
create_market_supply <- function(supply, tracts) {
  
}
