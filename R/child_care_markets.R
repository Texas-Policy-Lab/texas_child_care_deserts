#' @title Drop the bottom 1 percent
drop_bottom_1pct <- function(df) {
  
  df <- df %>%
    dplyr::group_by(dsrt_type) %>% 
    dplyr::mutate(mkt_value = ifelse(mkt_value <= quantile(mkt_value, .01), 
                                     NA, mkt_value))
  
  assertthat::assert_that(any(is.na(df$mkt_value)))
  
  return(df)
}

#' @title Create Market demand
create_market_demand <- function(demand, tracts) {

  tracts %>%
    dplyr::inner_join(demand, by = c("surround_tract" = "tract")) %>% 
    dplyr::group_by(anchor_county, anchor_tract) %>% 
    dplyr::summarise(all_provider = sum(n_kids_working_parents_lt5, na.rm = T),
                     sub_provider = sum(n_kids_lt5_working_under200_pct, na.rm = T))
}

#' @title Create market supply
create_market_supply <- function(supply, tracts) {
  
}
