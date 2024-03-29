#' @title Create Supply
create_supply <- function(df_hhsc_ccl,
                          supply_adjustment_sub = NULL,
                          supply_adjustment_03 = NULL) {
  browser()
  df <- df_hhsc_ccl %>%
    dplyr::left_join(supply_adjustment_sub) %>% 
    dplyr::mutate(adj_sub_capacity = licensed_capacity * desired_pct_sub_capacity)
  
  if (!is.null(supply_adjustment_03)) {
    df <- df %>% 
      dplyr::left_join(supply_adjustment_03) %>% 
      dplyr::mutate(adj_all_capacity = licensed_capacity * desired_pct_capacity)
    
  } else {
    df <- df %>% 
      dplyr::mutate(adj_all_capacity = licensed_capacity * .85)
  }
  
  df <- df %>%  
    dplyr::mutate(adj_all_capacity = ifelse(prek_prvdr, licensed_capacity, adj_all_capacity),
                              adj_sub_capacity = ifelse(prek_prvdr, licensed_capacity, adj_sub_capacity)) %>% 
    dplyr::select(operation_number, tract, county_code, adj_all_capacity, adj_sub_capacity,
                  all_provider, sub_provider, sub_trs_provider, sub_trs4_provider) %>%
    tidyr::pivot_longer(names_to = "desert", values_to = "supply", 
                        cols = -c(operation_number, tract, county_code,
                                  adj_all_capacity, adj_sub_capacity)) %>%     
    dplyr::filter(supply) %>%
    dplyr::select(-supply) %>% 
    dplyr::mutate(adj_capacity = dplyr::case_when(desert == "all_provider" ~ adj_all_capacity,
                                                  desert != "all_provider" ~ adj_sub_capacity)) %>% 
    dplyr::select(-c(adj_all_capacity, adj_sub_capacity))
}

#' @title Create tract supply
create_tract_supply <- function(supply) {

   supply %>%
    dplyr::group_by(tract, county_code, desert) %>%
    dplyr::summarise(tract_supply = sum(adj_capacity, na.rm = TRUE))
}

#' @title Create market supply
create_market_supply <- function(tract_supply, tracts, xwalk_tract_desert) {

  tracts %>%
    dplyr::left_join(tract_supply, by = c("surround_tract" = "tract")) %>%
    dplyr::group_by(anchor_county, anchor_tract, desert) %>%
    dplyr::summarise(mkt_supply = sum(tract_supply, na.rm = T)) %>%
    dplyr::ungroup() %>% 
    dplyr::right_join(xwalk_tract_desert) %>% 
    dplyr::mutate(mkt_supply = ifelse(is.na(mkt_supply), 0, mkt_supply),
                  anchor_county = ifelse(is.na(anchor_county), substr(anchor_tract, 1, 5), anchor_county))
}

#' @title Create tract demand
create_tract_demand <- function(demand,
                                lt_age = 5) {
  if (lt_age == 4){
    n_kids_working_parents <- "n_kids_working_parents_lt4"
    n_kids_working_under200_pct <- "n_kids_lt4_working_under200_pct"
  } else if (lt_age == 5) {
    n_kids_working_parents <- "n_kids_working_parents_lt5"
    n_kids_working_under200_pct <- "n_kids_lt5_working_under200_pct"
  }

  demand %>%
    dplyr::select(tract, county_code, n_kids_working_parents, 
                  n_kids_working_under200_pct) %>%
    dplyr::rename(all_provider = n_kids_working_parents,
                  sub_provider = n_kids_working_under200_pct) %>%
    dplyr::mutate(sub_trs4_provider = sub_provider,
                  sub_trs_provider = sub_provider) %>%
    tidyr::pivot_longer(names_to = "desert", values_to = "tract_demand", 
                        cols = -c(tract, county_code))
}

#' @title Drop the bottom 1 percent
drop_bottom_1pct <- function(df) {
  
  df <- df %>%
    dplyr::group_by(desert) %>% 
    dplyr::mutate(mkt_demand = ifelse(mkt_demand <= quantile(mkt_demand, .01), 
                                      NA, mkt_demand))
  
  assertthat::assert_that(any(is.na(df$mkt_demand)))
  
  return(df)
}

#' @title Create Market demand
create_market_demand <- function(tract_demand, tracts, xwalk_tract_desert) {

  tracts %>%
    dplyr::inner_join(tract_demand, by = c("surround_tract" = "tract")) %>% 
    dplyr::group_by(anchor_county, anchor_tract, desert) %>% 
    dplyr::summarise(mkt_demand = sum(tract_demand, na.rm = T)) %>%
    dplyr::ungroup() %>% 
    dplyr::right_join(xwalk_tract_desert) %>% 
    dplyr::mutate(mkt_demand = ifelse(is.na(mkt_demand), 0, mkt_demand)) %>% 
    drop_bottom_1pct()
}

#' @title Create market ratio
create_market_ratio <- function(mkt_supply, mkt_demand) {

  mkt_supply %>%
    dplyr::full_join(mkt_demand) %>%
    dplyr::mutate(seats_per_100 = (mkt_supply/mkt_demand)*100,
                  desert_type = desert,
                  desert = ifelse(seats_per_100 < 33, TRUE, FALSE),
                  label = dplyr:::case_when(seats_per_100 < 5 ~ "< 5 seats",
                                            seats_per_100 >= 5 & seats_per_100 < 15 ~ ">= 5 and < 15",
                                            seats_per_100 >= 15 & seats_per_100 < 25 ~ ">= 15 and < 25",
                                            seats_per_100 >= 25 & seats_per_100 < 33 ~ ">= 25 and < 33",
                                            seats_per_100 >= 33 ~ "Not a desert",
                                            is.na(seats_per_100) ~ "Not enough children to estimate")
    ) %>%
    dplyr::mutate(
                  label = ordered(label,
                                  levels = c("< 5 seats", ">= 5 and < 15", ">= 15 and < 25", ">= 25 and < 33", "Not a desert",
                                             "Not enough children to estimate")),
                  desert_desc = dplyr::case_when(!desert ~ "Not a desert",
                                                  desert ~ "Desert")
    )
}

#' @title Tract desert crosswalk
xwalk_tract_desert <- function(tracts) {

  expand.grid(unique(tracts$anchor_tract), 
              c("all_provider", "sub_provider", "sub_trs_provider", "sub_trs4_provider")) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    dplyr::rename(anchor_tract = Var1,
                  desert = Var2)
}
