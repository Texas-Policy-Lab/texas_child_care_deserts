#' @title Find percent of markets that are a desert for a specific type of desert
#' @export
pct_desert <- function(df,
                       type) {
  
  df %>% 
    dplyr::filter(desert_type %in% type) %>% 
    dplyr::group_by(desert) %>% 
    dplyr::summarise(n = dplyr::n()) %>% 
    dplyr::mutate(pct = round((n/sum(n))*100,1)) %>% 
    dplyr::filter(desert == T) %>% 
    dplyr::pull(pct)
}

#' @title Find total number of children in need of care of each type
#' @export
total_children <- function(df) {
  
  df %>% 
    dplyr::group_by(desert) %>% 
    dplyr::summarise(n_kids = round(sum(tract_demand, na.rm = T)))
}

#' @title Find total number of seats of each type
#' @export
total_seats <- function(df){
  
  df %>% 
    dplyr::group_by(desert) %>% 
    dplyr::summarise(n_seats = round(sum(tract_supply, na.rm = T)))
}

#' @title Find average number of children per market
#' @export
avg_children_mkt <- function(df) {
  
  df %>% 
    dplyr::group_by(desert_type) %>% 
    dplyr::summarise(n_kids = round(mean(mkt_demand, na.rm = T)))
}

#' @title Find average number of seats per market
#' @export
avg_seats_mkt <- function(df){
  
  df %>% 
    dplyr::group_by(desert_type) %>% 
    dplyr::summarise(n_seats = round(mean(mkt_supply, na.rm = T)))
}

#' @title Find average number of providers per market
#' @export
avg_provider_mkt <- function(xwalk_tract_provider) {

  xwalk_tract_provider %>%
    dplyr::group_by(anchor_tract) %>% 
    dplyr::summarise(n_provider = dplyr::n_distinct(operation_number))
}

#' @title Find number of children per desert type
#' @export
total_children_desert <- function(df_ratio,
                                  df_demand) {
  
  df_ratio %>% 
    dplyr::filter(desert) %>% 
    dplyr::select(anchor_tract, desert_type) %>% 
    dplyr::left_join(XWALK_TRACTS) %>% 
    dplyr::distinct(surround_tract, desert_type) %>% 
    dplyr::inner_join(df_demand, by = c("surround_tract" = "tract",
                                        "desert_type" = "desert"))%>% 
    dplyr::group_by(desert_type) %>% 
    dplyr::summarise(n_kids = round(sum(tract_demand, na.rm = T)))
}

