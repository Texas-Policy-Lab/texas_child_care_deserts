#' @title Calculate 0-3 capacity estimate
#' @param config list. Vector of county FIPS codes with names attributes
#' @param tract_radius numeric. A number indicating the tract radius in miles.
#' @param xwalk_tracts data.frame. 
#' @param adj_tracts data.frame.
#' @param df_hhsc_ccl data.frame.
#' @param df_acf data.frame.
#' @param grouping_vars string. The results to be grouped by. Default is NULL.
#' @param qtrs vector of strings. Default is c("1","2","4").
#' @export
calc.capacity_adjustment_03 <- function(df_hhsc_ccl,
                                        df_frontline,
                                        grouping_vars = NULL) {
  

  pct_03 <- df_frontline %>%
    dplyr::inner_join(df_hhsc_ccl) %>% 
    dplyr::mutate(pct_03_ofcapacity = seats_03/licensed_capacity)
  
  pct_03_by_group <- pct_03 %>% 
    dplyr::group_by_at(dplyr::vars(anchor_county, grouping_vars)) %>% 
    dplyr::summarise(mean_pct_03 = mean(pct_03_ofcapacity)) 
  
  return(pct_03_by_group)
}