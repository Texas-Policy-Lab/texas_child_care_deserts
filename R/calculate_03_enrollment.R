#' @title Calculate percent of kids 0-3 enrolled per provider
#' @param acf data.frame. The cleaned acf dataframe.
#' @return Summarized data with the percent 0-3 for each provider
#' @export
dm.pct_enrolled_03 <- function(df_acf,
                               grouping_vars){
  
  n_kids <- df_acf %>% 
    dplyr::group_by_at(dplyr::vars(operation_number, quarter_year, date, anchor_county, grouping_vars)) %>% 
    dplyr::summarise(n_kids_03 = dplyr::n_distinct(child_id[child_age < 4], na.rm = T),
                     n_kids_total = dplyr::n_distinct(child_id, na.rm = T)) %>% 
    dplyr::mutate(pct_enrolled_03 = n_kids_03/n_kids_total)
}


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
calc.capacity_adjustment_03 <- function(config,
                                        xwalk_tracts,
                                        adj_tracts,
                                        df_hhsc_ccl,
                                        df_acf,
                                        grouping_vars = NULL,
                                        yr = "2019",
                                        qtrs = c("4")) {
  
  lapply(names(config), function(county_fips) {

    df_acf <- subset_acf(config,
                         county_fips,
                         xwalk_tracts,
                         adj_tracts,
                         df_hhsc_ccl,
                         df_acf,
                         qtrs,
                         ccl_join = T) %>% 
      dplyr::filter(year == yr) %>% 
      dplyr::mutate(anchor_county = county_fips)
    
    pct_03 <- dm.pct_enrolled_03(df_acf = df_acf,
                                 grouping_vars = grouping_vars)
    
    pct_03_by_group <- pct_03 %>% 
      dplyr::group_by_at(dplyr::vars(anchor_county, grouping_vars)) %>% 
      dplyr::summarise(mean_pct_03 = mean(pct_enrolled_03, na.rm = T)) 
    
    return(pct_03_by_group)
  }) %>% dplyr::bind_rows()
}