#' @title Create subsidy capacity estimate
#' @param df_hhsc_ccl data.frame.
#' @param df_frontline data.frame.
#' @param grouping_vars string. The results to be grouped by. Default is NULL.
#' @export
calc.capacity_adjustment_sub <- function(df_hhsc_ccl,
                                         df_frontline,
                                         df_supply_adjustment_03 = NULL,
                                         grouping_vars = NULL) {
  
  if(is.null(df_supply_adjustment_03)){
    supply_adjustment <- df_hhsc_ccl %>% 
      dplyr::select(operation_number,
                    desired_pct_capacity = 1)
  }

  df <- df_frontline %>% 
    dplyr::inner_join(df_hhsc_ccl) %>% 
    dplyr::left_join(supply_adjustment) %>% 
    dplyr::filter(sub_provider) %>% 
    dplyr::mutate(med_param = (n_subsidy_kids/enrollment_total)*desired_pct_capacity,
                  med_param = ifelse(!is.finite(med_param), NA, med_param),
                  optimistic_param =  ((n_subsidy_kids + seats_total)/licensed_capacity)*desired_pct_capacity,
                  pessimistic_param = (n_subsidy_kids/licensed_capacity)*desired_pct_capacity)
  
  df_expectation <- df %>% 
    tidyr::pivot_longer(cols = ends_with("param")) %>% 
    dplyr::group_by_at(dplyr::vars(operation_number, grouping_vars)) %>% 
    dplyr::summarise(expectation = mean(value, na.rm = T))
  
  subsidy_expectation_by_groups <- df_expectation %>% 
    dplyr::group_by_at(dplyr::vars(grouping_vars)) %>% 
    dplyr::summarise(desired_pct_sub_capacity = mean(expectation))

  return(subsidy_expectation_by_groups)
}
