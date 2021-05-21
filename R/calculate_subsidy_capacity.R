#' @title Process the Tracts and Provider crosswalk
process.xwalk_tract_prvdr <- function(xwalk_tracts,
                                      df_hhsc_ccl) {

  xwalk_tracts %>%
    dplyr::inner_join(df_hhsc_ccl %>%
                        dplyr::select(operation_number, tract),
                        by = c("surround_tract" = "tract")) %>% 
    dplyr::select(operation_number, anchor_county, anchor_tract)
}

#' @title Aggregate number of kids per provider
#' @param acf data.frame. The cleaned acf dataframe.
#' @return Summarized data with the max, median, and minimum number of kids per
#' provider
#' @export
dm.agg_kids_prvdr <- function(df_acf) {
  
  df_acf %>%
    dplyr::group_by(operation_number, quarter_year) %>%
    dplyr::summarise(n_kids = dplyr::n_distinct(child_id)) %>%
    tidyr::pivot_wider(names_from = quarter_year, values_from = n_kids, values_fill = 0) %>%
    tidyr::pivot_longer(names_to = "quarter_year", values_to = "value", -c(operation_number)) %>%
    dplyr::group_by(operation_number, quarter_year) %>%
    dplyr::summarise(max_n_kids = max(value),
                     med_n_kids = median(value),
                     min_n_kids = min(value)) %>% 
    dplyr::arrange(operation_number, quarter_year)
}

#' @title Create market subsidy
#' @export
calc.mkt_subsidy <- function(xwalk_track_pvrdr, 
                             df_hhsc_ccl,
                             df_acf) {
  
  n_kids <- dm.agg_kids_prvdr(df_acf = df_acf)
  
  mkt_enrollment_ratios <- n_kids %>%
    dplyr::left_join(df_hhsc_ccl %>% 
                       dplyr::select(operation_number, licensed_capacity, subsidy)) %>%
    dplyr::filter(subsidy) %>%
    dplyr::inner_join(xwalk_track_pvrdr) %>%
    dplyr::select(-operation_number) %>% 
    dplyr::summarise(max_ratio = max_n_kids/licensed_capacity,
                     med_ratio = med_n_kids/licensed_capacity,
                     min_ratio = min_n_kids/licensed_capacity) %>% 
    # replace ratios over 1 with 1
    dplyr::mutate_at(dplyr::vars(max_ratio, med_ratio, min_ratio), 
                     list(~ ifelse(.>=1,1,.))) %>% 
    #drop markets that have no providers and therefore na for the ratios
    tidyr::drop_na()
  
  mom_params <- mkt_enrollment_ratios %>% 
    tidyr::pivot_longer(-anchor_tract) %>% 
    dplyr::group_by(anchor_tract) %>% 
    #raw moments
    dplyr::summarise(mu_1=mean(value)) %>% 
    # aggregate moments across markets
    dplyr::summarise(mu_hat_1 = mean(mu_1))
  
  tri_params <- mkt_enrollment_ratios %>% 
    dplyr::summarise(a=mean(min_ratio),
                     b=mean(max_ratio)) %>% 
    dplyr::bind_cols(mom_params) %>% 
    dplyr::mutate(c=3*mu_hat_1 - a - b)
  
  return(tri_params$b)
}