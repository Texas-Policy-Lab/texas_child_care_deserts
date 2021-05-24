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
dm.agg_kids_prvdr <- function(df_acf,
                              year,
                              quarters) {
  
  df_acf %>%
    dplyr::filter(year == year & quarter %in% quarters) %>%
    dplyr::group_by(operation_number, quarter_year) %>%
    dplyr::summarise(n_kids = dplyr::n_distinct(child_id)) %>%
    tidyr::pivot_wider(names_from = quarter_year, values_from = n_kids, values_fill = 0)%>%
    tidyr::pivot_longer(names_to = "quarter_year", values_to = "value", -c(operation_number)) %>%
    dplyr::group_by(operation_number) %>%
    dplyr::summarise(max_n_kids = max(value),
                     med_n_kids = median(value),
                     min_n_kids = min(value)) %>% 
    dplyr::ungroup()
}

#' @title Calculate enrollment ratios aggregated to market level
#' @param n_kids data.frame. Result of dm.agg_kids_prvdr.
#' @export
dm.agg_ratio_mkt <- function(n_kids, 
                             df_hhsc_ccl, 
                             xwalk_tract_prvdr){
  
  n_kids %>%
    dplyr::left_join(df_hhsc_ccl %>% 
                       dplyr::select(operation_number, licensed_capacity, subsidy)) %>%
    dplyr::filter(subsidy) %>%
    dplyr::inner_join(xwalk_tract_prvdr) %>%
    dplyr::group_by(anchor_tract) %>% 
    dplyr::summarise(max_ratio = sum(max_n_kids)/sum(licensed_capacity),
                     med_ratio = sum(med_n_kids)/sum(licensed_capacity),
                     min_ratio = sum(min_n_kids)/sum(licensed_capacity)) %>% 
    # replace ratios over 1 with 1
    dplyr::mutate_at(dplyr::vars(max_ratio, med_ratio, min_ratio), 
                     list(~ ifelse(.>=1,1,.)))
}

#' @title Calculate enrollment ratios by market
#' @export
dm.mkt_enrollment_ratio <- function(df_acf,
                                    year,
                                    quarters,
                                    df_hhsc_ccl,
                                    xwalk_tract_prvdr) {
  
  n_kids <- dm.agg_kids_prvdr(df_acf = df_acf,
                              year = year,
                              quarters = quarters)
  
  mkt_ratios <- dm.agg_ratio_mkt(n_kids, 
                                 df_hhsc_ccl, 
                                 xwalk_tract_prvdr)
  
  return(mkt_ratios)

}

#' @title Create subsidy capacity estimate
#' @export
calc.subsidy_capacity <- function(xwalk_tract_prvdr, 
                                  df_hhsc_ccl,
                                  df_acf,
                                  year,
                                  quarters) {
  
  mkt_enrollment_ratios <- dm.mkt_enrollment_ratio(df_acf = df_acf,
                                                   year = year,
                                                   quarters = quarters,
                                                   df_hhsc_ccl = df_hhsc_ccl,
                                                   xwalk_tract_prvdr = xwalk_tract_prvdr)
  
  m1_param <- mkt_enrollment_ratios %>% 
    tidyr::pivot_longer(-anchor_tract) %>% 
    dplyr::summarise(m1 = mean(value))
  
  tri_params <- mkt_enrollment_ratios %>% 
    dplyr::summarise(a=mean(min_ratio),
                     b=mean(max_ratio)) %>% 
    dplyr::bind_cols(m1_param) %>% 
    dplyr::mutate(c=3*m1 - a - b)
  
  return(tri_params$b)
}