#' @title Subset the tract crosswalk crosswalk
subset_tracts <- function(xwalk_tracts,
                          county,
                          tract_radius) {

  xwalk_tracts %>%
    dplyr::filter(anchor_county %in% county) %>%
    dplyr::filter(mi_to_tract <= tract_radius)
}

#' @title Subset surround tracts
subset_surround_tracts <- function(xwalk_tracts) {
  xwalk_tracts %>% 
    dplyr::distinct(surround_tract) %>% 
    dplyr::pull(surround_tract)
}

#' @title Subset HHSC
subset_hhsc_ccl <- function(df_hhsc_ccl,
                            surround_tracts) {
  df_hhsc_ccl %>%
    dplyr::filter(tract %in% surround_tracts)
} 

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
    dplyr::group_by(operation_number, anchor_county, anchor_tract, quarter_year) %>%
    dplyr::summarise(n_kids = dplyr::n_distinct(child_id)) %>%
    tidyr::pivot_wider(names_from = quarter_year, values_from = n_kids, values_fill = 0) %>%
    tidyr::pivot_longer(names_to = "quarter_year", values_to = "value", -c(operation_number, anchor_county, anchor_tract)) %>% 
    dplyr::mutate(year = gsub(".*-", "", quarter_year)) %>%
    dplyr::group_by(operation_number, anchor_county, anchor_tract, year) %>%
    dplyr::summarise(max_n_kids = max(value),
                     med_n_kids = median(value),
                     min_n_kids = min(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(max_n_kids > 0)
}

#' @title Calculate enrollment ratios aggregated to market level
#' @param n_kids data.frame. Result of dm.agg_kids_prvdr.
#' @export
dm.agg_ratio_mkt <- function(n_kids) {

  n_kids %>%
    dplyr::group_by(anchor_tract, anchor_county, year, center_prvdr) %>% 
    dplyr::summarise(max_ratio = sum(max_n_kids)/sum(licensed_capacity),
                     med_ratio = sum(med_n_kids)/sum(licensed_capacity),
                     min_ratio = sum(min_n_kids)/sum(licensed_capacity)) %>% 
    # replace ratios over 1 with 1
    dplyr::mutate_at(dplyr::vars(max_ratio, med_ratio, min_ratio), 
                     list(~ ifelse(.>=1,1,.)))
}

#' @title Create subsidy capacity estimate
#' @export
calc.subsidy_capacity <- function(county,
                                  tract_radius,
                                  xwalk_tracts, 
                                  df_hhsc_ccl,
                                  df_acf,
                                  qtrs) {

  xwalk_tracts <- subset_tracts(xwalk_tracts = xwalk_tracts,
                                county = county,
                                tract_radius = tract_radius)

  surround_tracts <- subset_surround_tracts(xwalk_tracts = xwalk_tracts)

  df_hhsc_ccl <- subset_hhsc_ccl(df_hhsc_ccl = df_hhsc_ccl,
                                 surround_tracts = surround_tracts) %>% 
    dplyr::filter(subsidy)

  xwalk_tract_provider <- process.xwalk_tract_prvdr(xwalk_tracts = xwalk_tracts,
                                                    df_hhsc_ccl = df_hhsc_ccl)

  df_acf <- df_acf %>%
    dplyr::filter(operation_number %in% df_hhsc_ccl$operation_number) %>%
    dplyr::filter(quarter %in% qtrs) %>%
    dplyr::inner_join(xwalk_tract_provider)

  n_kids <- dm.agg_kids_prvdr(df_acf = df_acf)
  
  mkt_ratios <- dm.agg_ratio_mkt(n_kids = n_kids %>% 
                                   dplyr::inner_join(df_hhsc_ccl))

  m1_param <- mkt_ratios %>%
    tidyr::pivot_longer(-c(anchor_tract, anchor_county, year, center_prvdr)) %>% 
    dplyr::group_by(anchor_county, year, center_prvdr) %>%
    dplyr::summarise(m1 = mean(value))

  tri_params <- mkt_ratios %>%
    dplyr::group_by(anchor_county, year, center_prvdr) %>%
    dplyr::summarise(a=mean(min_ratio),
                     b=mean(max_ratio)) %>% 
    dplyr::inner_join(m1_param) %>% 
    dplyr::mutate(c=3*m1 - a - b) %>%
    dplyr::select(anchor_county, year, center_prvdr, b) %>% 
    tidyr::pivot_wider(names_from = "anchor_county", values_from = b)

  return(tri_params)
}