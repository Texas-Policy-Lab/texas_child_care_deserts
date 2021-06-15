#' @title Subset the tract crosswalk crosswalk
subset_tracts <- function(xwalk_tracts,
                          adj_tracts,
                          config) {

  test_config(config = config, str = "tract_radius")

  xwalk_tracts %>%
    dplyr::inner_join(config, by = c("anchor_county" = "county_code")) %>%
    dplyr::filter(mi_to_tract <= tract_radius) %>% 
    dplyr::select(-mi_to_tract) %>% 
    dplyr::bind_rows(adj_tracts %>%
                       dplyr::inner_join(config, by = c("anchor_county" = "county_code"))) %>% 
    dplyr::distinct()
}

#' @title Subset surround tracts
subset_surround_tracts <- function(xwalk_tracts) {
  xwalk_tracts %>% 
    dplyr::distinct(surround_tract) %>% 
    dplyr::pull(surround_tract)
}

#' @title Subset HHSC
subset_hhsc_ccl <- function(df_hhsc_ccl,
                            df_prek = NULL,
                            surround_tracts) {

  if (!is.null(df_prek)) {
    df <- df_prek %>% 
      dplyr::rename(licensed_capacity = prek_enrollment,
                    operation_number = campus_id,
                    operation_name = campus_name) %>%
      dplyr::mutate(infant = FALSE,
                    toddler = FALSE,
                    prek = TRUE,
                    school = FALSE,
                    home_prvdr = FALSE,
                    center_prvdr = FALSE,                   
                    prek_prvdr = TRUE,
                    head_start = FALSE,
                    after_school = FALSE,
                    after_school_only = FALSE,
                    school_age_only = FALSE,
                    after_school_school_age_only = FALSE,
                    subsidy = FALSE,
                    trs_provider = FALSE,
                    subsidy_provider = FALSE,
                    trs_star_level = NA,
                    naeyc = FALSE,
                    all_provider = TRUE,
                    sub_provider = TRUE,
                    sub_trs_provider = FALSE,
                    sub_trs4_rovider = FALSE,
                    quality_desc = NA,
                    quality = FALSE,
                    download_date = NA) %>% 
      dplyr::bind_rows(df_hhsc_ccl %>% 
                         dplyr::mutate(phone_number = as.character(phone_number)))
  } else {
    df <- df_hhsc_ccl %>% 
      dplyr::mutate(prek_prvdr = FALSE)
  }
  df %>%
    dplyr::filter(tract %in% surround_tracts)
} 

#' @title Test config
#' @description Test to make sure configuration is set-up correctly
test_config <- function(config,
                        str,
                        msg = "Parameter '{str}' is missing from {n} county in the list") {
  assertthat::assert_that(all(!is.na(config[[str]])),
                          msg = glue::glue(msg, str = str, n = sum(is.na(config[[str]]))))
}

#' @title Test config pct
#' @description Test percent parameters are between 0 and 1
test_config_pct <- function(config,
                            str,
                            msg = "Parameter '{str}' should be between 0 and 1") {

  assertthat::assert_that(all(config[[str]] <= 1) & all(config[[str]] >= 0),
                          msg = glue::glue(msg, str = str))
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
dm.agg_ratio_mkt <- function(n_kids, grouping_vars) {
  
  n_kids %>%
    dplyr::group_by_at(dplyr::vars(anchor_tract, anchor_county, year, grouping_vars)) %>% 
    dplyr::summarise(max_ratio = sum(max_n_kids)/sum(licensed_capacity),
                     med_ratio = sum(med_n_kids)/sum(licensed_capacity),
                     min_ratio = sum(min_n_kids)/sum(licensed_capacity)) %>% 
    # replace ratios over 1 with 1
    dplyr::mutate_at(dplyr::vars(max_ratio, med_ratio, min_ratio), 
                     list(~ ifelse(.>=1,1,.)))
}

#' @title Create subsidy capacity estimate
#' @param config list. Vector of county FIPS codes with names attributes
#' @param tract_radius numeric. A number indicating the tract radius in miles.
#' @param xwalk_tracts data.frame. 
#' @param adj_tracts data.frame.
#' @param df_hhsc_ccl data.frame.
#' @param df_acf data.frame.
#' @param grouping_vars string. The results to be grouped by. Default is NULL.
#' @param qtrs vector of strings. Default is c("1","2","4").
#' @export
calc.subsidy_capacity <- function(config,
                                  xwalk_tracts,
                                  adj_tracts,
                                  df_hhsc_ccl,
                                  df_acf,
                                  grouping_vars = NULL,
                                  qtrs = c("1","2","4")) {

  config <- config %>%
    dplyr::bind_rows(.id = "county_code")

  xwalk_tracts <- subset_tracts(xwalk_tracts = xwalk_tracts,
                                adj_tracts = adj_tracts,
                                config = config)

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
                                   dplyr::inner_join(df_hhsc_ccl),
                                 grouping_vars = grouping_vars)

  m1_param <- mkt_ratios %>%
    tidyr::pivot_longer(-c(anchor_tract, anchor_county, year, grouping_vars)) %>% 
    dplyr::group_by_at(dplyr::vars(anchor_county, year, grouping_vars)) %>%
    dplyr::summarise(m1 = mean(value))

  tri_params <- mkt_ratios %>%
    dplyr::group_by_at(dplyr::vars(anchor_county, year, grouping_vars)) %>%
    dplyr::summarise(a=mean(min_ratio),
                     b=mean(max_ratio)) %>% 
    dplyr::inner_join(m1_param) %>% 
    dplyr::mutate(c=3*m1 - a - b) %>%
    dplyr::select(dplyr::one_of("anchor_county", "year", "b", grouping_vars)) %>% 
    tidyr::pivot_wider(names_from = "anchor_county", values_from = b)

  return(tri_params)
}