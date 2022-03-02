#' @title Subset the tract crosswalk crosswalk
subset_tracts <- function(xwalk_tracts,
                          adj_tracts,
                          tract_radius,
                          county_fips) {

  test_config(x = tract_radius, str = "tract_radius")
  
  xwalk_tracts %>%
    dplyr::filter(mi_to_tract <= tract_radius) %>%
    dplyr::select(-mi_to_tract) %>%
    dplyr::bind_rows(adj_tracts) %>%
    dplyr::filter(anchor_county %in% county_fips) %>%
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
                            surround_tracts,
                            lt_age = NULL) {

  if (!is.null(df_prek)) {

    if (lt_age == 4){

      df_prek <- df_prek %>% 
        dplyr::select(-prek_3_4_enrollment) %>% 
        dplyr::filter(!is.na(prek_3_enrollment)) %>% 
        dplyr::rename(prek_enrollment = prek_3_enrollment)
    } else {
      
      df_prek <- df_prek %>% 
        dplyr::select(-prek_3_enrollment) %>% 
        dplyr::rename(prek_enrollment = prek_3_4_enrollment)
    }
    
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
                    quality = campus_rating %in% c("A", "B", "C", "D", "F"),
                    quality_desc = dplyr::case_when(quality ~ "TEA Accountability Rating"),
                    sub_trs_provider = quality,
                    sub_trs4_provider = campus_rating %in% c("A", "B"),
                    download_date = NA) %>% 
      dplyr::bind_rows(df_hhsc_ccl %>% 
                         dplyr::mutate(phone_number = as.character(phone_number)))
  } else {
    df <- df_hhsc_ccl %>% 
      dplyr::mutate(prek_prvdr = FALSE,
                    campus_rating = NA)
  }

  df <- df %>%
    dplyr::mutate(prek_prvdr = ifelse(is.na(prek_prvdr), FALSE, prek_prvdr),
                  campus_rating = ifelse(is.na(campus_rating), NA, campus_rating)) %>%
    dplyr::filter(!is.na(tract)) %>%
    dplyr::filter(tract %in% surround_tracts) %>% 
    dplyr::filter(head_start | home_prvdr | center_prvdr | prek_prvdr) %>% 
    dplyr::filter(!after_school_school_age_only)

  prvdr_type <- df %>%
    dplyr::select(operation_number, head_start, home_prvdr, center_prvdr, 
                  prek_prvdr) %>%
    tidyr::pivot_longer(names_to = "prvdr_type", values_to = "prvdr_type_values",
                        -c(operation_number)) %>% 
    dplyr::filter(prvdr_type_values) %>%
    dplyr::mutate(prvdr_type_desc = dplyr::case_when(grepl("head_start", prvdr_type) ~ "Head Start",
                                                     grepl("home_prvdr", prvdr_type) ~ "Home",
                                                     grepl("center_prvdr", prvdr_type) ~ "Center",
                                                     grepl("prek_prvdr", prvdr_type) ~ "Pre-K")) %>%
    dplyr::group_by(operation_number) %>%
    dplyr::summarise(prvdr_type_desc = paste(prvdr_type_desc, collapse = ", "))

  df <- df %>%
    dplyr::left_join(prvdr_type) %>% 
    dplyr::mutate(prvdr_type_desc = as.factor(prvdr_type_desc),
                  subsidy_desc = ifelse(sub_provider, "Yes", "No"),
                  trs_desc = ifelse(trs_provider, "Yes", "No"),
                  subsidy_trs_desc = dplyr::case_when(prek_prvdr ~ "Pre-K",
                                                      !subsidy_provider & !prek_prvdr ~ "Non-subsidy",
                                                      subsidy_provider & !trs_provider ~ "Subsidy, non-TRS",
                                                      trs_provider ~ paste("TRS ", trs_star_level)),
                  prvdr_size_desc = dplyr::case_when(home_prvdr & licensedhome_prvdr ~ "Licensed Home",
                                                     home_prvdr & registeredhome_prvdr ~ "Registered Home",
                                                     center_prvdr & licensed_capacity <= 50 ~ "Small Center (0-50)",
                                                     center_prvdr & licensed_capacity > 50 & licensed_capacity <= 99 ~ "Medium Center (51-99)",
                                                     center_prvdr & licensed_capacity > 99 ~ "Large Center (100+)")) %>%
    dplyr::left_join(tigris::fips_codes %>%
                       dplyr::mutate(county_code = paste(state_code, county_code, sep = "")) %>%
                       dplyr::select(county_code, county))
}

#' @title Test config
#' @description Test to make sure configuration is set-up correctly
test_config <- function(x,
                        str,
                        msg = "Parameter '{str}' is missing from {n} county in the list") {
  assertthat::assert_that(all(!is.na(x)),
                          msg = glue::glue(msg, str = str, n = sum(is.na(x))))
}

#' @title Test config pct
#' @description Test percent parameters are between 0 and 1
test_config_pct <- function(x,
                            str,
                            msg = "Parameter '{str}' should be between 0 and 1") {

  assertthat::assert_that(all(x <= 1) & all(x >= 0),
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
