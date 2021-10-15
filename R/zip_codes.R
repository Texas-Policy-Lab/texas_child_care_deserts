#' @title Download tract to zip xwalk
#' @description data downloaded from
#' https://www.huduser.gov/portal/datasets/usps_crosswalk.html
dwnld.zip <- function(raw_pth, 
                      name = "TRACT_ZIP_062021.csv") {
  
  df <- readr::read_csv(file.path(raw_pth, name))
}

#' @title Process tract to zip xwalk
process.zip <- function(raw_pth) {
  
  dwnld.zip(raw_pth = raw_pth) %>% 
    dplyr::filter(usps_zip_pref_state == "TX") %>% 
    dplyr::mutate(zip = as.character(zip)) %>% 
    dplyr::select(tract, 
                  zip)
  
}

#' @title Find zip codes which are deserts
#' @export
zip_desert <- function(xwalk_zip_tract,
                       df_ratio) {
  browser()
  xwalk_zip_tract %>% 
    dplyr::inner_join(df_ratio %>% dplyr::select(anchor_tract,
                                                 desert_type,
                                                 label),
                      by = c("tract" = "anchor_tract"))
}

#' @title Find demand for each zip code
#' @export
zip_demand <- function(xwalk_zip_tract,
                       tract_demand) {
  browser()
  xwalk_zip_tract %>% 
    dplyr::inner_join(tract_demand) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(zip, desert) %>% 
    dplyr::summarise(zip_demand = round(sum(tract_demand, na.rm = T)))
}

#' @title Find SVI stats for each zip code
zip_svi <- function(xwalk_zip_tract,
                    tract_svi) {
  browser()
  xwalk_zip_tract %>% 
    dplyr::inner_join(tract_svi) %>% 
    dplyr::group_by(zip) %>% 
    dplyr::summarise(min_svi = round(min(svi_pctl, na.rm = T), 2),
                     med_svi = round(median(svi_pctl, na.rm = T), 2),
                     max_svi = round(max(svi_pctl, na.rm = T), 2),
                     avg_svi = round(mean(svi_pctl, na.rm = T), 2))
}

#' @title Find attributes for each zip code
#' @export
zip_attributes <- function(zip_desert,
                           zip_demand,
                           zip_svi) {
  
  zip_desert %>%
    dplyr::left_join(zip_demand, by = c("zip", "desert_type" = "desert")) %>% 
    dplyr::group_by(zip, desert_type) %>% 
    dplyr::slice(which.min(label)) %>% 
    dplyr::left_join(zip_svi) %>%
    dplyr::ungroup()
}
