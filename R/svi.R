#' @title Download SVI
#' @description data downloaded from
#' https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
#' @note Year: 2018
#' Geography: Texas
#' Geography type: Census tracts
#' File type: CSV
dwnld.svi <- function(raw_pth, 
                       name = "Texas.csv") {
  
  df <- readr::read_csv(file.path(raw_pth, name)) %>% 
    dplyr::na_if(-999)
}

#' @title Process SVI data
process.svi <- function(raw_pth) {
  
  dwnld.svi(raw_pth = raw_pth) %>% 
    dplyr::mutate(county_code = as.character(STCNTY),
                  tract = as.character(FIPS)) %>% 
    dplyr::select(county_code,
                  tract,
                  svi_sum = SPL_THEMES,
                  svi_pctl = RPL_THEMES)
  
}

#' @title Find SVI stats for each neighborhood
neighborhood_svi <- function(xwalk_neighborhood_tract,
                             tract_svi) {
  
  xwalk_neighborhood_tract %>% 
    dplyr::inner_join(tract_svi) %>% 
    dplyr::group_by(neighborhood) %>% 
    dplyr::summarise(min_svi = min(svi_pctl, na.rm = T),
                     med_svi = median(svi_pctl, na.rm = T),
                     max_svi = max(svi_pctl, na.rm = T),
                     avg_svi = mean(svi_pctl, na.rm = T))
}
