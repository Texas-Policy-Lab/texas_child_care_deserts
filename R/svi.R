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
