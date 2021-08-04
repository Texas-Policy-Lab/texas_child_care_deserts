#' @title Tarrant County Neighborhoods
dm.tarrant_neighborhood_center <- function(df, ...) {
  
  df %>%
    dplyr::distinct(NPA_NAME, LATITUDE, LONGITUDE) %>% 
    dplyr::group_by(NPA_NAME) %>% 
    dplyr::summarise(mean_lat = mean(LATITUDE),
                     mean_long = mean(LONGITUDE),
                     size = dplyr::n()) %>% 
    dplyr::rename(neighborhood = NPA_NAME) %>% 
    dplyr::mutate(county_code = "48439")
}

#' @title Process the Tarrant Neighborhood data
process.tarrant_neighborhood <- function(cls) {

  cls$df <- do.call(dwnld.tarrant_neighborhood, cls)
  neighborhood_center <- do.call(dm.tarrant_neighborhood_center, cls)
}

#' @title Tarrant County Neighborhoods
dm.harris_neighborhood_center <- function(df, ...) {

 df %>%
    dplyr::mutate(GEOID10 = as.character(GEOID10)) %>%
    dplyr::inner_join(GEO_TRACTS, by = c("GEOID10" = "tract")) %>%
    dplyr::group_by(KCTA_NAME) %>%
    dplyr::summarise(mean_long = mean(X),
                     mean_lat = mean(Y),
                     size = dplyr::n()) %>%
    dplyr::rename(neighborhood = KCTA_NAME) %>%
    dplyr::mutate(county_code = "48201")
}

#' @title Process the Tarrant Neighborhood data
process.harris_neighborhood <- function(cls) {

  cls$df <- do.call(dwnld.harris_neighborhood, cls)
  neighborhood_center <- do.call(dm.harris_neighborhood_center, cls)

}

#' @title Process Neighborhood Centers
process.neighborhood_center <- function(cls) {
  
  harris <- process.harris_neighborhood(cls)
  tarrant <- process.tarrant_neighborhood(cls)

  harris %>%
    dplyr::bind_rows(tarrant)
}

#' @title Process Neighborhood Tract Xwalk 
#' @note Harris County only because of data availability
process.xwalk_neighborhood_tract <- function(raw_pth) {
  
  dwnld.harris_neighborhood(raw_pth = raw_pth) %>% 
    dplyr::mutate(tract = as.character(GEOID10)) %>% 
    dplyr::select(tract,
                  neighborhood = KCTA_NAME)
}

#' @title Find neighborhoods which are deserts
#' @export
neighborhood_desert <- function(xwalk_neighborhood_tract,
                                df_ratio) {
  
  xwalk_neighborhood_tract %>% 
    dplyr::inner_join(df_ratio %>% dplyr::select(anchor_tract,
                                                 desert_type,
                                                 label),
                      by = c("tract" = "anchor_tract"))
}

#' @title Find demand for each neighborhood
#' @export
neighborhood_demand <- function(xwalk_neighborhood_tract,
                                tract_demand) {
  
  xwalk_neighborhood_tract %>% 
    dplyr::inner_join(tract_demand) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(neighborhood, desert) %>% 
    dplyr::summarise(neighborhood_demand = round(sum(tract_demand, na.rm = T)))
}

#' @title Find SVI stats for each neighborhood
neighborhood_svi <- function(xwalk_neighborhood_tract,
                             tract_svi) {
  
  xwalk_neighborhood_tract %>% 
    dplyr::inner_join(tract_svi) %>% 
    dplyr::group_by(neighborhood) %>% 
    dplyr::summarise(min_svi = round(min(svi_pctl, na.rm = T), 2),
                     med_svi = round(median(svi_pctl, na.rm = T), 2),
                     max_svi = round(max(svi_pctl, na.rm = T), 2),
                     avg_svi = round(mean(svi_pctl, na.rm = T), 2))
}

#' @title Find demand for each neighborhood
#' @export
neighborhood_attributes <- function(neighborhood_desert,
                                    neighborhood_demand,
                                    neighborhood_svi) {

  neighborhood_desert %>%
    dplyr::left_join(neighborhood_demand, by = c("neighborhood", "desert_type" = "desert")) %>% 
    dplyr::group_by(neighborhood, desert_type) %>% 
    dplyr::slice(which.min(label)) %>% 
    dplyr::left_join(neighborhood_svi) %>%
    dplyr::ungroup()
}
