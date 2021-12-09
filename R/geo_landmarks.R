#' @title Get coords
#' @description Extracts the coordinates from geography variable
#' @return data.frame
get_coords <- function(GEO_TRACTS) {

  coords <- sf::st_coordinates(GEO_TRACTS) %>%
    as.data.frame()

  grouping_var <- tail(names(coords), 1)

  names(coords)[grep(grouping_var, names(coords))] <- "group"

  coords %>%
    dplyr::ungroup() %>%
    dplyr::select(X, Y, group) %>%
    dplyr::inner_join(GEO_TRACTS %>%
                        dplyr::mutate(group = seq(1, dplyr::n(), 1)))
}

#' @title Get geo landmark
get_geo <- function(county_name, key, value, geo_type) {
  Sys.sleep(10)

  osmdata::getbb(county_name) %>%
    osmdata::opq() %>%
    osmdata::add_osm_feature(key = key, value = c(value)) %>%
    osmdata::osmdata_sf() %>% 
    purrr::pluck(geo_type)
}

#' @title Get Geo highways
get_geo.highway <- function(county_name, key = "highway", value = "motorway",
                            geo_type = "osm_lines") {
  get_geo(county_name = county_name, key = key, value = value, 
          geo_type = geo_type) %>%
    dplyr::select(name, geometry)
}

#' @title Get Geo waterways
get_geo.waterway <- function(county_name, key = "waterway", value = "river",
                             geo_type = "osm_lines") {
  get_geo(county_name = county_name, key = key, value = value, 
          geo_type = geo_type) %>% 
    dplyr::select(geometry)
}

#' @title Get Geo park
get_geo.park <- function(county_name, key = "leisure", value = "park",
                         geo_type = "osm_polygons") {
  get_geo(county_name = county_name, key = key, value = value, 
          geo_type = geo_type) %>% 
    dplyr::select(geometry)
}

#' @title Get cities
get_geo.city <- function(county_name, key = "place", value = "city",
                         geo_type = "osm_points") {

  get_geo(county_name = county_name, key = key, value = value,
          geo_type = geo_type) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::select(name, population, geometry) %>% 
    dplyr::mutate(population = as.numeric(population)) %>%
    dplyr::arrange(dplyr::desc(population))
}
