#' @title Get geo landmark
get_geo <- function(county_name, key, value, geo_type) {

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
