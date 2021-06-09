#' @title Create County Name
county_name <- function(cnty, lu_code) {

  structure(
    list(name = lu_code %>%
           dplyr::filter(county_code %in% cnty) %>% 
           dplyr::pull(county) %>% 
           paste("United States")),
    class = cnty)
}

#' @title Get geo landmark
get_geo <- function(lu_code, county, key, value, geo_type) {

  f <- function(name) {

    fips <- class(name)
    
    osmdata::getbb(name$name) %>%
      osmdata::opq() %>%
      osmdata::add_osm_feature(key = key, value = c(value)) %>%
      osmdata::osmdata_sf() %>% 
      purrr::pluck(geo_type) %>% 
      dplyr::mutate(county_code = fips)
  }

  name <- lapply(county, county_name, lu_code = lu_code)

  if (length(name) > 1) {
    z <- lapply(name, f) %>% dplyr::bind_rows()
  } else {
    z <- f(name)
  }
}

#' @title Get Geo highways
get_geo.highway <- function(lu_code, county, key = "highway", value = "motorway",
                            geo_type = "osm_lines") {
  get_geo(lu_code = lu_code, county = county, key = key, value = value, 
          geo_type = geo_type) %>%
    dplyr::select(name, geometry, county_code)
}

#' @title Get Geo waterways
get_geo.waterway <- function(lu_code, county, key = "waterway", value = "river",
                             geo_type = "osm_lines") {
  get_geo(lu_code = lu_code, county = county, key = key, value = value, 
          geo_type = geo_type) %>% 
    dplyr::select(geometry, county_code)
}

#' @title Get cities
get_geo.city <- function(lu_code, county, key = "place", value = "city",
                             geo_type = "osm_points") {
  get_geo(lu_code = lu_code, county = county, key = key, value = value,
          geo_type = geo_type) %>%
    dplyr::filter(!is.na(name)) %>% 
    dplyr::select(name, geometry, county_code)
}
