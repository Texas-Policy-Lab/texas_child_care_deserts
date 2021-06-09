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
get_geo <- function(lu_code, county, key, value) {

  f <- function(name) {

    fips <- class(name)
    
    osmdata::getbb(name$name) %>%
      osmdata::opq() %>%
      osmdata::add_osm_feature(key = key, value = c(value)) %>%
      osmdata::osmdata_sf() %>% 
      purrr::pluck("osm_lines") %>% 
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
get_geo.highway <- function(lu_code, county, key = "highway", value = "motorway") {
  get_geo(lu_code = lu_code, county = county, key = key, value = value)
}

#' @title Get Geo waterways
get_geo.waterway <- function(lu_code, county, key = "waterway", value = "river") {
  get_geo(lu_code = lu_code, county = county, key = key, value = value)
}
