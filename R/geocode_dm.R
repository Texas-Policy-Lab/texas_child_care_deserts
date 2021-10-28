#' @title Pulls down bounding box parameters for Texas
#' @export
tx_bounding_box <- function(x) {

  bb <- readr::read_csv(x$bb_url) %>%
    dplyr::filter(STATEFP == x$state_fips)

  x$bb <- list(ul = list(lng = bb$xmin, lat = bb$ymax),
       lr = list(lng = bb$xmax, lat = bb$ymin))

  return(x)
}

#' @title Check Texas Bounds
#' @description Checks that non-missing lat and longitudes are within the Texas
#' state boundaries and if they are not it assign an NA
#' @return data.frame
check_tx_bounds <- function(x) {

  x$df <- x$df %>%
    dplyr::mutate(lat = ifelse(lat >= x$bb$lr$lat & lat <= x$bb$ul$lat & 
                                 long >= x$bb$ul$lng & long <= x$bb$lr$lng, lat, NA),
                  long = ifelse(long >= x$bb$ul$lng & long <= x$bb$lr$lng & 
                                  lat >= x$bb$lr$lat & lat <= x$bb$ul$lat, long, NA))
  return(x)
}

#' @title Split calls
#' @description Splits list into multiple groups for batch calls with limits per call
#' @param v vector. Vector to split into multiple groups
#' @param limit integer. The max size of each group.
#' @export
split_calls <- function(v, limit) {

  if (length(v) > limit) {
    calls <- split(v, ceiling(seq_along(v)/limit))
  } else {
    calls <- list(v)
  }

  return(calls)
}

#' @title Response -> Dataframe
#' @description Turns the response from mapquest into a dataframe
#' @export
dm.geocode_request <- function(results, call) {

  lapply(1:length(results), function(x) {

    row <- results[[x]]$locations[[1]]

    data.frame(lat2 = row$latLng$lat,
               long2 = row$latLng$lng,
               geocodeQualityCode = row$geocodeQualityCode,
               stringsAsFactors = FALSE
               )
    }) %>% 
    dplyr::bind_rows()
}

#' @title Geocode addresses
#' @description Geocodes addresses using the Mapquest API
#' @param addresses vector. The list of addresses to geocode.
#' @param key string. The api key registered with your personal Mapquest account.
#' @export
dm.geocode_address <- function(x) {

  subset <- x$df %>%
    dplyr::filter(is.na(lat) | is.na(long)) %>%
    dplyr::select(operation_number, address)

  calls <- split_calls(v = subset$address,
                       limit = x$geocode$limit)

  url <- httr::modify_url(url = x$geocode$url, path = x$geocode$path)

  geo <- lapply(calls, function(call, url, key) {

    r <- httr::POST(url = url,
                    query = list(key = key),
                    body = list(locations = call,
                                boundingBox = x$bb,
                                maxResults = 1,
                                outFormat ="json"),
                    encode = "json")

    if (r$status_code == 200) {

      if (httr::http_type(r) != "application/json") {
        stop("API did not return json", call. = FALSE)
      }

      dm.geocode_request(results = httr::content(r)$results,
                         call)
    } else {
      warning("status not 200")
    }

  }, url = url, key = get_key.mapquest()) %>%
    dplyr::bind_rows()
  
  geo <- geo %>%
    dplyr::mutate(poorQuality = stringr::str_starts(string = geocodeQualityCode, 
                                                     pattern = x$geocode$qualityCode),
                  lat2 = ifelse(poorQuality, NA, lat2),
                  long2 = ifelse(poorQuality, NA, long2)) %>%
    dplyr::bind_cols(subset %>%
                        dplyr::select(operation_number)) 

  x$df <- x$df %>%
    dplyr::left_join(geo) %>% 
    dplyr::mutate(lat = ifelse(is.na(lat), lat2, lat),
                  long = ifelse(is.na(long), long2, long)) %>% 
    dplyr::select(-c(lat2, long2, geocodeQualityCode, poorQuality))

  return(x)
}

#' @title Unlist FCC request
fcc_request <- function(result) {

  data.frame(tract2 = result$Block$FIPS,
             county_code2 = result$County$FIPS,
             stringsAsFactors = FALSE)
}

#' @title Geocode addresses
#' @description Geocodes addresses using FCC
dm.reverse_geocode <- function(x) {

  url <- httr::modify_url(url = x$reverse_geocode$url, path = x$reverse_geocode$path)

  subset <- x$df %>%
    dplyr::filter(is.na(tract))

  geo <- lapply(1:nrow(subset), function(i) {
      
      r <- httr::GET(url = url,
                     query = list(latitude=subset$lat[i],
                                  longitude=subset$long[i],
                                  showall="true",
                                  format="json"),
                     encode = "json")
      
      resp <- httr::content(r)
      
      if (httr::http_type(r) != "application/json") {
        stop("API did not return json", call. = FALSE)
      }
      
      fcc_request(resp) %>% 
        dplyr::mutate(operation_number = subset$operation_number[i])
    }) %>% dplyr::bind_rows()

  x$df <- x$df %>%
    dplyr::left_join(geo) %>%
    dplyr::mutate(tract2 = ifelse(county_code != county_code2, NA, tract2),
                  tract2 = substr(tract2, 1, 11),
                  tract = ifelse(is.na(tract), tract2, tract)) %>% 
    dplyr::select(-c(tract2, county_code2))
  
  return(x)
}

dm.geocode_lat_long <- function(df,
                                url = "https://geocoding.geo.census.gov",
                                path = "/geocoder/{returntype}/{searchtype}?{query}",
                                searchtype = "coordinates",
                                returntype = "geographies",
                                benchmark = "Public_AR_Current",
                                vintage = "Census2010_Current",
                                query = "benchmark={benchmark}&vintage={vintage}&x={x}&y={y}") {

  lapply(1:nrow(df), function(i) {

    x <- df %>%
      dplyr::slice(i) %>% 
      dplyr::pull(long)
    
    y <- df %>% 
      dplyr::slice(i) %>% 
      dplyr::pull(lat)
    
    r <- httr::GET(httr::modify_url(url = url, 
                                    path = glue::glue(path,
                                                      searchtype = searchtype,
                                                      returntype = returntype,
                                                      query = glue::glue(query, 
                                                                         benchmark = benchmark, 
                                                                         vintage = vintage,
                                                                         x = x, 
                                                                         y = y)))) %>% 
      httr::content()

    df %>%
      dplyr::slice(i) %>%
      dplyr::mutate(tract = r$result$geographies$`Census Tracts`[[1]]$GEOID)

  }) %>% dplyr::bind_rows()
}
