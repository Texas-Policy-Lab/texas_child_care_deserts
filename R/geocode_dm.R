#' @title Pulls down bounding box parameters for Texas
#' @export
tx_bounding_box <- function(url = "https://gist.githubusercontent.com/a8dx/2340f9527af64f8ef8439366de981168/raw/81d876daea10eab5c2675811c39bcd18a79a9212/US_State_Bounding_Boxes.csv",
                            state_fips) {

  bb <- readr::read_csv(url) %>%
    dplyr::filter(STATEFP == state_fips)

  return(list(ul = list(lng = bb$xmin, lat = bb$ymax),
              lr = list(lng = bb$xmax, lat = bb$ymin))
  )
}

#' @title Check Texas Bounds
#' @description Checks that non-missing lat and longitudes are within the Texas
#' state boundaries and if they are not it assign an NA
#' @return data.frame
check_tx_bounds <- function(df,
                            bb) {

  df %>%
    dplyr::mutate(lat = as.numeric(lat),
                  long = as.numeric(long),
                  lat = ifelse(lat >= bb$lr$lat & lat <= bb$ul$lat & long >= bb$ul$lng & long <= bb$lr$lng, lat, NA),
                  long = ifelse(long >= bb$ul$lng & long <= bb$lr$lng & lat >= bb$lr$lat & lat <= bb$ul$lat, long, NA)
                  )
}

#' @title Pulls down bounding box parameters for each county in Texas
#' @export
county_bounding_box <- function(url = "https://raw.githubusercontent.com/stucka/us-county-bounding-boxes/master/bounding.csv",
                            state_fips) {

  county_bb <- readr::read_csv(url) %>% 
    dplyr::filter(statefips == state_fips) %>% 
    dplyr::mutate(county_code = paste0(statefips, countyfips)) %>% 
    dplyr::select(county_code,
                  max_lat = extentn,
                  min_lat = extents,
                  max_long = extente,
                  min_long = extentw)

  assertthat::assert_that(is.numeric(county_bb$max_lat))
  assertthat::assert_that(is.numeric(county_bb$min_lat))
  assertthat::assert_that(is.numeric(county_bb$max_long))
  assertthat::assert_that(is.numeric(county_bb$min_long))
  assertthat::assert_that(all(nchar(df$county_code) == 5))

  return(county_bb)
}

#' @title Check county bounds
#' @description Checks that non-missing lat and longitudes are within their designated county boundaries
#' and if they are not it assign an NA
#' @return data.frame
check_county_bounds <- function(df,
                                county_bb) {
  
  assertthat::assert_that(as.numeric(df$lat))
  assertthat::assert_that(as.numeric(df$long))
  
  df %>% 
    dplyr::left_join(county_bb) %>% 
    dplyr::mutate(lat = ifelse(lat >= min_lat & lat <= max_lat & long >= min_long & long <= max_long, lat, NA),
                  long = ifelse(long >= min_long & long <= max_long & lat >= min_lat & lat <= max_lat, long, NA),
                  tract = ifelse(long >= min_long & long <= max_long & lat >= min_lat & lat <= max_lat, tract, NA)) %>% 
    dplyr::select(-c(min_lat, max_lat, min_long, max_long))
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

  l <- lapply(1:length(results), function(x) {

    row <- results[[x]]$locations[[1]]

    data.frame(
               street = row$street,
               neighborhood = row$adminArea6,
               city  = row$adminArea5,
               county = row$adminArea4,
               state = row$adminArea3,
               zip = row$postalCode,
               lat = row$latLng$lat,
               long = row$latLng$lng,
               geocodeQualityCode = row$geocodeQualityCode,
               mapURl= row$mapUrl,
               stringsAsFactors = FALSE
    )
    }) %>% 
    dplyr::bind_rows() %>%
    dplyr::bind_cols(address = call)

}

#' @title Drops poor quality geocodes
#' @description Use the geocodeQualityCode value returned to determine the quality of the geocode. https://developer.mapquest.com/documentation/geocoding-api/quality-codes/.
#' @export
dm.drop_poor_quality <- function(df,
                                 qualityCode = "A1|A3|A4") {

  poorQuality <- stringr::str_starts(string = df$geocodeQualityCode, 
                                     pattern = qualityCode)

  df %>%
    dplyr::mutate(lat = ifelse(poorQuality, NA, lat),
                  long = ifelse(poorQuality, NA, long))
}

#' @title Geocode addresses
#' @description Geocodes addresses using the Mapquest API
#' @param addresses vector. The list of addresses to geocode.
#' @param key string. The api key registered with your personal Mapquest account.
#' @export
dm.geocode_address <- function(df,
                               bb,
                               version = "v1",
                               url = "http://www.mapquestapi.com",
                               path = "/geocoding/v1/batch",
                               limit = 100) {

  subset <- df %>%
    dplyr::filter(is.na(lat) | is.na(long)) %>%
    dplyr::select(operation_number, address)

  calls <- split_calls(v = subset$address,
                       limit = limit)

  key <- get_key.mapquest()
  url <- httr::modify_url(url = url, path = path)

  l <- lapply(calls, function(call, url, key) {

    r <- httr::POST(url = url,
                    query = list(key = key),
                    body = list(locations = call,
                                boundingBox = bb,
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

  }, url = url, key = key) %>%
    dplyr::bind_rows() %>%
    dplyr::bind_cols(subset %>%
                        dplyr::select(operation_number)) %>%
    dm.drop_poor_quality() %>%
    dplyr::mutate(lat2 = lat,
                  long2 = long) %>% 
    dplyr::select(operation_number, lat2, long2)

  df %>%
    dplyr::left_join(l) %>% 
    dplyr::mutate(lat = ifelse(is.na(lat), lat2, lat),
                  long = ifelse(is.na(long), long2, long)) %>% 
    dplyr::select(-c(lat2, long2))
}

#' @title Unlist FCC request
fcc_request <- function(result) {

  data.frame(tract2 = result$Block$FIPS,
             county_code2 = result$County$FIPS,
             stringsAsFactors = FALSE)
}

#' @title Geocode addresses
#' @description Geocodes addresses using FCC
dm.reverse_geocode <- function(df,
                               url = "https://geo.fcc.gov",
                               path = "/api/census/block/find") {

  url <- httr::modify_url(url = url, path = path)

  subset <- df %>%
    dplyr::filter(is.na(tract))

  df %>%
    dplyr::left_join(
      lapply(1:nrow(subset), function(i) {

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
    ) %>%
    dplyr::mutate(tract2 = ifelse(county_code != county_code2, NA, tract2),
                  tract2 = substr(tract2, 1, 11),
                  tract = ifelse(is.na(tract), tract2, tract)) %>% 
    dplyr::select(-c(tract2, county_code2))
}

dm.geocode_lat_long <- function(df,
                                url = "https://geocoding.geo.census.gov",
                                path = "/geocoder/{returntype}/{searchtype}?{query}",
                                searchtype = "coordinates",
                                returntype = "geographies",
                                benchmark = "Public_AR_Current",
                                vintage = "Census2010_Current",
                                query = "benchmark={benchmark}&vintage={vintage}&x={x}&y={y}") {

  subset <- df %>%
    dplyr::filter(is.na(tract))
  
  df %>% 
    dplyr::left_join(
      lapply(1:nrow(subset), function(i) {
        
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
    )
}
