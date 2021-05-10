#' @title Pulls down bounding box parameters for Texas
#' @export
tx_bounding_box <- function(df,
                            url = "https://gist.githubusercontent.com/a8dx/2340f9527af64f8ef8439366de981168/raw/81d876daea10eab5c2675811c39bcd18a79a9212/US_State_Bounding_Boxes.csv",
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
                  lat = ifelse(lat >= bb$lr$lat & lat <= bb$ul$lat, lat, NA),
                  long = ifelse(long >= bb$ul$lng & long <= bb$lr$lng, long, NA)
                  )
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
                               state_fips,
                               version = "v1",
                               url = "http://www.mapquestapi.com",
                               path = "/geocoding/v1/batch",
                               limit = 100) {

  bb <- tx_bounding_box(df = df,
                        state_fips = state_fips)

  subset <- dm.subset_ccl_geocode(df, bb)

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
    dplyr::mutate(lat2 = as.character(lat),
                  long2 = as.character(long)) %>% 
    dplyr::select(operation_number, lat2, long2)

  df %>%
    dplyr::left_join(l) %>% 
    dplyr::mutate(lat = ifelse(is.na(lat), lat2, lat),
                  long = ifelse(is.na(long), long2, long)) %>% 
    dplyr::select(-c(lat2, long2))
}

#' @title Geocode addresses
#' @description Geocodes addresses using the Mapquest API
#' @param latLng list. The list of addresses to geocode.
#' @param key string. The api key registered with your personal Mapquest account.
#' @examples
#'  latLng <- list(lat = 30.333472, lng = -81.470448)
#'  key <- "XXXXX"
#'  address <- dm.reverse_geocode(latLng = latLng, key = key)
#' @export
dm.reverse_geocode <- function(latLng,
                               version = "v1",
                               url = "http://www.mapquestapi.com/geocoding/{version}/reverse?key={key}") {

  key <- get_key.mapquest()

  l <- lapply(latLng, function(x, url, version, key) {
    r <- httr::POST(url = glue::glue(url, version = version, 
                                     key = key),
                    query = list(key = key),
                    body = list(location = list(latLng = x)),
                    encode = "json")

    if(r$status_code == 200) {

      c <- httr::content(r)

      df <- dm.geocode_request(c)

    } else {
      warning("status not 200")
    }

  }, url = url, version = version, key = key)

  l %>% 
    dplyr::bind_rows()
}

#' @title Subset CCL for geocoding
dm.subset_ccl_geocode <- function(df, bb) {

  df %>%
    check_tx_bounds(bb) %>%
    dplyr::filter(is.na(lat) | is.na(long)) %>%
    dplyr::select(operation_number, address)

}
