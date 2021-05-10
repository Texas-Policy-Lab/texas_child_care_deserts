#' @title Pulls down bounding box parameters for Texas
#' @export
tx_bounding_box <- function(url = "https://gist.githubusercontent.com/a8dx/2340f9527af64f8ef8439366de981168/raw/81d876daea10eab5c2675811c39bcd18a79a9212/US_State_Bounding_Boxes.csv",
                            state_fips) {

  df <- readr::read_csv(url) %>%
    dplyr::filter(STATEFP == state_fips)

  return(list(ul = list(lng = df$xmin, lat = df$ymax),
              lr = list(lng = df$xmax, lat = df$ymin))
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
dm.geocode_request <- function(c) {

  l <- lapply(1:length(c$results), function(x) {

    row <- c$results[[x]]$locations[[1]]

    df <- data.frame(street = row$street,
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
  })

  df <- do.call(rbind, l)

  return(df)
}

#' @title Drops poor quality geocodes
#' @description Use the geocodeQualityCode value returned to determine the quality of the geocode. https://developer.mapquest.com/documentation/geocoding-api/quality-codes/.
#' @export
dm.drop_poor_quality <- function(df,
                                 qualityCode = "A1|A3|A4") {

  poorQuality <- stringr::str_starts(string = df$geocodeQualityCode, pattern = qualityCode)

  df <- df %>%
    dplyr::mutate(lat = ifelse(poorQuality, NA, lat),
                  long = ifelse(poorQuality, NA, long)
    )

  return(df)
}

#' @title Geocode addresses
#' @description Geocodes addresses using the Mapquest API
#' @param addresses vector. The list of addresses to geocode.
#' @param key string. The api key registered with your personal Mapquest account.
#' @export
dm.geocode_address <- function(df,
                               state_fips,
                               version = "v1",
                               url = "http://www.mapquestapi.com/geocoding/{version}/batch?key={key}",
                               limit = 100) {
  
  get_key.mapquest()

  bb <- tx_bounding_box(state_fips = state_fips)

  calls <- split_calls(v = df$address,
                       limit = limit)
  
  l <- lapply(calls, function(call, url, version) {
    r <- httr::POST(url = glue::glue(url, version = version, key = ),
                    query = list(key = key),
                    body = list(locations = call,
                                boundingBox = bb,
                                maxResults = 1),
                    encode = "json")

    if(r$status_code == 200) {

      c <- httr::content(r)

      df <- dm.geocode_request(c)

      return(df)

    } else {
      warning("status not 200")
    }

  }, url = url, version = version, key = key)

  df <- do.call(rbind, l)

  df <- df %>%
    dplyr::bind_cols(addresses %>%
                       dplyr::select(operation_number))

  df <- dm.drop_poor_quality(df)

  return(df)
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

  get_key.mapquest()
  
  l <- lapply(latLng, function(x, url, version) {
    r <- httr::POST(url = glue::glue(url, version = version, key = key),
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

  df <- do.call(rbind, l)

  return(df)
}

#' @title Subset CCL for geocoding
dm.subset_ccl_geocode <- function(df) {

  df %>%
    dplyr::filter(is.na(lat) | is.na(long)) %>%
    dplyr::select(opeartion_number, address)

}
