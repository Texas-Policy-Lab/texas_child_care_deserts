#' @title Get NBER Tract data
#' @description Link to data: http://data.nber.org/distance/2010/sf1/tract/sf12010tractdistance25miles.csv
#' @details 25 mile radius is used rather then the 5 miles radius because the 5 mile radius was missing two counties in Harris County
#' @param pth string. The raw path to write the data to.
#' @param url string. The URL to download the data from.
#' @export
dwnld.xwalk_tracts <- function(raw_pth,
                               url = "http://data.nber.org/distance/2010/sf1/tract/sf12010tractdistance25miles.csv") {

  fl <- basename(url)

  if (!(fl %in% list.files(raw_pth))) {

    httr::GET(url, httr::write_disk(temp_dir <- tempfile(fileext = ".csv")))
    file.copy(from = temp_dir, to = file.path(raw_pth, fl))
  }

  df <- read.csv(file.path(raw_pth, fl))

  return(df)
}

#' @title Data management steps for the tracts crosswalk
#' @description Creates a crosswalk between anchor tracts and surround tracts within x miles of each anchor tract
#' @param df data.frame. The tracts data to process
#' @param pth string. The name of the data to read in.
#' @param name string. The name of the data to write out.
#' @export
dm.xwalk_tracts <- function(x, tract_side = "right", county_side = "left", 
                            tract_width = 11, county_width = 5, pad = "0") {

  df <- x$df %>%
    dplyr::mutate(texas = ifelse(substr(county1, 1, 2) == "48", TRUE, FALSE)) %>%
    dplyr::filter(texas) %>%
    dplyr::select(-texas) %>%
    dplyr::mutate(anchor_tract = stringr::str_pad(paste0(county1, tract1), 
                                                  side = tract_side, 
                                                  width = tract_width,
                                                  pad = pad),
                  surround_tract = stringr::str_pad(paste0(county2, tract2), 
                                                    side = tract_side, 
                                                    width = tract_width,
                                                    pad = pad),
                  anchor_county = stringr::str_pad(county1, 
                                                   side = county_side, 
                                                   width = county_width, 
                                                   pad = pad),
                  surround_county = stringr::str_pad(county2, 
                                                     side = county_side,
                                                     width = county_width, 
                                                     pad = pad))

  tracts <- df %>%
    dplyr::distinct(anchor_tract, anchor_county) %>%
    dplyr::mutate(surround_tract = anchor_tract,
                  surround_county = anchor_county,
                  mi_to_tract = 0)

  df <- df %>%
    dplyr::bind_rows(tracts) %>% 
    dplyr::select(anchor_county, anchor_tract, 
                  surround_county, surround_tract, mi_to_tract)

  assertthat::assert_that(all(nchar(df$anchor_tract) == tract_width))
  assertthat::assert_that(all(nchar(df$surround_tract) == tract_width))
  assertthat::assert_that(all(nchar(df$anchor_county) == 5))
  assertthat::assert_that(all(nchar(df$surround_county) == 5))
  
  return(df)
}

#' @title Process ACF data
process.tracts_xwalk <- function(cls) {

  cls$df <- dwnld.xwalk_tracts(raw_pth = cls$raw_pth)
  tracts_xwalk <- dm.xwalk_tracts(cls)
}
