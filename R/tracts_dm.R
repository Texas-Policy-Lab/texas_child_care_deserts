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
dm.tracts_xwalk <- function(df,
                            processed_pth,
                            name,
                            radius) {

  df <- df %>%
    dplyr::mutate(anchor_tract = paste0(county1, tract1),
                  surround_tract = paste0(county2, tract2))

  tracts <- df %>%
    dplyr::distinct(anchor_tract) %>% 
    dplyr::mutate(surround_tract = anchor_tract)

  df <- df %>% 
    dplyr::filter(mi_to_tract <= radius) %>% 
    dplyr::bind_rows(tracts) %>% 
    dplyr::select(anchor_tract, surround_tract)

  readr::write_csv(df, file.path(processed_pth, name))

  return(df)
}

#' @title Process ACF data
process.tracts_xwalk <- function(tracts_xwalk) {

  tracts_xwalk$df <- readr::read_csv(tracts_xwalk$raw_pth, tracts_xwalk$name)
  tracts_xwalk <- do.call(dm.tracts_xwalk, tracts_xwalk)
}
