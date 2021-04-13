#' @title Get NBER Tract data
#' @description Link to data: http://data.nber.org/distance/2010/sf1/tract/sf12010tractdistance25miles.csv
#' @details 25 mile radius is used rather then the 5 miles radius because the 5 mile radius was missing two counties in Harris County
#' @param pth string. The raw path to write the data to.
#' @param url string. The URL to download the data from.
#' @export
dwnld.nber_tract_data <- function(pth,
                                  url = "http://data.nber.org/distance/2010/sf1/tract/sf12010tractdistance25miles.csv") {

  fl <- basename(url)

  if (!(fl %in% list.files(pth))) {

    httr::GET(url, httr::write_disk(temp_dir <- tempfile(fileext = ".xlsx")))
    file.copy(from = temp_dir, to = pth)
  }

}
