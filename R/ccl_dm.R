#' @title Get HHSC CCL data
#' @description Returns the most recent HHSC CLL Daycare and Residential Operations 
#' Data. Link to data: https://data.texas.gov/resource/bc5r-88dy.csv
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.hhsc_ccl_data <- function(data_in_name,
                              data_in_pth,
                              url = "https://data.texas.gov/api/views/bc5r-88dy/rows.csv?accessType=DOWNLOAD",
                              download_date = Sys.Date()) {
  
  fl_name <- glue::glue(as.character(download_date), data_in_name, .sep = "_")
  
  dwnld_pth <- file.path(data_in_pth, fl_name)
  
  download.file(url, destfile = dwnld_pth, mode = "wb")
  
}