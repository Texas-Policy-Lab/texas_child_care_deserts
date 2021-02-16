#' @title Returns the most recent HHSC CLL Daycare and Residential Operations 
#' Data
#' @description Link to data: 
#' https://data.texas.gov/Social-Services/HHSC-CCL-Daycare-and-Residential-Operations-Data/bc5r-88dy/data
#' @param pth. string. Path to download the data to.
#' @param url. string. Default is https://data.texas.gov/api/views/bc5r-88dy/rows.csv?accessType=DOWNLOAD 
#' and links the the Health and Human Services Child Care Licensing Data
#' @export
get.hhsc_ccl_data <- function(pth,
                              url = "https://data.texas.gov/api/views/bc5r-88dy/rows.csv?accessType=DOWNLOAD",
                              df_name = "HHSC_CCL_Daycare_and_Residential_Operations_Date.csv",
                              ...) {
  dwnld_df(url = url,
           pth = pth,
           df_name = df_name,
           ...)
}
