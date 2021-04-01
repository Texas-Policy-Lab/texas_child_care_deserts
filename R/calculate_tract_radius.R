#' @title ACF data management to get zip codes for families
#' @description Clean ACF family data.
#' @param df data.frame. The dataframe of acf family data.
#' @param zip_latlong_xwalk data.frame. The crosswalk of zip code to latitude/longitude.
#' @param zip_county_xwalk data.frame. The crosswalk of zip code to county.
#' @param input_columns. List. List of the columns to keep.
#' @param max_child_age. Integer. Age to filter the child age to be less than or equal.
#' @return data.frame

dm.acf_family <- function(df,
                          zip_latlong_xwalk,
                          zip_county_xwalk,
                          input_columns = list(ChildrenID = "numeric",
                                               Parents.FamilyZip = "numeric",
                                               CCSettings.ProviderStateID = "numeric",
                                               Parents.ReportingDate = "POSIXct",
                                               Age = "numeric"),
                          max_child_age = 4){
  
  df <- df %>% 
    test_input(input_columns) %>% 
    dplyr::filter(Age <= max_child_age) %>% 
    dplyr::left_join(zip_latlong_xwalk, by = c("Parents.FamilyZip" = "zip")) %>% 
    dplyr::left_join(zip_county_xwalk, by = c("Parents.FamilyZip" = "zip")) %>% 
    dplyr::select(child_id = ChildrenID,
                  family_zip = Parents.FamilyZip,
                  provider_id = CCSettings.ProviderStateID,
                  date = Parents.ReportingDate,
                  lat,
                  lon,
                  county)
  
  assertthat::assert_that(is.numeric(df$family_zip),
                          msg = "Zip not numeric")
  
  assertthat::assert_that(is.numeric(df$provider_id),
                          msg = "Provider ID not numeric")
  
  assertthat::assert_that(is.numeric(df$lat),
                          msg = "Latitude not numeric")
  
  assertthat::assert_that(is.numeric(df$lon),
                          msg = "Longitude not numeric")
  
  assertthat::assert_that(is.numeric(df$county),
                          msg = "County not numeric")
  
  return(df)
  
}

#' @title ACF data management to get zip codes for providers
#' @description Clean ACF provider data.
#' @param df data.frame. The dataframe of acf provider data.
#' @param zip_latlong_xwalk data.frame. The crosswalk of zip code to latitude/longitude.
#' @param zip_county_xwalk data.frame. The crosswalk of zip code to county.
#' @param input_columns. List. List of the columns to keep.
#' @return data.frame

dm.acf_provider <- function(df,
                            zip_latlong_xwalk,
                            zip_county_xwalk,
                            input_columns = list(Data.StateID = "numeric",
                                                 Data.ZipCode = "character",
                                                 Data.ReportingDate = "POSIXct")){
  
  df <- df %>% 
    test_input(input_columns) %>% 
    dplyr::mutate(provider_zip = as.numeric(Data.ZipCode)) %>% 
    dplyr::left_join(zip_latlong_xwalk, by = c("provider_zip" = "zip")) %>% 
    dplyr::left_join(zip_county_xwalk, by = c("provider_zip" = "zip")) %>% 
    dplyr::select(provider_id = Data.StateID,
                  provider_zip,
                  date = Data.ReportingDate,
                  lat,
                  lon,
                  county)
  
  assertthat::assert_that(is.numeric(df$provider_id),
                          msg = "Provider ID not numeric")
  
  assertthat::assert_that(is.numeric(df$provider_zip),
                          msg = "Zip not numeric")
  
  assertthat::assert_that(is.numeric(df$lat),
                          msg = "Latitude not numeric")
  
  assertthat::assert_that(is.numeric(df$lon),
                          msg = "Longitude not numeric")
  
  assertthat::assert_that(is.numeric(df$county),
                          msg = "County not numeric")
  
  return(df)
  
}

#' @title ACF data management to get zip codes for providers
#' @description Merge home and child care zip codes and calculate distance
#' @param df_family data.frame. The dataframe of acf family data.
#' @param df_family data.frame. The dataframe of acf family data.
#' @return data.frame

dm.acf_dist <- function(df_family,
                        df_provider){
  browser()
  zip_latlong_xwalk <- get.zip_latlong_xwalk()
  zip_county_xwalk <- get.zip_county_xwalk()
  
  assertthat::assert_that(all(c("zip", "lat", "lon") %in% colnames(zip_latlong_xwalk)),
                          msg = "Zip lat/long xwalk missing column")
  assertthat::assert_that(all(c("zip", "county") %in% colnames(zip_county_xwalk)),
                          msg = "Zip county xwalk missing column")

  df_family <- dm.acf_family(df_family, zip_latlong_xwalk, zip_county_xwalk)
  df_provider <- dm.acf_provider(df_provider, zip_latlong_xwalk, zip_county_xwalk)
  
  df <- df_family %>% 
    dplyr::left_join(df_provider, by = c("provider_id", "date"), suffix = c("_family", "_provider")) %>% 
    dplyr::mutate(distance = geosphere::distHaversine(cbind(lon_family, lat_family), cbind(lon_provider, lat_provider)) / 1609)
  
  assertthat::assert_that(is.numeric(df$distance))
  
  return(df)
}

#' @title ACF data management to get zip codes for providers
#' @description Clean ACF provider data.
#' @param df data.frame. The merged family/home ACF data from dm.acf_dist function.
#' @param county_list. A list of TX county codes of counties of interest to compare. 
#' Eg county_list = c(439) for Tarrant County; county_list = c(201, 439) to compare Harris to Tarrant County
#' @return a table and a plot. Decile values for distance between home and provider zip code and density plot of distances.

summary.acf_dist <- function(df,
                             county_list = NULL){
  
  subset <- df %>% 
    dplyr::filter(county_family %in% county_list & county_provider %in% county_list & county_family == county_provider) %>% 
    dplyr::left_join(tigris::fips_codes %>% dplyr::filter(state_code == 48) %>% dplyr::mutate(county_code = as.numeric(county_code)),
                     by = c("county_family" ="county_code")) %>% 
    dplyr::rename(county_label = county)
  
  assertthat::assert_that(all(unique(subset$county_family) %in% county_list))
  assertthat::assert_that(all(unique(subset$county_provider) %in% county_list))
  assertthat::assert_that(all(subset$county_provider == subset$county_provider))
  
  assertthat::assert_that(sum(is.na(subset$distance)) == 0,
                          msg = "NAs in the distance")
  
  density_plot <- ggplot2::ggplot(subset) +
    ggplot2::geom_density(ggplot2::aes(x = distance, group=county_label, fill = county_label), alpha = .5) +
    ggplot2::theme_minimal()
  
  decile_table <- subset %>% 
    dplyr::group_by(county_label) %>% 
    dplyr::summarise(decile = list(quantile(distance, prob = seq(0, 1, .1)))) %>% 
    tidyr::unnest_wider(col = decile)
  
  return(list(density_plot, decile_table))
  
}
