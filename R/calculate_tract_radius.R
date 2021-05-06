#' @title Calculate distance between two zip codes in miles
#' @description Find lat/lon coordinates for each zip code, calculate straight 
#' line distance.
#' @param df. The data frame containing matched zip code columns.
#' @param meter. Default is 1609, the conversion for the number of meters in a 
#' mile.
#' @return data.frame with distance column.
distance_between_zips <- function(df,
                                  meter = 1609) {

  assertthat::assert_that(all(c("zip1", "zip2") %in% colnames(df)))

  GEO_ZIP$geometry <- NULL
  
  df <- df %>%
    dplyr::left_join(GEO_ZIP %>% 
                       dplyr::rename(zip1 = zip,
                                     lat1 = lat,
                                     lon1 = lon)) %>%
    dplyr::left_join(GEO_ZIP %>% 
                       dplyr::rename(zip2 = zip,
                                     lat2 = lat,
                                     lon2 = lon)) %>% 
    dplyr::mutate(distance = geosphere::distHaversine(cbind(lon1, lat1), 
                                                      cbind(lon2, lat2)) / meter)

  return(df)
}

#' @title ACF data management to get zip codes for providers
#' @description Merge home and child care zip codes and calculate distance
#' @param df data.frame. The dataframe of ACF data.
#' @param max_child_age. Integer. Age to filter the child age to be less than or
#' equal.
#' @return data.frame
dm.acf_dist <- function(df = DF_ACF,
                        max_child_age = 4) {
  
  df <- df %>%
    dplyr::filter(child_age <= max_child_age) %>% 
    dplyr::rename(zip1 = family_zip, 
                  zip2 = provider_zip) %>% 
    distance_between_zips()

  assertthat::assert_that(is.numeric(df$distance))
  assertthat::assert_that(all(df$distance[df$zip1 == df$zip2] %in% c(0, NA)))

  return(df)
}

#' @title ACF data management to get zip codes for providers
#' @description Clean ACF provider data, find coordinates for each zip code, 
#' calculate straight line distance.
#' @param df data.frame. The merged family/home ACF data from dm.acf_dist 
#' function.
#' @param county_list. A list of TX county codes of counties of interest to 
#' compare. 
#' Eg county_list = c(439) for Tarrant County; county_list = c(201, 439) to 
#' compare Harris to Tarrant County
#' @return a table and a plot. Decile values for distance between home and 
#' provider zip code and density plot of distances.
summary.acf_dist <- function(df = DF_ACF,
                             county_list = NULL) {

  df <- df %>% 
    dplyr::left_join(XWALK_ZIP_COUNTY, by = c("family_zip" = "zip")) %>% 
    dplyr::left_join(XWALK_ZIP_COUNTY, by = c("provider_zip" = "zip"), 
                     suffix = c("_family", "_provider"))

  subset <- df %>%
    dplyr::filter(county_family %in% county_list & county_provider %in% county_list & county_family == county_provider) %>% 
    dplyr::left_join(tigris::fips_codes %>% 
                       dplyr::filter(state_code == 48) %>%
                       dplyr::mutate(county_code = as.numeric(county_code)),
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
