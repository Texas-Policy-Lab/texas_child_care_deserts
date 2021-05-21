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
dm.acf_dist <- function(df,
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
#' @param df data.frame.
#' @param county_list. A list of TX county codes of counties of interest to 
#' compare. E.g county_list = c(48439) for Tarrant County; 
#' county_list = c("48201", "48439") to compare Harris to Tarrant County
#' @export
summary.acf_dist <- function(df, county_list) {
  
  df <- df %>%
    dplyr::filter(family_fips_code %in% county_list) %>%
    dm.acf_dist() %>% 
    dplyr::group_by(family_fips_code, quarter_year)
}

#' @title Distance density plot
#' @param df. data.frame. Default is DF_ACF.
#' @param county_list. A list of TX county codes of counties of interest to 
#' compare. E.g county_list = c(48439) for Tarrant County; 
#' county_list = c("48201", "48439") to compare Harris to Tarrant County
#' @return plot
#' @export
calc.distance_density_plot <- function(df, county_list) {

  summary.acf_dist(df = df, county_list = county_list) %>%
    dplyr::filter(distance <= 10) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_density(ggplot2::aes(x = distance,
                                       fill = quarter_year), alpha = .5) +
    ggplot2::theme_minimal() +
    ggplot2::facet_grid(. ~ family_fips_code)
}

#' @title Distance decile table
#' @param df. data.frame. Default is DF_ACF.
#' @param county_list. A list of TX county codes of counties of interest to 
#' compare. E.g county_list = c(48439) for Tarrant County; 
#' county_list = c("48201", "48439") to compare Harris to Tarrant County
#' @return table
#' @export
calc.distance_decile_table <- function(df, county_list) {

  summary.acf_dist(df = df, county_list = county_list) %>%
    dplyr::summarise(decile = list(quantile(distance, prob = seq(0, 1, .1), 
                                            na.rm = TRUE))) %>% 
    tidyr::unnest_wider(col = decile)
}
