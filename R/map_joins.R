#' @title Anchor tracts
anchor_tract <- function(xwalk_tracts) {
  xwalk_tracts %>% 
    dplyr::distinct(anchor_tract)
}

#' @title Filter to selected anchor_tract
#' @param df_tracts_xwalks data.frame. Crosswalk between the anchor and surround
#' tracts
#' @param input_mkt data.frame. Data frame with subset of tracts to map
anchor_tract_sub <- function(df_anchor_tract,
                             input_mkt = NULL) {

  if (is.null(input_mkt)) {

    df_anchor_tract

  } else {

    df_anchor_tract %>%
      dplyr::filter(anchor_tract %in% input_mkt)
  }
}

#' @title Filter to corresponding surround tracts to the anchor tract
#' @param df_tracts_xwalks data.frame. Crosswalk between the anchor and surround
#' @param xwalk_tracts data.frame. Contains the filtered anchor tracts.
#' @param input_mkt data.frame. Data frame with subset of tracts to map
surround_tract_sub <- function(df_anchor_tract,
                               xwalk_tracts) {

  xwalk_tracts %>%
    dplyr::right_join(df_anchor_tract) %>%
    dplyr::distinct(surround_tract)

}

#' @title Filters the desert choices to a single desert
#' @param df_desert data.frame. A data frame with all the desert choices.
#' @param input_dsrt string. The desert to display.
desert_sub <- function(df_desert,
                       input_dsrt) {

  df_desert %>% 
    dplyr::filter(dsrt_type %in% input_dsrt)
}
