#' @title Download precinct boundaries
#' @description data downloaded from
#' https://data-tarrantcounty.opendata.arcgis.com/datasets/commissioner-precinct/explore?location=32.766250%2C-97.230678%2C10.82
#' https://harriscounty.maps.arcgis.com/home/item.html?id=a51b563da9ad479786a05f5c9f946e4c
#' @note Year: 2020+
#' Geography: Harris & Tarrant county precinct boundaries
#' Geography type: Commissioner precinct boundraies 
#' File type: geojson
dwnld.precinct_boundaries <- function(raw_pth, county_fips) {
    # pass this function in twice with different county_fips for two separate precinct boundaries dataframes
    # reading in precinct geojson files ... 
    if (county_fips == "48201") {
        precinct_file_name <- "Harris_precinct_polygons.geojson"
    }
    if (county_fips == "48439") {
        precinct_file_name <- "Tarrant_precinct_polygons.geojson"
    }
    coords_precincts <- geojsonsf::geojson_sf(file.path(raw_pth, precinct_file_name))
    return(coords_precincts)
}

#' @title Find which precinct providers within Harris/Tarrant belong to
retrieve_precincts <- function(COORDS_PRECINCTS, DF_HHSC_CCL) {
    # add package dependencies geojsonsf

    # reading in providers... 
    coords_prvdrs <- DF_HHSC_CCL %>% dplyr::select(operation_number, county_code, lat, long)

    # converting providers to sf (geo object), transform so coordinates match...
    coords_prvdrs <- sf::st_as_sf(coords_prvdrs, 
                            coords = c("long", "lat"), 
                            crs = 4326, # ensuring consistent encodings
                            agr = "constant", 
                            na.fail=FALSE
    )
    # coords_prvdrs <- coords_prvdrs %>% sf::st_transform(crs = sf::st_crs(COORDS_PRECINCTS))
    coords_prvdrs <- coords_prvdrs %>% sf::st_transform(crs = 4326)
    
    # accepts objects of class sf, sfc, sfg, returns 
    intersection <- sf::st_contains(COORDS_PRECINCTS, coords_prvdrs)
    df_intersection <- intersection %>% 
                            as.data.frame() %>% 
                            dplyr::as_tibble() %>% 
                            dplyr::rename(c("precinct"="row.id",
                                            "index_operation"="col.id"))
    
    # merging initial providers by row index
    coords_prvdrs$index <- rownames(coords_prvdrs) %>% as.integer()
    df_merge <- coords_prvdrs %>% dplyr::left_join(df_intersection, by=c("index"="index_operation"))
    df_final <- df_merge %>% 
                    dplyr::as_tibble() %>% 
                    dplyr::select(operation_number, county_code, precinct)

    # returns dataframe with {operation_number (chr), county_code (dbl), precinct (int)}
    return(df_final) 
}


