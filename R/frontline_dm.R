#' @title Parse frontline date
#' @param format string. The format to use to parse the string. Default is
#' '%m.%d.%y'.
#' @return date
parse_date.frontline <- function(x, format = "\\d{4}-\\d{2}-\\d{2}"){

  date <- lubridate::ymd(stringr::str_extract(x, format))
}

#' @title Data management for date column
#' @description Manage date: filter date to be less than export date so that future dates for which 
#' providers have not yet entered data will be excluded.
#' @param df
#' @return data.frame
col.date <- function(df){

  df <- df %>% 
    dplyr::mutate(date = as.Date(date, format = "%m-%d-%Y")) %>% 
    dplyr::filter(date <= export_date)
  
  assertthat::assert_that(all(df$date <= df$export_date))
  
  return(df)
}

#' @title Data management for modification date column
#' @description Manage modification date: pick most recent modification for each provider
#' @param df
#' @param start_date. If there is a date that Frontline/Bowtie tells us has a processing/data error, 
#' set this as start_date, use only modification dates after this date
#' @param max_date. For now, we are manually picking the two week period with highest response. This will 
#' be updated to something more dynamic in the future.
#' @return data.frame
col.mod_date <- function(df, 
                         start_date = as.Date("07082021", "%m%d%Y"),
                         max_date = as.Date("08042021", "%m%d%Y")){

  df <- df %>% 
    dplyr::mutate(mod_date = as.Date(ifelse(grepl("-", last_modified_at_a),
                                            as.Date(last_modified_at_a,
                                                    format = "%Y-%m-%d"),
                                            as.Date(last_modified_at_a,
                                                    format = "%m/%d/%y")),
                                     origin = "1970-01-01"),
                  days_since_mod = max_date - mod_date) %>% 
    dplyr::select(-last_modified_at_a) %>% 
    dplyr::filter(days_since_mod >= 0 & days_since_mod <= 14) %>% 
    dplyr::group_by(operation_number) %>% 
    dplyr::slice(which.max(mod_date))
  
}

#' @title Data management for availability data
#' @description Manage availability: sum 0-3 availability and total availability
#' @param df
#' @return data.frame
col.availability <- function(df){

  df <- df %>%  
    dplyr::mutate(dplyr::across(ends_with("capacity"), function(x) tidyr::replace_na(as.numeric(x), 0)),
                  availability_03 = infant_capacity + toddler_capacity + prek_capacity/2,
                  availability_05 = infant_capacity + toddler_capacity + prek_capacity,
                  availability_total = infant_capacity + toddler_capacity + prek_capacity + school_capacity) %>% 
    dplyr::select(-c(infant_capacity,
                     toddler_capacity,
                     prek_capacity,
                     school_capacity))
  
}

#' @title Data management for enrollment data
#' @description Manage enrollment: sum 0-3 enrollment and total enrollment
#' @param df
#' @return data.frame
col.enrollment <- function(df){

  df <- df %>%  
    dplyr::mutate(dplyr::across(ends_with("enrollment"), function(x) tidyr::replace_na(as.numeric(x), 0)),
                  enrollment_03 = infant_enrollment + toddler_enrollment + prek_enrollment/2,
                  enrollment_05 = infant_enrollment + toddler_enrollment + prek_enrollment,
                  enrollment_total = infant_enrollment + toddler_enrollment + prek_enrollment + school_enrollment) %>% 
    dplyr::select(-c(infant_enrollment,
                     toddler_enrollment,
                     prek_enrollment,
                     school_enrollment))
  
}

#' @title Data management for total seats
#' @description Find total seats for each provider by summing enrollment and availability
#' @param df
#' @return data.frame
col.seats <- function(df){

  df <- df %>%  
    dplyr::mutate(seats_03 = availability_03 + enrollment_03,
                  seats_05 = availability_05 + enrollment_05,
                  seats_total = availability_total + enrollment_total)
  
}

#' @title Download Frontline provider data
#' @description Frontline provider data comes from the childcare.bowtiebi.com portal (currently, only Sadie has login)
dwnld.frontline <- function(raw_pth,
                            name = "frontline/export_translation_Daily_Vacancy_2021-09-07_05_12_48.csv") {

  df <- readr::read_csv(file.path(raw_pth, name)) %>%
    dplyr::mutate(export_date = parse_date.frontline(name))
}

#' @title Data management for frontline provider data
#' @description Manage frontline data, specifically enrollment and attendance numbers
dm.frontline <- function(df,
                         input_columns = list(`OP Number` = "character",
                                              `Infant Capacity` = "numeric",
                                              `Toddler Capacity` = "numeric",
                                              `Pre- K Capacity` = "numeric",
                                              `School Aged Capacity` = "numeric",
                                              Infant_enrollment = "numeric",
                                              Toddler_enrollment = "numeric",
                                              Preschool_enrollment = "numeric",
                                              SchoolAge_enrollment = "numeric",
                                              last_modified_at_A = "character",
                                              Date = "character",
                                              export_date = "Date"
                                              )){

  df <- df %>%
    test_input(input_columns) %>%
    dplyr::select_all(~gsub(" ", "_", tolower(.))) %>% 
    dplyr::rename(operation_number = op_number,
                  prek_capacity = "pre-_k_capacity",
                  prek_enrollment = "preschool_enrollment",
                  school_capacity = "school_aged_capacity",
                  school_enrollment = "schoolage_enrollment") %>% 
    col.date()  %>% 
    col.mod_date() %>% 
    col.operation_number() %>% 
    col.availability() %>% 
    col.enrollment() %>% 
    col.seats()

}

#' @title Process the frontline data
process.frontline <- function(raw_pth) {

  df <- dwnld.frontline(raw_pth) %>%
    dm.frontline()
}
