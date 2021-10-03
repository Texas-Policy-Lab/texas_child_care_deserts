#' @title Data management for date column
#' @description Manage date
#' @return object
col.date <- function(x, date_format = "%m-%d-%Y") {

  x$df <- x$df %>%
    dplyr::mutate(date = as.Date(date, format = date_format)) %>%
    dplyr::filter(date <= export_date)

  assertthat::assert_that(all(x$df$date <= x$df$export_date))

  return(x)
}

#' @title Data management for modification date column
#' @description Returns a data.frame with the most recent modification date for 
#' each provider.
#' @return object
col.mod_date <- function(x,
                         date_format = "%m%d%Y") {

  max_date <- as.Date(x$max_date, date_format)

  x$df <- x$df %>% 
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

  return(x)
}

#' @title Data management for availability data
#' @description Manage availability: sum 0-3 availability and total availability
#' @param x
#' @return object
col.availability <- function(x) {
  browser()
  x$df <- x$df %>%
    dplyr::mutate(dplyr::across(ends_with("capacity"), 
                                function(x) tidyr::replace_na(as.numeric(x), 0)),
                  availability_03 = infant_capacity + toddler_capacity,
                  availability_05 = infant_capacity + toddler_capacity + prek_capacity,
                  availability_total = infant_capacity + toddler_capacity + prek_capacity + school_capacity) %>% 
    dplyr::select(-c(infant_capacity, toddler_capacity, prek_capacity, 
                     school_capacity))
  
  return(x)
}

#' @title Data management for enrollment data
#' @description Manage enrollment: sum 0-3 enrollment and total enrollment
#' @param df
#' @return object
col.enrollment <- function(x) {
  browser()
  x$df <- x$df %>%  
    dplyr::mutate(dplyr::across(ends_with("enrollment"), 
                                function(x) tidyr::replace_na(as.numeric(x), 0)),
                  enrollment_03 = infant_enrollment + toddler_enrollment,
                  enrollment_05 = infant_enrollment + toddler_enrollment + prek_enrollment,
                  enrollment_total = infant_enrollment + toddler_enrollment + prek_enrollment + school_enrollment) %>% 
    dplyr::select(-c(infant_enrollment, toddler_enrollment, prek_enrollment,
                     school_enrollment))

  return(x)
}

#' @title Data management for total seats
#' @description Find total seats for each provider by summing enrollment and 
#' availability
#' @param df
#' @return object
col.seats <- function(x) {
  browser()
  x$df <- x$df %>%
    dplyr::mutate(seats_03 = availability_03 + enrollment_03,
                  seats_05 = availability_05 + enrollment_05,
                  seats_total = availability_total + enrollment_total)
  return(x)
}

#' @title Frontline attributes
#' @param input_columns. The columns to keep from the data.
#' @param max_date. Two week period with the highest reponse rate. Default is 
#' 08022021.
#' @return object
attr.frontline <- function(pth,
                           max_date = "08042021") {

  # TODO: For now, we are manually picking the two week period with highest 
  # response for the max_date. This will be updated to something more dynamic 
  # in the future.
  
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
  )

  name <- "frontline/export_translation_Daily_Vacancy_2021-09-07_05_12_48.csv"

  list(input_columns = input_columns,
       max_date = max_date,
       name = name,
       pth = pth)
}

#' @title Parse frontline date
#' @param format string. The format to use to parse the string. Default is
#' '%m.%d.%y'.
#' @return date
parse_date.frontline <- function(x, date_format = "\\d{4}-\\d{2}-\\d{2}") {
  
  lubridate::ymd(stringr::str_extract(x, date_format))
}

#' @title Download Frontline provider data
#' @description Frontline provider data comes from the childcare.bowtiebi.com 
#' portal (currently, only Sadie has login)
#' @param x
#' @return object
dwnld.frontline <- function(x) {

  x$df <- readr::read_csv(file.path(x$pth, x$name)) %>%
    dplyr::mutate(export_date = parse_date.frontline(x$name))

  return(x)
}

#' @title Select and rename columns
#' @param x
#' @return object
col.select <- function(x) {
  
  x$df <- x$df %>%
    test_input(x$input_columns) %>%
    dplyr::select_all(~ gsub(" ", "_", tolower(.))) %>%
    dplyr::rename(operation_number = op_number,
                  prek_capacity = "pre-_k_capacity",
                  prek_enrollment = "preschool_enrollment",
                  school_capacity = "school_aged_capacity",
                  school_enrollment = "schoolage_enrollment")
  return(x)
}

#' @title Fronline provider data management steps
#' @description Data management steps, including, specifically enrollment and 
#' attendance, summation of seats
dm.frontline <- function(x) {

  x <- x %>%
    col.select() %>%
    col.date() %>%
    col.mod_date() %>%
    col.operation_number() %>%
    col.availability() %>%
    col.enrollment() %>%
    col.seats()

  return(x$df)
}

#' @title Process the frontline data
process.frontline <- function(pth) {

  attr.frontline(pth) %>%
    dwnld.frontline() %>%
    dm.frontline()
}
