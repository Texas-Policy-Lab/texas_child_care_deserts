#' @title Parse frontline date
#' @param format string. The format to use to parse the string. Default is
#' '%m.%d.%y'.
#' @return date
parse_date.frontline <- function(x, format = "\\d{4}-\\d{2}-\\d{2}"){

  date <- lubridate::ymd(stringr::str_extract(x, format))
}

#' @title Data management for reporting column
#' @description Filter to only "reporting"
#' @param df
#' @return data.frame
col.reporting <- function(df){
  
  df <- df %>% 
    dplyr::filter(reporting_status == "Reporting") %>% 
    dplyr::select(-reporting_status)
  
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
                         max_date = as.Date("11162021", "%m%d%Y")){

  df <- df %>% 
    dplyr::mutate(mod_date = as.Date(ifelse(grepl("-", last_modified_at_a),
                                            as.Date(last_modified_at_a,
                                                    format = "%Y-%m-%d"),
                                            as.Date(last_modified_at_a,
                                                    format = "%m/%d/%y")),
                                     origin = "1970-01-01"),
                  days_since_mod = max_date - mod_date) %>% 
    dplyr::select(-last_modified_at_a) %>% 
    dplyr::filter(days_since_mod >= 0 & days_since_mod <= 15) %>% 
    dplyr::group_by(operation_number) %>% 
    dplyr::slice(which.max(mod_date))
  
}

#' @title Data management for availability data
#' @description Manage availability: sum 0-3 availability and total availability
#' @param df
#' @return data.frame
col.availability <- function(df){

  df <- df %>%  
    dplyr::mutate(availability_03 = infant_availability + toddler_availability + prek_availability/2,
                  availability_05 = infant_availability + toddler_availability + prek_availability,
                  availability_total = infant_availability + toddler_availability + prek_availability + school_availability) %>% 
    dplyr::select(-c(infant_availability,
                     toddler_availability,
                     prek_availability,
                     school_availability))
  
}

#' @title Data management for enrollment data
#' @description Manage enrollment: sum 0-3 enrollment and total enrollment
#' @param df
#' @return data.frame
col.enrollment <- function(df){

  df <- df %>%  
    dplyr::mutate(enrollment_03 = infant_enrollment + toddler_enrollment + prek_enrollment/2,
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

#' @title Merge availability and enrollment data
#' @description If "frontline" availability and enrollment data come in two separate sheets,
#' merge based on operation number
merge.frontline_sheets <- function(raw_pth,
                                   ava_file_name,
                                   enr_file_name,
                                   full_file_name){
  
  availability <- readxl::read_excel(file.path(raw_pth, ava_file_name))
  enrollment <- readxl::read_excel(file.path(raw_pth, enr_file_name))
  
  ava_enr <- enrollment %>% 
    dplyr::rename("OpNumber" = `Op Number`) %>% 
    dplyr::full_join(availability, by = "OpNumber") %>% 
    dplyr::select(OpNumber,
                  starts_with(c("Enr", "Avail"))) %>% 
    dplyr::select_all(~gsub(": ", "", .))
  
  write.csv(ava_enr, file.path(raw_pth, full_file_name), row.names = F)
}

#' @title Download Frontline provider data
#' @description Frontline provider data comes from the childcare.bowtiebi.com portal. Current sheet was emailed by Myriam Guillen 11/16.
dwnld.frontline <- function(raw_pth,
                            full_file_name = "frontline/2022-04-07_ava_enr.csv",
                            ava_file_name = "frontline/Availability Report_040722_Public.xlsx",
                            enr_file_name = "frontline/Spring Enrollment for TPL.xlsx") {
  
  ava_enr_pth <- file.path(raw_pth, full_file_name)
  already_run <- file.exists(ava_enr_pth)
  
  if (!already_run) {
    
    merge.frontline_sheets(raw_pth,
                           ava_file_name,
                           enr_file_name,
                           full_file_name)
    
  }
  
  df <- read.csv(file.path(raw_pth, full_file_name)) %>%
    dplyr::mutate(export_date = parse_date.frontline(full_file_name))
}

#' @title Data management for frontline provider data
#' @description Manage frontline data, specifically enrollment and attendance numbers
dm.frontline <- function(df,
                         input_columns = list(OpNumber = "integer",
                                              AvailSlotsInfants = "integer",
                                              AvailSlotsToddlers = "integer",
                                              AvailSlotsPreschool = "integer",
                                              AvailSlotsSchoolAge = "integer",
                                              EnrInfants = "integer",
                                              EnrToddlers = "integer",
                                              EnrPreschool = "integer",
                                              EnrSchoolAge = "integer",
                                              export_date = "Date")){

  df <- df %>%
    test_input(input_columns) %>%
    dplyr::select_all(~gsub(" ", "_", tolower(.))) %>% 
    dplyr::rename(operation_number = opnumber,
                  infant_availability = "availslotsinfants",
                  infant_enrollment = "enrinfants",
                  toddler_availability = "availslotstoddlers",
                  toddler_enrollment = "enrtoddlers",
                  prek_availability = "availslotspreschool",
                  prek_enrollment = "enrpreschool",
                  school_availability = "availslotsschoolage",
                  school_enrollment = "enrschoolage") %>% 
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
