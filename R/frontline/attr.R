attr.frontline <- function(pth) {
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

  start_date <- "07082021"
  max_date <- "08042021"

  name <- "frontline/export_translation_Daily_Vacancy_2021-09-07_05_12_48.csv"

  list(input_columns = input_columns,
       start_date = start_date,
       max_date = max_date,
       name,
       pth = pth)
}