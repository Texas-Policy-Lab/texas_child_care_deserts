#' @title Select quarter year
#' @description Parses the quarter-year parameter and returns the file names
#' which are associated with that quarter year combination
#' @inheritParams dm.acf
#' @return A vector of file paths associated with the given quarter year
select_qtr_year <- function(pth,
                            acf_qtr_years) {

  pth <- file.path(pth, "acf")
  fls <- list.files(pth)

  assertthat::assert_that(length(fls) >= 1, 
                          msg = paste("The path to the data", pth, "is empty. Did you run the necessary download steps to create the child care data base file structure?"))
  fl_opts <- gsub("acf-801-", "", fls)
  fl_opts <- gsub("-twc.xlsx|-twc%20.xlsx", "", fl_opts)

  if (is.null(acf_qtr_years)) {
    acf_qtr_years <- fl_opts
  }

  qtr_years <- sapply(acf_qtr_years, function(qtr_year) {

    qy <- fls[grepl(toupper(qtr_year), toupper(fls))]

    if (length(qy) == 0) {
      return(NULL)
    } else {
      return(qy)
    }

  }, USE.NAMES = F, simplify = TRUE)

  fl_opts <- paste0("\n", paste(fls, collapse = "\n"))

  test <- sapply(qtr_years, is.null, simplify = T)
  assertthat::assert_that(!all(test),
                          msg = paste("\nNo matching files found for the following quarter-years: ", paste(acf_qtr_years[test], collapse = ", "), 
                                      "\nYour quarter-year choices are: ", 
                                      toupper(fl_opts)))

  return(list(fls = file.path(pth, qtr_years),
              acf_qtr_years = acf_qtr_years))
}

#' @title Assigns a class to ACF data
#' @description Assigns a class to all the incoming files which will perform different
#' data management steps according to which file type it is
#' @inheritParams select_qtr_year
assign_acf_class <- function(pth,
                             acf_qtr_years) {

  x <- select_qtr_year(pth = pth,
                       acf_qtr_years = acf_qtr_years)
  fls <- x$fls
  
  acf_qtr_years <- x$acf_qtr_years
  
  lapply(1:length(fls), function(i) {
    fl <- fls[i]
    sheets <- readxl::excel_sheets(fl)

    if ("ChildrenParentsSettings" %in% sheets) {
      cls <- structure(
        list(pth = fl,
             sheet = list(
               "ChildrenParentsSettings" = 
                 list(
                   operation_number = "CCSettings.ProviderStateID",
                   family_zip = "Parents.FamilyZip",
                   family_fips_code = "Parents.FIPS",
                   family_id = "ParentsID",
                   child_id = "ChildrenID",
                   child_age = "Age",
                   date = "ReportingDate"),
               "Providers" = 
                 list(
                   operation_number = "Data.StateID",
                   provider_zip = "Data.ZipCode",
                   date = "Data.ReportingDate")
               )
             ), 
                       class = "cps")
    } else if ("CCSettings" %in% sheets) {
      cls <-  structure(
        list(pth = fl,
             sheet = list(
               "CCSettings" = list(operation_number = "ProviderStateID",
                                   family_id = "ParentsID",
                                   child_id = "ChildrenID",
                                   date = "ReportingDate"),
               "Parents"  = list(family_id = "ParentsID",
                                 family_zip = "FamilyZip",
                                 family_fips_code = "FIPS",
                                 date = "ReportingDate"), 
               "Children"  = list(child_id = "ChildrenID",
                                  child_age = "Age",
                                  date = "ReportingDate"), 
               "Providers"  = list(operation_number = "Data.StateID",
                                   provider_zip = "Data.ZipCode",
                                   date = "Data.ReportingDate"))), 
                        class = "ccs")
    } else {
      cls <- NULL
    }

    cls$qtr <- substr(acf_qtr_years[i], 2, 2)
    cls$year <- substr(acf_qtr_years[i], 4, 7)
    cls$qtr_year <- acf_qtr_years[i]

    assertthat::assert_that(!is.null(cls),
                            msg = "ACF data format has changed")

    return(cls)
  })
}

#' @title Data management steps for the ACF data
dm_acf <- function(x) {

  assertthat::assert_that(all(tools::file_ext(x$pth) %in% c("xlsx", "xls")),
                          msg = "ACF files are not in the expected format of 
                                .xlsx or .xls")

  df <- lapply(names(x$sheet), function(sheet) {

    df <- readxl::read_excel(x$pth, sheet = sheet)
    names(df) <- ifelse(!grepl(paste0(sheet, "\\."), names(df)), paste(sheet, names(df), sep = "."), 
                        names(df))  

    df <- df %>%
      dplyr::select(dplyr::one_of(paste(sheet, unlist(x$sheet[[sheet]], use.names = F), 
                                        sep = ".")))

    names(df) <- names(x$sheet[[sheet]])

    if ("operation_number" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(operation_number = as.character(operation_number),
                      operation_number = stringr::str_pad(operation_number,
                                                          side = "left",
                                                          pad = "0",
                                                          width = 15))
    }

    if ("child_id" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(child_id = as.character(child_id))
    }

    if ("family_id" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(family_id = as.character(family_id))
    }

     df %>% 
      dplyr::distinct()
    }) %>% 
    purrr::reduce(dplyr::inner_join) %>%
    dplyr::mutate(family_zip = stringr::str_pad(family_zip,
                                                side = "left",
                                                pad = "0",
                                                width = 5),
                  family_fips_code = as.character(ifelse(family_fips_code == -1, 
                                                         NA, family_fips_code)),
                  child_age = as.numeric(child_age),
                  family_id = as.character(family_id),
                  family_zip = as.character(family_zip),
                  provider_zip = as.character(provider_zip),
                  quarter = x$qtr,
                  year = x$year,
                  quarter_year = x$qtr_year)

  assertthat::assert_that(all(nchar(df$family_zip) == 5))
  assertthat::assert_that(all(nchar(df$provider_zip) == 5))
  assertthat::assert_that(all(nchar(df$family_fips_code) == 5, na.rm = TRUE))

  return(df)
}

#' @title Data management ACF
#' @description Data are located: 
#' https://www.twc.texas.gov/programs/childcare#dataAndReports
#' @inheritParams child_care_db
#' @param pth String. Path to the data
#' @return data.frame
dm.acf <- function(raw_pth,
                   acf_qtr_years,
                   ...) {
  fls <- assign_acf_class(pth = raw_pth,
                          acf_qtr_years = acf_qtr_years)

  df <- lapply(fls, dm_acf) %>%
    dplyr::bind_rows()

  assertthat::assert_that(is.data.frame(df),
                          msg = "dfs is not a dataframe")
  return(df)
}

#' @title Process ACF data
#' @export
process.acf <- function(cls) {

  do.call(dwnld.acf, cls)
  do.call(dm.acf, cls)
}

                 