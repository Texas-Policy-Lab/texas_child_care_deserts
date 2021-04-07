#' @title Select quarter year
#' @description Parses the quarter-year parameter and returns the file names
#' which are associated with that quarter year combination
#' @inheritParams read_qtr_year
#' @return A vector of file paths associated with the given quarter year
select_qtr_year <- function(pth,
                            acf_qtr_years) {

  pth <- file.path(pth, "acf")
  fls <- list.files(pth)

  assertthat::assert_that(length(fls) >=1, 
                          msg = paste("The path to the data", pth, "is empty. Did you run the necessary download steps to create the child care data base file structure?"))

  qtr_years <- sapply(acf_qtr_years, function(qtr_year) {

    qy <- fls[grepl(qtr_year, toupper(fls))]

    if (length(qy) == 0) {
      return(NULL)
    } else {
      return(qy)
    }

  }, USE.NAMES = F, simplify = TRUE)

  fl_opts <- gsub("acf-801-", "", fls)
  fl_opts <- gsub("-twc.xlsx|-twc%20.xlsx", "", fl_opts)
  fl_opts <- paste0("\n", paste(fl_opts, collapse = "\n"))

  test <- sapply(qtr_years, is.null, simplify = T)
  assertthat::assert_that(!all(test),
                          msg = paste("\nNo matching files found for the following quarter-years: ", paste(acf_qtr_years[test], collapse = ", "), 
                                      "\nYour quarter-year choices are: ", 
                                      toupper(fl_opts)))

  return(file.path(pth, qtr_years))
}


#' @title Read older ACS dataframes from 2018
#' @description This function reads in the sheet 'ChildrenParentsSettings'
read_acf.cps <- function(pth,
                         sheet = "ChildrenParentsSettings") {
 
  readxl::read_excel(pth, sheet = sheet)
   
}

#' @title Read newer ACS dataframes from 2019 and 2020
#' @description This function reads in the sheet 'CCSettings'
read_acf.ccs <- function(pth,
                         sheet = "CCSettings") {

  readxl::read_excel(pth, sheet = sheet)
  
}

#' @title Assigns a class to ACF data
#' @description Assigns a class to all the incoming files which will perform different
#' data management steps according to which file type it is
assign_acf_class <- function(fls) {

  lapply(fls, function(fl) {
    sheets <- readxl::excel_sheets(fl)

    cls <- c()

    if("ChildrenParentsSettings" %in% sheets) {
      cls <- c(cls, "cps")
    } else if ("CCSettings" %in% sheets) {
      cls <- c(cls, "ccp")
    } else {
      cls <- NULL
    }

    assertthat::assert_that(length(cls) == 1,
                            msg = "ACF data format has changed")

    structure(list(fl = fl), class = cls)
  })
}

#' @title Reads in the data from the user selected quarter-year
#' @inheritParams childcare_db
#' @param pth String. Path to the data
read_qtr_year <- function(pth,
                          acf_qtr_years) {

  fls <- select_qtr_year(pth = pth,
                         acf_qtr_years = acf_qtr_years)

  assertthat::assert_that(all(tools::file_ext(fls) %in% c("xlsx", "xls")),
                          msg = "ACF files are not in the expected format of .xlsx or .xls")

  fls <- assign_acf_class(fls)
  
   
}


#' @title Data management ACF
#' @description Data are located: 
#' https://www.twc.texas.gov/programs/childcare#dataAndReports
#' @return data.frame
dm.acf <- function(raw_pth) {

  fls <- list.files(file.path(raw_pth, "acf"))

}

#' @title Process ACF data
process.acf <- function(acf) {

  acf <- do.call(dwnld.acf, acf)
  do.call(dm.acf, acf)

}

#' @title 
dm.mkt_subsidy <- function(tracts,
                           tract_provider_xwalk) {

  q1 <- readxl::read_excel(pth1, sheet = "ChildcareParentSettings")
  q1 <- readxl::read_excel(pth2, sheet = "ChildcareParentSettings")
  q1 <- readxl::read_excel(pth3, sheet = "ChildcareParentSettings")

  provider_kids <- dplyr::bind_rows("q1"= q1, "q2"= q2, "q3"= q4, .id="quarter") %>% 
    dplyr::mutate("operation_number" = as.character(CCSettings.ProviderStateID)) %>% 
    dplyr::select(ChildrenID, quarter, operation_number) %>% 
    dplyr::group_by(quarter, operation_number) %>% 
    dplyr::summarise(n_kids = dplyr::n_distinct(ChildrenID)) %>% 
    tidyr::pivot_wider(names_from = quarter, values_from = n_kids, values_fill = 0) %>% 
    tidyr::pivot_longer(-operation_number) %>% 
    dplyr::group_by(operation_number) %>% 
    dplyr::summarise(max_n_kids=max(value),
                     med_n_kids = median(value),
                     min_n_kids=min(value)) %>% 
    dplyr::left_join(dplyr::select(ccp,operation_number, total_capacity, ccl_accepts_subsidy), by = "operation_number") %>% 
    dplyr::filter(ccl_accepts_subsity == T) %>% 
    dplyr::inner_join(tract_provider_xwalk %>% 
                        dplyr::mutate(operation_number= as.character(operation_number)), by= "operation_number")
  
  provider_kids<-provider_kids %>% 
    dplyr::select(-operation_number) %>% 
    dplyr::summarise(max_ratio = max_n_kids/total_capacity,
                     med_ratio = med_n_kids/total_capacity,
                     max_ratio = min_n_kids/total_capacity) %>% 
    # replace ratios over 1 with 1
    dplyr::mutate_at(vars(max_ratio, med_ratio, min_ratio), 
                     list(~ ifelse(.>=1,1,.))) %>% 
    #drop markets that have no providers and therefore na for the ratios
    tidyr::drop_na()
  
  mom_params <- mkt_enrollment_ratios %>% 
    tidyr::pivot_longer(-anchor_tract) %>% 
    dplyr::group_by(anchor_tract) %>% 
    #raw moments
    dplyr::summarise(mu_1=mean(value)) %>% 
    # aggregate moments across markets
    dplyr::summarise(mu_hat_1 = mean(mu_1))
  
  tri_params <-market_enrollment_ratios %>% 
    dplyr::summarise(a=mean(min_ratio),
                     b=mean(max_ratio)) %>% 
    dplyr::bind_cols(mom_params) %>% 
    dplyr::mutate(c=3*mu_hat_1 - a - b)
  
  return(tri_params$b)
}