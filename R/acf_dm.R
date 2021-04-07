#' @title Parse quarter year
#' @inheritParams childcare_db
parse_qtr_year <- function(raw_pth,
                           acf_qtr_years) {

  fls <- list.files(file.path(raw_pth, "acf"))

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
  assertthat::assert_that(all(test),
                          msg = paste("\nNo matching files found for the following quarter-years: ", paste(acf_qtr_years[test], collapse = ", "), 
                                      "\nYour quarter-year choices are: ", 
                                      toupper(fl_opts)))

  return(qtr_years)
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