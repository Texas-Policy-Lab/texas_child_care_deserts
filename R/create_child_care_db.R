#' @title Create child care data base
#' @param root string. Path to the root directory to create the DB.
#' @examples
#' \dontrun{
#' census_tbls <- list(B23008 = list(year = 2019, state = 48, 
#'                     geography = "tract", county = 439))
#' root <- "C:/"
#' childcare_db(census_tbls = census_tbls, root = root)
#' }
childcare_db <- function(root) {

  data_pth <- file.path(root, "data")
  raw_pth <- file.path(data_pth, "raw")
  processed_pth <- file.path(data_pth, "processed")

  pths <- c(data_pth, raw_pth, processed_pth)

  create_folder_str(pths = pths)

  # if (!is.null(census_tbls)) {
  #   dwnld.acs(tbls = census_tbls,
  #             pth = raw_pth)
  # }
  # 
  # hhsc_ccl <- list()
  # hhsc_ccl$raw_pth <- raw_pth
  # hhsc_ccl$processed_pth <- processed_pth
  # hhsc_ccl$name <- "HHSC_CCL"

  # process.hhsc_ccl(hhsc_ccl = hhsc_ccl)

  acf <- list()
  acf$raw_pth <- raw_pth
  acf$processed_pth <- processed_pth

  process.acf(acf = acf)
}


dm.mkt_subsidy <- function(pth1,
                           pth2,
                           pth3,
                           tracts,
                           tract_provider_xwalk){
  q1<- readxl::read_excel(pth1, sheet = "ChildcareParentSettings")
  q1<- readxl::read_excel(pth2, sheet = "ChildcareParentSettings")
  q1<- readxl::read_excel(pth3, sheet = "ChildcareParentSettings")
  
  provider_kids <-dplyr::bind_rows("q1"= q1, "q2"= q2, "q3"= q4, .id="quarter") %>% 
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
    tidyr::pivot_longr(-anchor_tract) %>% 
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