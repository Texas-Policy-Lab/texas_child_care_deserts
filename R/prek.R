#' @title Download prek
#' @description data downloaded from
#' https://rptsvr1.tea.texas.gov/adhocrpt/adste.html
#' @note School Year: 2020-2021
#' Report: Statewide Campus Totals
#' Grade, Ethnicity or Gender: Grade
#' Downloaded as CSV file
dwnld.prek <- function(raw_pth, 
                       name = "Enrollment Report_Statewide_Campuses_Grade_2020-2021.csv") {

  df <- readr::read_csv(file.path(raw_pth, name), skip = 4) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(grade %in% c("Pre-kindergarten", "Early Education")) %>%
    dplyr::mutate(earlyed_prek_enrollment = as.numeric(gsub("<", "", enrollment))) %>% 
    dplyr::group_by(year, region, `county name`, district, `district name`, campus, `campus name`, `grade group name`) %>% 
    dplyr::summarise(earlyed_prek_enrollment = sum(earlyed_prek_enrollment)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(campus_id = campus, earlyed_prek_enrollment)
}

#' @title Download ISD characteristics
#' @description https://schoolsdata2-93b5c-tea-texas.opendata.arcgis.com/datasets/TEA-Texas::current-schools2020to2021/about
dwnld.isd <- function(raw_pth,
                      name = "Current_Schools2020to2021.csv") {

  df <- readr::read_csv(file.path(raw_pth, name)) %>%
    dplyr::select(Organiza_2, Organiza_1, X, Y, Phone, Email_Addr, Match_addr) %>% 
    dplyr::rename(campus_id = Organiza_2,
                  campus_name = Organiza_1,
                  phone_number = Phone,
                  email_address = Email_Addr,
                  address = Match_addr,
                  long = X,
                  lat = Y)

  split_names <- stringr::str_split(df$campus_name, " ")
  
  df$type <- sapply(split_names, function(x) {
    
    l <- length(x)
    
    if(x[l] %in% c("S", "H")) {
      type <- paste(x[(l - 1):l], collapse = "")
    } else {
      type <- x[l]
    }
    
    return(type)
  }, simplify = TRUE)

  df <- df %>%
    dplyr::mutate(campus_name = dplyr::case_when(
      type %in% "MIDDLE" ~ gsub("MIDDLE", "Middle School", campus_name),
      type %in% c("H S", "HS", "HIGH") ~ gsub("H S|HS|HIGH", "High School", campus_name),
      type %in% c("EL", "ELEM") ~ gsub("EL|ELEM", "Elementary", campus_name),
      type %in% "INT" ~ gsub("INT", "Intermediate School", campus_name),
      type %in% c("JH", "J H") ~ gsub("JH|J H", "Junior High School", campus_name),
      type %in% "PRI" ~ gsub("PRI", "Primary School", campus_name),
      type %in% "CTR" ~ gsub("CTR", "Center", campus_name),
      type %in% "SCH" ~ gsub("SCH", "School", campus_name),
      type %in% "ED" ~ gsub("ED", "Education", campus_name),
      type %in% "ACAD" ~ gsub("ACAD", "Academy", campus_name),
      grepl("J J A E P", campus_name) ~ gsub("J J A E P", "JJAEP", campus_name),
      TRUE ~ campus_name
    ),
    campus_name = stringr::str_to_title(campus_name),
    campus_name = dplyr::case_when(
      grepl("Deap", campus_name) ~ gsub("Deap", "DEAP", campus_name),
      grepl("Jjaep", campus_name) ~ gsub("Jjaep", "JJAEP", campus_name),
      grepl("Isd", campus_name) ~ gsub("Isd", "ISD", campus_name),
      grepl("Cisd", campus_name) ~ gsub("Cisd", "CISD", campus_name),
      TRUE ~ campus_name
    )) %>% 
    dplyr::select(-type)
}

#' @title Download school ratings
#' @description data downloaded from
#' https://rptsvr1.tea.texas.gov/perfreport/account/index.html
#' @note Schools were not rated in 2020 - use rating from 2019
dwnld.rating <- function(raw_pth, 
                         name = "Multi-year Rating List 2020.xlsx") {

  df <- readxl::read_excel(file.path(raw_pth, name), sheet = "Campus Rating Label") %>%
    dplyr::rename_all(tolower) %>% 
    dplyr::select(campus_id = `campus\r\nnumber`,
                  campus_rating = `campus 2019 rating`)
}

#' @title Process Pre-K data
process.prek <- function(raw_path) {

  dwnld.isd(raw_pth = raw_pth) %>%
    dplyr::inner_join(dwnld.prek(raw_pth = raw_pth)) %>% 
    dplyr::left_join(dwnld.rating(raw_pth = raw_pth)) %>% 
    dm.geocode_lat_long() %>% 
    dplyr::mutate(county_code = substr(tract, 1, 5))

}
