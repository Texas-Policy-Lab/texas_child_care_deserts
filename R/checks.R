#' @title Checks for data quality
check <- function() UseMethod("check")

#' @title Checks the values match what is expected
#' @param var vector. List of values to check.
#' @param values vector. List of values to check against.
#' @param var_name string. Name of the variable to include in message if assert 
#' fails.
#' @param data_name string. Name of the data to include in message if assert 
#' fails.
check.values <- function(var,
                         values,
                         var_name, 
                         data_name,
                         msg = "Values other than {v_collapse} in {var_name} in {data_name} data") {

  v_collapse <- paste(values, collapse = ", ")

  assertthat::assert_that(all(unique(var) %in% values),
                          msg = glue::glue(msg))
}

#' @title Checks there are no NAs
check.no_na <- function(var, 
                        var_name,
                        data_name,
                        msg = "Missing values in {var_name} variable in {data_name} data") {
  assertthat::assert_that(sum(is.na(var)) == 0,
                          msg = msg) 
}
