#' @title Test and select input
#' @description Tests that the expected columns exist, and then selects those
#' columns
#' @param df data.frame
#' @param input_columns. List. List of variable names to select with expected
#' variable types.
#' @return data.frame
test_input <- function(df, input_columns) {
  
  assertthat::assert_that(all(c(names(input_columns) %in% colnames(df))))

  assertthat::assert_that(all(
    sapply(names(input_columns), function(col) {
    cls <- input_columns[[col]]
    test_col <- structure(list(v = df[[col]]), 
                          class = cls)
    do.call(check_type, test_col)
    })
  ))

  df %>%
    dplyr::select(dplyr::one_of(names(input_columns)))
}

#' @title Tests the vector or column type
#' @param v. Vector. A vector to test.
#' @param msg. String. A message to return if the check fails.
check_type <- function(v, msg = NULL) UseMethod("check_type")

#' @title Test for character (string) column or vector
#' @inheritParams check_type
check_type.character <- function(v, 
                                 msg = "Not a character (string) vector") {

  assertthat::assert_that(class(v) == "character",
                          msg = msg)
}

#' @title Test for numeric column or vector
#' @inheritParams check_type
check_type.numeric <- function(v, 
                               msg = "Not a numeric vector") {

  assertthat::assert_that(class(v) == "numeric",
                          msg = msg)
}

#' @title Test for integer column or vector
#' @inheritParams check_type
check_type.integer <- function(v, 
                               msg = "Not an integer vector") {
  
  assertthat::assert_that(class(v) == "integer",
                          msg = msg)
}

#' @title Test for date column or vector
#' @inheritParams check_type
check_type.Date <- function(v, 
                            msg = "Not a date vector") {

  assertthat::assert_that(class(v) == "Date",
                          msg = msg)
}

#' @title Test for POSIXct date column or vector
#' @inheritParams check_type
check_type.POSIXct <- function(v) {
  
  assertthat::assert_that(all(class(v) == c("POSIXct", "POSIXt")),
                          msg = "Not a POSIXct vector")
}

#' @title Test for boolean (logical) column or vector
#' @inheritParams check_type
check_type.boolean <- function(v,
                               msg = "Not a boolean (logical) vector") {
  
  assertthat::assert_that(class(v) == "logical",
                          msg = msg)
}
