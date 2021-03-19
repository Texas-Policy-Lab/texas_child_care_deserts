testthat::test_that('test ccl data cleaning', {
  
  df <- data.frame("operation_number" = c(1, 2, 2))
  testthat::expect_error(test.ccl_dm(df))
  
  df <- data.frame("operation_number" = c(1, 2, 3),
                   "infant" = c(0, 1, "string"))
  testthat::expect_error(test.ccl_dm(df))
  
  df <- data.frame("operation_number" = c(1, 2, 3),
                   "infant" = c(0, 1, 0),
                   "licensed_capacity" = c(10, NA, "string"))
  testthat::expect_error(test.ccl_dm(df))
  
  df <- data.frame("operation_number" = c(1, 2, 3),
                   "infant" = c(0, 1, 0),
                   "licensed_capacity" = c(10, NA, 3),
                   "center_prvdr" = c(0,1,NA))
  testthat::expect_error(test.ccl_dm(df))
  
  df <- data.frame("operation_number" = c(1, 2, 3),
                   "infant" = c(0, 1, 0),
                   "licensed_capacity" = c(10, NA, 3),
                   "center_prvdr" = c(0,1,1),
                   "after_school" = c(0,1,"string"))
  testthat::expect_error(test.ccl_dm(df))
  
  df <- data.frame("operation_number" = c(1, 2, 3),
                   "infant" = c(0, 1, 0),
                   "licensed_capacity" = c(10, NA, 3),
                   "center_prvdr" = c(0,1,1),
                   "after_school" = c(0,1,1))
  testthat::expect_true(test.ccl_dm(df))
  
  
})
