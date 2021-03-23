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

testthat::test_that('Test county conversion function', {

  df <- data.frame(county = c("TARRANT", "HARRIS", "EL PASO", "TOM GREEN"))

  # Test that we can pass in either a character or numeric FIPS code
  testthat::expect_is(dm.county_col(df, county = "48439"), "data.frame")
  testthat::expect_is(dm.county_col(df, county = 48439), "data.frame")
  testthat::expect_is(dm.county_col(df, county = NULL), "data.frame")

  # Test that the number of rows returned is equal to 1
  testthat::expect_equal(nrow(dm.county_col(df, county = "48439")), 1)
  testthat::expect_equal(nrow(dm.county_col(df, county = 48439)), 1)
  testthat::expect_gte(nrow(dm.county_col(df, county = NULL)), 4)

  # Test that passing a fake FIPS code will return 0 rows
  testthat::expect_equal(nrow(dm.county_col(df, county = "48438")), 0)
  testthat::expect_equal(nrow(dm.county_col(df, county = 48438)), 0)
  
  # Test that passing the county name in a different format will still return the
  # intended result
  df <- data.frame(county = c("tarrant", "harris", "el paso", "tom green"))
  testthat::expect_equal(nrow(dm.county_col(df, county = "48439")), 1)
  testthat::expect_equal(nrow(dm.county_col(df, county = 48439)), 1)
  testthat::expect_gte(nrow(dm.county_col(df, county = NULL)), 4)
  
  # Test that passing the county name in a different format will still return the
  # intended result
  df <- data.frame(county = c("tarrant", "harris ", "el paso", "tom green"))
  testthat::expect_equal(nrow(dm.county_col(df, county = "48201")), 1)
  testthat::expect_equal(nrow(dm.county_col(df, county = 48201)), 1)
  testthat::expect_gte(nrow(dm.county_col(df, county = NULL)), 4)

  # Test that passing county as a name will fail
  testthat::expect_error(dm.county_col(df, county = "tarrant"))
  testthat::expect_error(dm.county_col(df, county = "Tarrant"))

})
