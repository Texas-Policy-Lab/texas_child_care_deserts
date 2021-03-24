testthat::test_that('Test county column conversion function', {

  df <- data.frame(county = c("TARRANT", "HARRIS", "EL PASO", "TOM GREEN"))

  # Test that we can pass in either a character or numeric FIPS code
  testthat::expect_is(col.county(df, county = "48439"), "data.frame")
  testthat::expect_is(col.county(df, county = 48439), "data.frame")
  testthat::expect_is(col.county(df, county = NULL), "data.frame")

  # Test that the number of rows returned is equal to 1
  testthat::expect_equal(nrow(col.county(df, county = "48439")), 1)
  testthat::expect_equal(nrow(col.county(df, county = 48439)), 1)
  testthat::expect_gte(nrow(col.county(df, county = NULL)), 4)

  # Test that passing a fake FIPS code will return 0 rows
  testthat::expect_equal(nrow(col.county(df, county = "48438")), 0)
  testthat::expect_equal(nrow(col.county(df, county = 48438)), 0)
  
  # Test that passing the county name in a different format will still return the
  # intended result
  df <- data.frame(county = c("tarrant", "harris", "el paso", "tom green"))
  testthat::expect_equal(nrow(col.county(df, county = "48439")), 1)
  testthat::expect_equal(nrow(col.county(df, county = 48439)), 1)
  testthat::expect_gte(nrow(col.county(df, county = NULL)), 4)
  
  # Test that passing the county name in a different format will still return the
  # intended result
  df <- data.frame(county = c("tarrant", "harris ", "el paso", "tom green"))
  testthat::expect_equal(nrow(col.county(df, county = "48201")), 1)
  testthat::expect_equal(nrow(col.county(df, county = 48201)), 1)
  testthat::expect_gte(nrow(col.county(df, county = NULL)), 4)

  # Test that passing county as a name will fail
  testthat::expect_error(col.county(df, county = "tarrant"))
  testthat::expect_error(col.county(df, county = "Tarrant"))
  
})
