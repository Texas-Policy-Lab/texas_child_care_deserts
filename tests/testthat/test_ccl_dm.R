testthat::test_that('Test operation number column data management', {
  
  df <- data.frame(operation_number = c("1","1","2","3"))
  testthat::expect_error(col.operation_number(df = df))
  
  df <- data.frame(operation_number = c("1-1","1-2","2","3"))
  testthat::expect_error(col.operation_number(df = df))
  
  df <- data.frame(operation_number = c("1-1","2","3"))
  testthat::expect_equal(nrow(col.operation_number(df = df)), 3)
  
  df <- data.frame(operation_number = c("1-1","2","3"))
  testthat::expect_true(
    all(nchar(col.operation_number(df = df)$operation_number)) == 1
  )
  
  df <- data.frame(operation_number = c("1-1", "2","3", NA))
  testthat::expect_error(col.operation_number(df = df))

})

testthat::test_that('Test county column conversion function', {

  df <- data.frame(county = c("TARRANT", "HARRIS", "EL PASO", "TOM GREEN"))

  testthat::expect_is(col.county(df), "data.frame")

  # Test that the number of rows returned is equal to 1
  testthat::expect_gte(nrow(col.county(df)), 4)

  # Test that passing the county name in a different format will still return the
  # intended result
  df <- data.frame(county = c("tarrant", "harris", "el paso", "tom green"))
  testthat::expect_gte(nrow(col.county(df)), 4)
  
  # Test that passing the county name in a different format will still return the
  # intended result
  df <- data.frame(county = c("tarrant", "harris ", "el paso", "tom green"))
  testthat::expect_gte(nrow(col.county(df)), 4)

})

testthat::test_that('Test data management col.accepts_child_care_subsidies', {

  df <- data.frame(accepts_child_care_subsidies = c("Y", "N", "Y"))
  testthat::expect_true(all(col.accepts_child_care_subsidies(df)$subsidy %in% 
                              c(TRUE, FALSE)))

  df <- data.frame(accepts_child_care_subsidies = c("Y", "N", NA))
  testthat::expect_true(all(col.accepts_child_care_subsidies(df)$subsidy %in% 
                              c(TRUE, FALSE, NA)))
})

testthat::test_that('Test data management col.total_capacity', {
  
  df <- data.frame(total_capacity = c(1,2,3,4))
  testthat::expect_true(all(is.numeric(col.total_capacity(df)$licensed_capacity)))
  
  df <- data.frame(total_capacity = c("1", "2", "3", "4"))
  testthat::expect_error(col.total_capacity(df)$licensed_capacity)
  
  df <- data.frame(total_capacity = c(1,2,3,4, NA))
  testthat::expect_true(all(is.numeric(col.total_capacity(df)$licensed_capacity)))

})
