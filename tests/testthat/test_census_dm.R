testthat::test_that('Test test_attr fails correctly', {
  
  attr <- list()
  testthat::expect_error(test_attr(attr))
  
  attr <- list(variables = "blah",
               geography = "blah")
  testthat::expect_true(test_attr(attr))
  
  attr <- list(table = "blah",
               geography = "blah")
  testthat::expect_true(test_attr(attr))
  
  attr <- list(table = "blah",
               variables = "blah_01",
               geography = "blah")
  testthat::expect_true(test_attr(attr))
  
  attr <- list(table = "blah",
               variables = "not_blah_01",
               geography = "blah")
  testthat::expect_error(test_attr(attr))
})

testthat::test_that('Test that ACS tables download correctly', {
  
  dwnld.acs(tbls,
            pth)
  
  
})