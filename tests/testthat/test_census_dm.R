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

  tbls <- list(B23008 = list(year = 2019, state = 48,
                             geography = "tract", county = 439))

  testthat::expect_true(all(unlist(dwnld.acs(tbls,
                                             pth = NULL))))
})

testthat::test_that('Test API key check function', {

  testthat::expect_error(check_census_key('blah'))
  testthat::expect_true(check_census_key('CENSUS_API_KEY'))

})