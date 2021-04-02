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

testthat::test_that('Test dm steps for B17024', {
  
#testing that fuction fails if missing variables to create less than 200pct
df <- data.frame(variable = "B17024_001")
x = list(df=df, table="B17024")
  testthat::expect_error(dm.B17024(x))
})

testthat::test_that('Test dm steps for B23008', {
  
  #testing that fuction fails if missing variables to create less than 200pct
  df <- data.frame(variable = "B23008_001")
  x = list(df=df, table="B23008")
  testthat::expect_error(dm.B23008(x))
})

testthat::test_that('Test all columns needed are returned for B23008', {
  
  #testing that needed columns are returned
  tbls <- list(B23008 = list(year = 2019, state = 48,
                             geography = "tract", county = 439))
  
  x <- dwnld.acs(tbls,pth = NULL)
  needed_cols <- c("n_kids_working_parents_lt5", "n_kids_lt5")
  testthat::expect_true(needed_cols %in% colnames(dm.B23008(x)))
})

testthat::test_that('Test all columns needed are returned for B17024', {
  
  #testing that needed columns are returned
  tbls <- list(B17024 = list(year = 2019, state = 48,
                             geography = "tract", county = 439))
  
  x <- dwnld.acs(tbls, pth = NULL)
  needed_cols <- c("pct_kids_lt5_under200_pct", "n_kids_lt5")
  testthat::expect_true(needed_cols %in% colnames(dm.B17024(x)))
})

testthat::test_that('Test pov_rate is less than or equal to 1' {
  
    #testing that pov_rate is a percentage less than or equal to 1
  tbls <- list(B17024 = list(year = 2019, state = 48,
                             geography = "tract", county = 439))
  tbls2 <- list(B23008 = list(year = 2019, state = 48,
                              geography = "tract", county = 439))
  
  x = dwnld.acs(tbls, pth = NULL)
  x2 = dwnld.acs(tbls2, pth=NULL)
  testthat::expect_error(dm.demand(B17024=dm.B17024(x), B23008 = dm.B23008(x2), 
                                   processed_pth = NULL, name="demand", pov_rate=1.05))
})

