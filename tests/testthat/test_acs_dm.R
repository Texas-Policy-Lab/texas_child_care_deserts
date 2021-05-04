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

testthat::test_that('Test API key check function', {

  testthat::expect_error(check_census_key('blah'))
  testthat::expect_true(check_census_key('CENSUS_API_KEY'))

})

testthat::test_that('Test dm steps for B17024', {
  
  #testing that fuction fails if missing variables to create less than 200pct
  df <- data.frame(variable = "B17024_001")
  x <- list(df = df, table = "B17024")
  testthat::expect_error(dm.B17024(x))

})

testthat::test_that('Test dm steps for B23008', {
  
  # Testing that fuction fails if missing variables to create less than 200pct
  df <- data.frame(variable = "B23008_001")
  x <- list(df = df, table = "B23008")
  testthat::expect_error(dm.B23008(x))
})

testthat::test_that('Test all columns needed are returned for B23008', {

  # Testing that needed columns are returned
  columns <- c(paste("B23008", c("002","004", "005", "006",
                                 "010", "013"), sep = "_"))

  df <- data.frame(variable = columns, GEOID = rep(1,length(columns)), 
                   estimate = c(50, rep(5,length(columns)-1)))

  x <- list(df = df, table = "B23008")
  needed_cols <- c("n_kids_working_parents_lt5", "n_kids_lt5")
  testthat::expect_true(all(needed_cols %in% colnames(dm.B23008(x))))
})

testthat::test_that('Test estimates in B17024 are less than 100', {

  # Testing that number kids lt6 with working parents is less than total number 
  # kids lt6
  columns <- c(paste("B17024", c("002","003", "004", "005", "006",
                                 "007", "008", "009", "010"), sep = "_"))

  df <- data.frame(variable = columns, GEOID = rep(1,length(columns)), 
                   estimate = c(50, rep(20,length(columns)-1)))

  x <- list(df = df, table = "B17024")

  testthat::expect_error(dm.B17024(x))
})

testthat::test_that('Test pov_rate is less than or equal to 1', {
  
  # testing that pov_rate is a percentage less than or equal to 1
  
  df1 <- data.frame(variable = paste("B23008",
                                     c("004", "005", "006", "010", "013"), 
                                     sep = "_"))
  df2 <- data.frame(variable = paste("B17024", c("003", "004", "005", "006",
                                                "007", "008", "009", "010"), 
                                     sep = "_"))
  x <- list(df1, table="B23008")
  x2 <- list(df2, table="B17024")
  testthat::expect_error(dm.demand(B17024=dm.B17024(x), B23008 = dm.B23008(x2), 
                                   processed_pth = NULL, name="demand", 
                                   pov_rate=1.05))
})

