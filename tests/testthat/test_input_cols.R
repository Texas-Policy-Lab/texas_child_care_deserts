testthat::test_that('Tests character column function'{

  # Test character values
  testthat::expect_true(test_type.character(v = "dsfsdlkj"))
  testthat::expect_true(test_type.character(v = c("sf", "dsfsdlkj")))
  testthat::expect_true(test_type.character(v = c("dfalksj", NA)))
  testthat::expect_true(test_type.character(v = c("dfalksj", NULL)))
  
  # Test numeric values
  testthat::expect_error(test_type.character(v = c(1,2,4)))
  testthat::expect_error(test_type.character(v = 1))
  testthat::expect_error(test_type.character(v = c(1, NA)))
  testthat::expect_error(test_type.character(v = c(1.1, 1.3)))

  # Test Boolean values
  testthat::expect_error(test_type.character(v = c(TRUE, FALSE)))
  testthat::expect_error(test_type.character(v = TRUE))
  
  # Test Date values
  testthat::expect_error(test_type.character(v = as.Date("2021-03-23")))
  
})
  