testthat::test_that('Tests character column function', {

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
  testthat::expect_error(test_type.character(v = c(as.Date("2021-03-22"),
                                                   as.Date("2021-03-23"))))

})

testthat::test_that('Tests numeric column function', {

  # Test character values
  testthat::expect_error(test_type.numeric(v = "dsfsdlkj"))
  testthat::expect_error(test_type.numeric(v = c("sf", "dsfsdlkj")))
  testthat::expect_error(test_type.numeric(v = c("dfalksj", NA)))
  testthat::expect_error(test_type.numeric(v = c("dfalksj", NULL)))

  # Test numeric values
  testthat::expect_true(test_type.numeric(v = c(1,2,4)))
  testthat::expect_true(test_type.numeric(v = 1))
  testthat::expect_true(test_type.numeric(v = c(1, NA)))
  testthat::expect_true(test_type.numeric(v = c(1.1, 1.3)))

  # Test Boolean values
  testthat::expect_error(test_type.numeric(v = c(TRUE, FALSE)))
  testthat::expect_error(test_type.numeric(v = TRUE))

  # Test Date values
  testthat::expect_error(test_type.numeric(v = as.Date("2021-03-23")))
  testthat::expect_error(test_type.numeric(v = c(as.Date("2021-03-22"),
                                                 as.Date("2021-03-23"))))

})

testthat::test_that('Tests date column function', {
  
  # Test character values
  testthat::expect_error(test_type.date(v = "dsfsdlkj"))
  testthat::expect_error(test_type.date(v = c("sf", "dsfsdlkj")))
  testthat::expect_error(test_type.date(v = c("dfalksj", NA)))
  testthat::expect_error(test_type.date(v = c("dfalksj", NULL)))
  
  # Test numeric values
  testthat::expect_error(test_type.date(v = c(1,2,4)))
  testthat::expect_error(test_type.date(v = 1))
  testthat::expect_error(test_type.date(v = c(1, NA)))
  testthat::expect_error(test_type.date(v = c(1.1, 1.3)))
  
  # Test Boolean values
  testthat::expect_error(test_type.date(v = c(TRUE, FALSE)))
  testthat::expect_error(test_type.date(v = TRUE))
  
  # Test Date values
  testthat::expect_true(test_type.date(v = as.Date("2021-03-23")))
  testthat::expect_true(test_type.date(v = c(as.Date("2021-03-22"),
                                                 as.Date("2021-03-23"))))
  
})

testthat::test_that('Tests date column function', {
  
  # Test character values
  testthat::expect_error(test_type.boolean(v = "dsfsdlkj"))
  testthat::expect_error(test_type.boolean(v = c("sf", "dsfsdlkj")))
  testthat::expect_error(test_type.boolean(v = c("dfalksj", NA)))
  testthat::expect_error(test_type.boolean(v = c("dfalksj", NULL)))
  
  # Test numeric values
  testthat::expect_error(test_type.boolean(v = c(1,2,4)))
  testthat::expect_error(test_type.boolean(v = 1))
  testthat::expect_error(test_type.boolean(v = c(1, NA)))
  testthat::expect_error(test_type.boolean(v = c(1.1, 1.3)))
  
  # Test Boolean values
  testthat::expect_true(test_type.boolean(v = c(TRUE, FALSE)))
  testthat::expect_true(test_type.boolean(v = TRUE))
  
  # Test Date values
  testthat::expect_error(test_type.boolean(v = as.Date("2021-03-23")))
  testthat::expect_error(test_type.boolean(v = c(as.Date("2021-03-22"),
                                             as.Date("2021-03-23"))))
  
})
