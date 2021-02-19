testthat::test_that('Test that kinder data is unique', {
  
  df <- get.kinder_neighborhood_tract_xwalk()

  testthat::expect_equal(length(unique(df$anchor_tract)), nrow(df))
  
})
