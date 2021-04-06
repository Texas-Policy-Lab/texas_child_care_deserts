testthat::test_that('Test ACF distance calculation', {
  
  df <- data.frame(family_zip = c(77006, 78703, 75089),
                   provider_zip = c(77006, 78703, 75089))
  
  dist <- distance_between_zips(df, zip1 = family_zip, zip2 = provider_zip)
  
  testthat::expect_true(all(dist$distance == 0))
  
  df <- data.frame(family_zip = c(78703, 75089, 77006),
                   provider_zip = c(77006, 78703, 75089))
  
  dist <- distance_between_zips(df, zip1 = family_zip, zip2 = provider_zip)
  
  testthat::expect_true(all(dist$distance != 0))
  
})
