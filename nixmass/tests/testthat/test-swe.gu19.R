test_that("swe.gu19 works with valid input", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     result <- swe.gu19(data, region.gu19 = "central")
     
     expect_type(result, "double")
     expect_length(result, nrow(data))
     expect_true(all(result >= 0, na.rm = TRUE)) # SWE should be non-negative
})

test_that("swe.gu19 throws error for invalid data type", {
     data <- list(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     expect_error(swe.gu19(data, region.gu19 = "central"), 
                  "swe.gu19: data must be given as data.frame")
})

test_that("swe.gu19 throws error when required columns are missing", {
     data <- data.frame(hs = seq(0, 0.5, length.out = 10))
     expect_error(swe.gu19(data, region.gu19 = "central"), 
                  "swe.gu19: data must contain at least two columns named 'hs' and 'date'")
})

test_that("swe.gu19 throws error for invalid snow depth values", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = c(0.1, -0.2, 0.3, NA, 0.5)
     )
     expect_error(swe.gu19(data, region.gu19 = "central"), 
                  "swe.gu19: snow depth data must not be NA")
     
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = c(0.1, -0.2, 0.3, 0.1, 0.5)
     )
     expect_error(swe.gu19(data, region.gu19 = "central"), 
                  "swe.gu19: snow depth data must not be negative")
})

test_that("swe.gu19 throws error for invalid date format", {
     data <- data.frame(
          date = c("2023-01-01", "invalid-date", "2023-01-03"),
          hs = seq(0, 0.5, length.out = 3)
     )
     expect_error(swe.gu19(data, region.gu19 = "central"), 
                  "date format must be '%Y-%m-%d'")
})

test_that("swe.gu19 throws error for invalid region", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     
     expect_error(swe.gu19(data, region.gu19 = "invalid_region"), 
                  "swe.gu19: region.gu19 must be one of 'italy','southwest','central','southeast','myregion'")
})

test_that("swe.gu19 throws error for missing coefficients in 'myregion'", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     
     expect_error(swe.gu19(data, region.gu19 = "myregion"), 
                  "swe.gu19: at least one of the coefficients n0, n1, n2 is NULL")
})

test_that("swe.gu19 works with valid input for 'myregion'", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     result <- swe.gu19(data, region.gu19 = "myregion", n0 = 300, n1 = -0.8, n2 = 0.01)
     
     expect_type(result, "double")
     expect_length(result, nrow(data))
     expect_true(all(result >= 0, na.rm = TRUE))
})

test_that("swe.gu19 handles zero snow depth correctly", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:4,
          hs = c(0, 0, 0, 0, 0)
     )
     result <- swe.gu19(data, region.gu19 = "central")
     
     expect_equal(result, rep(0, nrow(data))) # SWE should be zero when snow depth is zero
})

test_that("swe.gu19 computes correct SWE for different regions", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:4,
          hs = seq(0.1, 0.5, length.out = 5)
     )
     
     result_italy <- swe.gu19(data, region.gu19 = "italy")
     result_southwest <- swe.gu19(data, region.gu19 = "southwest")
     result_central <- swe.gu19(data, region.gu19 = "central")
     
     expect_true(all(!is.na(result_italy)))
     expect_true(all(!is.na(result_southwest)))
     expect_true(all(!is.na(result_central)))
})

test_that("swe.gu19 handles unsupported months (summer months) correctly", {
     data <- data.frame(
          date = as.Date(c('2023-07-01', '2023-08-01', '2023-09-01')),
          hs = c(0.1, 0.2, 0.3)
     )
     result <- swe.gu19(data, region.gu19 = "central")
     
     expect_true(all(is.na(result))) # Should return NA for unsupported months
})



