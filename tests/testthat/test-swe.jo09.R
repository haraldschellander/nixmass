test_that("swe.jo09 works with valid input", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     result <- swe.jo09(data, alt = 1500, region.jo09 = 3)
     
     expect_type(result, "double")
     expect_length(result, nrow(data))
     expect_true(all(result >= 0, na.rm = TRUE)) # SWE should be non-negative
})

test_that("swe.jo09 throws error for invalid data type", {
     data <- list(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     expect_error(swe.jo09(data, alt = 1500, region.jo09 = 3), 
                  "swe.jo09: data must be given as data.frame")
})

test_that("swe.jo09 throws error when required columns are missing", {
     data <- data.frame(hs = seq(0, 0.5, length.out = 10))
     expect_error(swe.jo09(data, alt = 1500, region.jo09 = 3), 
                  "swe.jo09: data must contain at least two columns named 'hs' and 'date'")
})

test_that("swe.jo09 throws error for invalid snow depth values", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = c(0.1, -0.2, 0.3, NA, 0.5)
     )
     expect_error(swe.jo09(data, alt = 1500, region.jo09 = 3), 
                  "swe.jo09: snow depth data must not be NA")
     
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = c(0.1, -0.2, 0.3, 0.1, 0.5)
     )
     expect_error(swe.jo09(data, alt = 1500, region.jo09 = 3), 
                  "swe.jo09: snow depth data must not be negative")
})

test_that("swe.jo09 throws error for invalid date format", {
     data <- data.frame(
          date = c("2023-01-01", "invalid-date", "2023-01-03"),
          hs = seq(0, 0.5, length.out = 3)
     )
     expect_error(swe.jo09(data, alt = 1500, region.jo09 = 3), 
                  "date format must be '%Y-%m-%d'")
})

test_that("swe.jo09 throws error for invalid altitude values", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     
     expect_error(swe.jo09(data, alt = -100, region.jo09 = 3), 
                  "swe.jo09: station elevation must not be negative")
     
     expect_error(swe.jo09(data, alt = NA, region.jo09 = 3), 
                  "swe.jo09: station elevation must be given")
})

test_that("swe.jo09 throws error for invalid region.jo09 values", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     
     expect_error(swe.jo09(data, alt = 1500, region.jo09 = 8), 
                  "swe.jo09: region.jo09 must be integer between 1 and 7")
     
     expect_error(swe.jo09(data, alt = 1500, region.jo09 = c(1, 2)), 
                  "region.jo09 must be of length one")
})

test_that("swe.jo09 handles zero snow depth correctly", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:4,
          hs = c(0, 0, 0, 0, 0)
     )
     result <- swe.jo09(data, alt = 1500, region.jo09 = 3)
     
     expect_equal(result, rep(0, nrow(data))) # SWE should be zero when snow depth is zero
})

# test_that("swe.jo09 computes correct SWE for different altitude classes", {
#      data <- data.frame(
#           date = as.Date('2023-01-01') + 0:4,
#           hs = seq(0.1, 0.5, length.out = 5)
#      )
#      
#      result_0 <- swe.jo09(data, alt = 1000, region.jo09 = 3)
#      result_1400 <- swe.jo09(data, alt = 1500, region.jo09 = 3)
#      result_2000 <- swe.jo09(data, alt = 2500, region.jo09 = 3)
#      
#      expect_true(all(result_0 <= result_1400, na.rm = TRUE)) # Higher altitude => Higher SWE
#      expect_true(all(result_1400 <= result_2000, na.rm = TRUE))
# })

test_that("swe.jo09 handles months with no valid coefficients (e.g., summer months)", {
     data <- data.frame(
          date = as.Date(c('2023-06-01', '2023-07-01', '2023-08-01')),
          hs = c(0.1, 0.2, 0.3)
     )
     result <- swe.jo09(data, alt = 1500, region.jo09 = 3)
     
     expect_true(all(is.na(result))) # Should return NA for months without valid coefficients
})



