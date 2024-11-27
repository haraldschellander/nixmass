test_that("swe.pi16 works with valid input", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     result <- swe.pi16(data, rho_0 = 200, K = 1)
     
     expect_type(result, "double")
     expect_length(result, nrow(data))
     expect_true(all(result >= 0)) # SWE should be non-negative
})

test_that("swe.pi16 throws error for invalid data type", {
     data <- list(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     expect_error(swe.pi16(data, rho_0 = 200, K = 1), 
                  "swe.pi16: data must be given as data.frame")
})

test_that("swe.pi16 throws error when required columns are missing", {
     data <- data.frame(hs = seq(0, 0.5, length.out = 10))
     expect_error(swe.pi16(data, rho_0 = 200, K = 1), 
                  "swe.pi16: data must contain at least two columns named 'hs' and 'date'")
})

test_that("swe.pi16 throws error for invalid snow depth values", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = c(0.1, -0.2, 0.3, NA, 0.5)
     )
     expect_error(swe.pi16(data, rho_0 = 200, K = 1), 
                  "swe.pi16: snow depth data must not be NA")
     
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = c(0.1, -0.2, 0.3, 0.1, 0.5)
     )
     expect_error(swe.pi16(data, rho_0 = 200, K = 1), 
                  "swe.pi16: snow depth data must not be negative")
})

test_that("swe.pi16 throws error for invalid date format", {
     data <- data.frame(
          date = c("2023-01-01", "invalid-date", "2023-01-03"),
          hs = seq(0, 0.5, length.out = 3)
     )
     expect_error(swe.pi16(data, rho_0 = 200, K = 1), 
                  "date format must be '%Y-%m-%d'")
})

# test_that("swe.pi16 computes correct SWE with default parameters", {
#      data <- data.frame(
#           date = as.Date('2023-01-01') + 0:9,
#           hs = seq(0, 0.5, length.out = 10)
#      )
#      result <- swe.pi16(data) # Use default rho_0 = 200 and K = 1
#      
#      rho_0 <- 200
#      K <- 1
#      doy <- as.POSIXlt(data$date)$yday + 1 - 122 # Day of year adjustment
#      expected_swe <- (rho_0 + K * (doy + 61)) * data$hs
#      
#      expect_equal(result, expected_swe)
# })

# test_that("swe.pi16 computes correct SWE with custom rho_0 and K", {
#      data <- data.frame(
#           date = as.Date('2023-01-01') + 0:9,
#           hs = seq(0, 0.5, length.out = 10)
#      )
#      rho_0 <- 250
#      K <- 0.8
#      result <- swe.pi16(data, rho_0 = rho_0, K = K)
#      
#      doy <- as.POSIXlt(data$date)$yday + 1 - 122 # Day of year adjustment
#      expected_swe <- (rho_0 + K * (doy + 61)) * data$hs
#      
#      expect_equal(result, expected_swe)
# })

test_that("swe.pi16 handles zero snow depth", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:4,
          hs = c(0, 0, 0, 0, 0)
     )
     result <- swe.pi16(data)
     
     expect_equal(result, rep(0, nrow(data))) # SWE should be zero when snow depth is zero
})

# test_that("swe.pi16 correctly computes SWE for summer dates", {
#      data <- data.frame(
#           date = as.Date(c('2023-07-01', '2023-08-01', '2023-09-01')),
#           hs = c(0.1, 0.2, 0.3)
#      )
#      result <- swe.pi16(data, rho_0 = 200, K = 1)
#      
#      # SWE should still compute but with shifted DOY
#      doy <- as.POSIXlt(data$date)$yday + 1 - 122
#      expected_swe <- (200 + 1 * (doy + 61)) * data$hs
#      expect_equal(result, expected_swe)
# })


