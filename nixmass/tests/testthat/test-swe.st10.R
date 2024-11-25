test_that("swe.st10 works with valid input for 'alpine'", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     result <- swe.st10(data, snowclass.st10 = "alpine")
     
     expect_type(result, "double")
     expect_length(result, nrow(data))
     expect_true(all(result >= 0)) # SWE should be non-negative
})

test_that("swe.st10 throws error for invalid data type", {
     data <- list(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     expect_error(swe.st10(data, snowclass.st10 = "alpine"), 
                  "swe.st10: data must be given as data.frame")
})

test_that("swe.st10 throws error when required columns are missing", {
     data <- data.frame(hs = seq(0, 0.5, length.out = 10))
     expect_error(swe.st10(data, snowclass.st10 = "alpine"), 
                  "swe.st10: data must contain at least two columns named 'hs' and 'date'")
})

test_that("swe.st10 throws error for invalid snow depth values", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = c(0.1, -0.2, 0.3, NA, 0.5)
     )
     expect_error(swe.st10(data, snowclass.st10 = "alpine"), 
                  "swe.st10: snow depth data must not be NA")
     
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = c(0.1, -0.2, 0.3, 0.3, 0.5)
     )
     expect_error(swe.st10(data, snowclass.st10 = "alpine"), 
                  "swe.st10: snow depth data must not be negative")
})

test_that("swe.st10 throws error for invalid date format", {
     data <- data.frame(
          date = c("2023-01-01", "invalid-date", "2023-01-03"),
          hs = seq(0, 0.5, length.out = 3)
     )
     expect_error(swe.st10(data, snowclass.st10 = "alpine"), 
                  "date format must be '%Y-%m-%d'")
})

# test_that("swe.st10 throws error for invalid snowclass.st10", {
#      data <- data.frame(
#           date = as.Date('2023-01-01') + 0:9,
#           hs = seq(0, 0.5, length.out = 10)
#      )
#      expect_error(swe.st10(data, snowclass.st10 = "invalid_class"), 
#                   "'arg' should be one of \"alpine\",\"maritime\",\"prairie\",\"tundra\",\"taiga\"")
# })

test_that("swe.st10 computes correctly for different snow classes", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:9,
          hs = seq(0, 0.5, length.out = 10)
     )
     
     for (class in c("alpine", "maritime", "prairie", "tundra", "taiga")) {
          result <- swe.st10(data, snowclass.st10 = class)
          expect_type(result, "double")
          expect_length(result, nrow(data))
          expect_true(all(result >= 0)) # SWE should be non-negative
     }
})

test_that("swe.st10 handles zero snow depth", {
     data <- data.frame(
          date = as.Date('2023-01-01') + 0:4,
          hs = c(0, 0, 0, 0, 0)
     )
     result <- swe.st10(data, snowclass.st10 = "alpine")
     
     expect_equal(result, rep(0, nrow(data))) # SWE should be zero when snow depth is zero
})

test_that("swe.st10 returns NA for summer dates", {
     data <- data.frame(
          date = as.Date(c('2023-07-01', '2023-08-01', '2023-09-01')),
          hs = c(0.1, 0.2, 0.3)
     )
     result <- swe.st10(data, snowclass.st10 = "alpine")
     
     expect_true(all(is.na(result))) # SWE should be NA for summer months
})




