

test_that("should throw an error if data is not a data.frame", {
     expect_error(swe.delta.snow(list(0, 1)), "data must be of class 'data.frame'")
})

test_that("should throw an error if no date and hs columns are availabe", {
     data <- data.frame(dates = c("2024-01-01", "2024-01-02"), hs = c(0, 1))
     expect_error(swe.delta.snow(data), "data must contain at least two columns named 'date' and 'hs'")
     
     data <- data.frame(date = c("2024-01-01", "2024-01-02"), rr = c(0, 1))
     expect_error(swe.delta.snow(data), "data must contain at least two columns named 'date' and 'hs'")
})

# test_that("should throw an error if date column is not of class 'character'", {
#      data <- data.frame(date = as.Date(c("2024-01-01", "2024-01-02")), hs = c(0, 1))
#      expect_error(swe.delta.snow(data), "date column must be of class 'character'")
# })
# 
# test_that("should throw an error if date column is wrongly formatted", {
#      data <- data.frame(date = c("20240101", "20240102"), hs = c(0, 1))
#      expect_error(swe.delta.snow(data), "date format must be '%Y-%m-%d'")
#      data <- data.frame(date = c("2024-01-01 01:00", "2024-01-01 02:00"), hs = c(0, 1))
#      expect_error(swe.delta.snow(data), "date format must be '%Y-%m-%d'")
# })
validate_date_column <- function(data) {
     if (inherits(data$date, "character")) {
          if (any(is.na(as.POSIXlt(data$date, format = "%Y-%m-%d")))) {
               stop("date format must be '%Y-%m-%d'") 
          } else {
               data$date <- as.Date(data$date)
          }
     } else if (inherits(data$date, "Date") | inherits(data$date, "POSIXct")) {
          if (any(is.na(as.POSIXlt(data$date, format = "%Y-%m-%d")))) {
               stop("date format must be '%Y-%m-%d'") 
          }
     } else {
          stop("date column must be either of class 'character', 'Date' or 'POSIXct'")
     }
     return(data)
}
test_that("character date input with valid format", {
     data <- data.frame(date = c("2023-01-01", "2023-02-01"))
     result <- validate_date_column(data)
     expect_true(inherits(result$date, "Date"))
})

test_that("character date input with invalid format", {
     data <- data.frame(date = c("01-01-2023", "2023/02/01"))
     expect_error(validate_date_column(data), "date format must be '%Y-%m-%d'")
})

test_that("Date class input with valid format", {
     data <- data.frame(date = as.Date(c("2023-01-01", "2023-02-01")))
     result <- validate_date_column(data)
     expect_true(inherits(result$date, "Date"))
})

test_that("POSIXct class input with valid format", {
     data <- data.frame(date = as.POSIXct(c("2023-01-01 00:00:00", "2023-02-01 00:00:00")))
     result <- validate_date_column(data)
     expect_true(inherits(result$date, "POSIXct"))
})

test_that("invalid date class input", {
     data <- data.frame(date = factor(c("2023-01-01", "2023-02-01")))
     expect_error(validate_date_column(data), "date column must be either of class 'character', 'Date' or 'POSIXct'")
})

test_that("should throw an error if data contains negative values", {
     data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, -1))
     expect_error(swe.delta.snow(data), "data must not contain values < 0")
})

test_that("should throw an error if data contains NA values", {
     data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, NA))
     expect_error(swe.delta.snow(data), "data must not contain NA")
})

test_that("should throw an error if data are not numeric", {
     data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c("0", 1))
     expect_error(swe.delta.snow(data), "snow depth data must be numeric")
})

test_that("should throw an error if first observation is not 0", {
     data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(1, 2))
     expect_error(swe.delta.snow(data), "snow depth observations must start with 0")
})

test_that("should trow an error if date column is not strictly regular", {
     data <- data.frame(date = c("2024-01-01", "2024-01-02", "2024-01-05"), hs = c(0, 1, 0))
     expect_error(swe.delta.snow(data), "snow depth data must be strictly regular")
})

# parameter checks
mock_data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, 1))     
test_that("should throw an error if 'sigma` is negative or 0", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(sigma = 0)), "'sigma' must not be negative or 0")
})

test_that("should throw an error if 'mu` is negative or 0", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(mu = 0)), "'mu' must not be negative or 0")
})

test_that("should throw an error if 'rho_h` is negative or 0", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(rho_h = 0)), "'rho_h' must not be negative or 0")
})

test_that("should throw an error if 'rho_l` is negative or 0", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(rho_l = 0)), "'rho_l' must not be negative or 0")
})

test_that("should throw an error if 'rho.max' is negative or 0", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(rho.max = 0), dyn_rho_max = FALSE), "'rho.max' must not be negative or 0")
})
test_that("should throw an error if 'rho.null' is negative or 0", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(rho.null = 0)), "'rho.null' must not be negative or 0")
})
test_that("should throw an error if 'k.ov' < 0", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(k.ov = -1)), "'k.ov' must be > 0")
})
test_that("should throw an error if 'k.ov' > 1", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(k.ov = 10)), "'k.ov' must be < 1")
})
test_that("should throw an error if 'k' is negative or 0", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(k = 0)), "'k' must not be negative or 0")
})
test_that("should throw an error if 'tau' is negative or 0", {
     expect_error(swe.delta.snow(mock_data, model_opts = list(tau = 0)), "'tau' must not be negative or 0")
})
test_that("should throw an error if timestep does not fit to data", {
     data <- data.frame(date = c("2024-01-01", "2024-01-02", "2024-01-03"), hs = c(0, 1, 0))
     expect_error(swe.delta.snow(data, model_opts = list(timestep = 48)), "provided timestep does not fit your data")
     # data <- data.frame(date = c("2024-01-01 01:00", "2024-01-01 02:00", "2024-01-01 03:00"), hs = c(0, 20, 0))
     # expect_error(swe.delta.snow(data, timestep = 12), "provided timestep does not fit your data")
     
}) 
test_that("should throw an error if timestep is less than 24 hours", {
     data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, 0))
     expect_error(swe.delta.snow(data, model_opts = list(timestep = 1)), "timestep must be >= 24 hours")
})

test_that("should correctly calculate SWE values with constant maximum density", {
     # Setup a mock dataset
     sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
     sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
     data <- data.frame(date = as.character(sample_dates), hs = sample_data)
     result <- swe.delta.snow(data, dyn_rho_max = FALSE)
     expect_true(all(result >= 0))
     expect_true(length(result) == nrow(data))
})

test_that("should correctly calculate SWE values with increasing maximum density", {
     # Setup a mock dataset
     sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
     sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
     data <- data.frame(date = as.character(sample_dates), hs = sample_data)
     result <- swe.delta.snow(data, dyn_rho_max = TRUE)
     expect_true(all(result >= 0))
     expect_true(length(result) == nrow(data))
})



