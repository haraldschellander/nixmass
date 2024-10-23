test_that("swe.delta.snow", {
     describe("swe.delta.snow function", {
          
          it ("should throw an error if data is not a data.frame", {
               expect_error(swe.delta.snow(list(0, 1)), "data must be of class 'data.frame'")
          })
          
          it ("should throw an error if no date and hs columns are availabe", {
               data <- data.frame(dates = c("2024-01-01", "2024-01-02"), hs = c(0, 1))
               expect_error(swe.delta.snow(data), "data must contain at least two columns named 'date' and 'hs'")
               
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), rr = c(0, 1))
               expect_error(swe.delta.snow(data), "data must contain at least two columns named 'date' and 'hs'")
          })
          
          # it ("should throw an error if date column is not of class 'character'", {
          #      data <- data.frame(date = as.Date(c("2024-01-01", "2024-01-02")), hs = c(0, 1))
          #      expect_error(swe.delta.snow(data), "date column must be of class 'character'")
          # })
          # 
          # it ("should throw an error if date column is wrongly formatted", {
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
          it ("character date input with valid format", {
               data <- data.frame(date = c("2023-01-01", "2023-02-01"))
               result <- validate_date_column(data)
               expect_true(inherits(result$date, "Date"))
          })
          
          it ("character date input with invalid format", {
               data <- data.frame(date = c("01-01-2023", "2023/02/01"))
               expect_error(validate_date_column(data), "date format must be '%Y-%m-%d'")
          })
          
          it ("Date class input with valid format", {
               data <- data.frame(date = as.Date(c("2023-01-01", "2023-02-01")))
               result <- validate_date_column(data)
               expect_true(inherits(result$date, "Date"))
          })
          
          it ("POSIXct class input with valid format", {
               data <- data.frame(date = as.POSIXct(c("2023-01-01 00:00:00", "2023-02-01 00:00:00")))
               result <- validate_date_column(data)
               expect_true(inherits(result$date, "POSIXct"))
          })
          
          it ("invalid date class input", {
               data <- data.frame(date = factor(c("2023-01-01", "2023-02-01")))
               expect_error(validate_date_column(data), "date column must be either of class 'character', 'Date' or 'POSIXct'")
          })
          
          it ("should throw an error if data contains negative values", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, -1))
               expect_error(swe.delta.snow(data), "data must not contain values < 0")
          })
          
          it ("should throw an error if data contains NA values", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, NA))
               expect_error(swe.delta.snow(data), "data must not contain NA")
          })
          
          it ("should throw an error if data are not numeric", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c("0", 1))
               expect_error(swe.delta.snow(data), "snow depth data must be numeric")
          })
          
          it ("should throw an error if first observation is not 0", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(1, 2))
               expect_error(swe.delta.snow(data), "snow depth observations must start with 0")
          })
          
          it ("should trow an error if date column is not strictly regular", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02", "2024-01-05"), hs = c(0, 1, 0))
               expect_error(swe.delta.snow(data), "snow depth data must be strictly regular")
          })
          
          # parameter checks
          it ("should throw an error if 'rho.max' is negative or 0", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, 1))
               expect_error(swe.delta.snow(data, rho.max = 0), "'rho.max' must not be negative or 0")
          })
          it ("should throw an error if 'rho.null' is negative or 0", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, 1))
               expect_error(swe.delta.snow(data, rho.null = 0), "'rho.null' must not be negative or 0")
          })
          it ("should throw an error if 'k.ov' < 0", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, 1))
               expect_error(swe.delta.snow(data, k.ov = -1), "'k.ov' must be > 0")
          })
          it ("should throw an error if 'k.ov' > 1", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, 1))
               expect_error(swe.delta.snow(data, k.ov = 10), "'k.ov' must be < 1")
          })
          it ("should throw an error if 'k' is negative or 0", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, 1))
               expect_error(swe.delta.snow(data, k = 0), "'k' must not be negative or 0")
          })
          it ("should throw an error if 'tau' is negative or 0", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, 1))
               expect_error(swe.delta.snow(data, tau = 0), "'tau' must not be negative or 0")
          })
          it ("should throw an error if timestep does not fit to data", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02", "2024-01-03"), hs = c(0, 1, 0))
               expect_error(swe.delta.snow(data, timestep = 48), "provided timestep does not fit your data")
               # data <- data.frame(date = c("2024-01-01 01:00", "2024-01-01 02:00", "2024-01-01 03:00"), hs = c(0, 20, 0))
               # expect_error(swe.delta.snow(data, timestep = 12), "provided timestep does not fit your data")
               
          }) 
          it ("should throw an error if timestep is less than 24 hours", {
               data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, 0))
               expect_error(swe.delta.snow(data, timestep = 1), "timestep must be >= 24 hours")
          })
          
          it ("should correctly calculate SWE values", {
               # Setup a mock dataset
               sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
               sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
               data <- data.frame(date = as.character(sample_dates), hs = sample_data)
               result <- swe.delta.snow(data)
               expect_true(all(result >= 0))
               expect_true(length(result) == nrow(data))
          })
          it ("should correctly calculate SWE values and internal matrices", {
               # Setup a mock dataset
               sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
               sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
               data <- data.frame(date = as.character(sample_dates), hs = sample_data)
               result <- swe.delta.snow(data, return_full = TRUE)
               expect_true("results" %in% names(result))
               expect_true("matrices" %in% names(result))
               expect_true("date" %in% names(result$results))
               expect_true("Hobs" %in% names(result$results))
               expect_true("SWE" %in% names(result$results))
               expect_true("ti" %in% names(result$matrices))
               expect_true("date" %in% names(result$matrices))
               expect_true("param" %in% names(result$matrices))
               expect_true(nrow(result$results) > 0)
               expect_true(nrow(result$matrices) > 0)
               
          })
          
          
          
          ####
          
          # it ("should throw an error if method != 'pwm' and threshold > 0", {
          #      data <- data.frame(groupvar = c(as.Date(c("2024-01-01", "2025-01-01"))), val = c(10, 20))
          #      expect_error(fmev(data, threshold = 1, method = "mle"), "threshold can only be used for method 'pwm'")
          # })
          # 
          # it ("should correctly calculate MEV parameters for valid input", {
          #      # Setup a mock dataset
          #      data <- data.frame(
          #           groupvar = as.Date(c("2024-01-01", "2024-02-01", "2025-01-01")),
          #           val = c(10, 15, 12)
          #      )
          #      result <- fmev(data, threshold = 5)
          #      # Check if the result has the expected structure
          #      expect_true("c" %in% names(result))
          #      expect_true("w" %in% names(result))
          #      expect_true("n" %in% names(result))
          #      # Add more assertions based on what you expect the output to be
          # })
          # 
          # it ("should correctly calculate MEV parameters for method 'mle'", {
          #      # Setup a mock dataset
          #      set.seed(123)
          #      sample_dates <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2010-01-01"), by = "day")
          #      sample_data <- data.frame(groupvar = sample_dates, val = sample(rnorm(length(sample_dates))))
          #      sample_data$groupvar <- as.Date(sample_data$groupvar)
          #      data <- sample_data %>%
          #           filter(val >= 0 & !is.na(val))
          #      result <- fmev(data, method = "mle")
          #      # Check if the result has the expected structure
          #      expect_true("c" %in% names(result))
          #      expect_true("w" %in% names(result))
          #      expect_true("n" %in% names(result))
          # })
          # 
          # it ("should correctly calculate MEV parameters for method 'ls'", {
          #      # Setup a mock dataset
          #      set.seed(123)
          #      sample_dates <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2001-01-01"), by = "day")
          #      sample_data <- data.frame(groupvar = sample_dates, val = sample(rnorm(length(sample_dates))))
          #      sample_data$groupvar <- as.Date(sample_data$groupvar)
          #      data <- sample_data %>%
          #           filter(val >= 0 & !is.na(val))
          #      result <- fmev(data, method = "ls")
          #      # Check if the result has the expected structure
          #      expect_true("c" %in% names(result))
          #      expect_true("w" %in% names(result))
          #      expect_true("n" %in% names(result))
          # })
          
     })
})
