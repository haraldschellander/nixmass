test_that("should throw an error if data is not a data.frame", {
  expect_error(hs2swe(list(0, 1)), "data must be of class 'data.frame'")
})

test_that("should throw an error if no date and hs columns are availabe", {
  data <- data.frame(dates = c("2024-01-01", "2024-01-02"), hs = c(0, 1))
  expect_error(
    hs2swe(data),
    "data must contain at least two columns named 'date' and 'hs'"
  )

  data <- data.frame(date = c("2024-01-01", "2024-01-02"), rr = c(0, 1))
  expect_error(
    hs2swe(data),
    "data must contain at least two columns named 'date' and 'hs'"
  )
})

# test_that("should throw an error if date column is not of class 'character'", {
#      data <- data.frame(date = as.Date(c("2024-01-01", "2024-01-02")), hs = c(0, 1))
#      expect_error(hs2swe(data), "date column must be of class 'character'")
# })
#
# test_that("should throw an error if date column is wrongly formatted", {
#      data <- data.frame(date = c("20240101", "20240102"), hs = c(0, 1))
#      expect_error(hs2swe(data), "date format must be '%Y-%m-%d'")
#      data <- data.frame(date = c("2024-01-01 01:00", "2024-01-01 02:00"), hs = c(0, 1))
#      expect_error(hs2swe(data), "date format must be '%Y-%m-%d'")
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
  data <- data.frame(
    date = as.POSIXct(c("2023-01-01 00:00:00", "2023-02-01 00:00:00"))
  )
  result <- validate_date_column(data)
  expect_true(inherits(result$date, "POSIXct"))
})

test_that("invalid date class input", {
  data <- data.frame(date = factor(c("2023-01-01", "2023-02-01")))
  expect_error(
    validate_date_column(data),
    "date column must be either of class 'character', 'Date' or 'POSIXct'"
  )
})

test_that("should throw an error if data contains negative values", {
  data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, -1))
  expect_error(hs2swe(data), "data must not contain values < 0")
})

test_that("should throw an error if data contains NA values", {
  data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c(0, NA))
  expect_error(hs2swe(data), "data must not contain NA")
})

test_that("should throw an error if data are not numeric", {
  data <- data.frame(date = c("2024-01-01", "2024-01-02"), hs = c("0", 1))
  expect_error(hs2swe(data), "snow depth data must be numeric")
})

test_that("should trow an error if date column is not strictly regular", {
  data <- data.frame(
    date = c("2024-01-01", "2024-01-02", "2024-01-05"),
    hs = c(0, 1, 0)
  )
  expect_error(hs2swe(data), "snow depth data must be strictly regular")
})
