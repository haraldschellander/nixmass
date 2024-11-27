test_that("nixmass works with delta.snow model", {
     data <- data.frame(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     result <- nixmass(data, model = "delta.snow", verbose = FALSE)
     
     expect_s3_class(result, "nixmass")
     expect_true("delta.snow" %in% names(result$swe))
     expect_equal(length(result$swe$delta.snow), nrow(data))
})

test_that("nixmass works with multiple models", {
     data <- data.frame(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     result <- nixmass(data, model = c("delta.snow", "delta.snow.dyn_rho_max"), verbose = FALSE)
     
     expect_s3_class(result, "nixmass")
     expect_true("delta.snow" %in% names(result$swe))
     expect_true("delta.snow.dyn_rho_max" %in% names(result$swe))
})

test_that("nixmass throws an error for missing parameters", {
     data <- data.frame(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     
     expect_error(nixmass(data, model = "jo09"), "Argument 'alt' is missing")
     expect_error(nixmass(data, model = "st10"), "Argument 'snowclass.st10' is missing")
     expect_error(nixmass(data, model = "gu19"), "Argument 'region.gu19' is missing")
})

test_that("nixmass handles invalid inputs", {
     data <- data.frame(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     
     expect_error(nixmass(data.frame(date = "invalid", hs = c(0.1, 0.2)), model = "delta.snow"), "date format must be '%Y-%m-%d'")
     expect_error(nixmass(data.frame(date = as.Date('2023-01-01') + 0:9, hs = c(NA, 0.1)), model = "delta.snow"), "data must not contain NA")
})

test_that("nixmass works with regional models", {
     data <- data.frame(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     
     result_jo09 <- nixmass(data, model = "jo09", alt = 1000, region.jo09 = 1)
     expect_true("jo09" %in% names(result_jo09$swe))
     
     result_gu19 <- nixmass(data, model = "gu19", region.gu19 = "central")
     expect_true("gu19" %in% names(result_gu19$swe))
     
     result_st10 <- nixmass(data, model = "st10", snowclass.st10 = "alpine")
     expect_true("st10" %in% names(result_st10$swe))
})

test_that("nixmass works with default model", {
     data <- data.frame(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     result <- nixmass(data)
     
     expect_s3_class(result, "nixmass")
     expect_true("delta.snow" %in% names(result$swe))
})

test_that("nixmass provides verbose output when verbose = TRUE", {
     data <- data.frame(date = as.Date('2023-01-01') + 0:9, hs = seq(0, 0.5, length.out = 10))
     
     expect_output(nixmass(data, model = "delta.snow", verbose = TRUE), "Using parameters")
})
