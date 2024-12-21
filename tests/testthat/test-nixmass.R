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

test_that("should return matrices for swe, snowdepth and age when layers is TRUE", {
        # Setup a mock dataset
        sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
        sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
        data <- data.frame(date = as.character(sample_dates), hs = sample_data)
        result <- nixmass(data, layers = TRUE)
        expect_true(inherits(result$swe$delta.snow$SWE, "numeric"))
        expect_true(inherits(result$swe$delta.snow$h, "matrix"))
        expect_true(inherits(result$swe$delta.snow$swe, "matrix"))
        expect_true(inherits(result$swe$delta.snow$age, "matrix"))
        expect_true(inherits(result$swe$delta.snow$processes, "character"))
})

test_that("nixmass works with layers=TRUE", {
        sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
        sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
        data <- data.frame(date = as.character(sample_dates), hs = sample_data)
        result <- nixmass(data, layers = TRUE)
        expect_true(length(result$swe$delta.snow) == 5)
})

test_that("nixmass returns a list for each model when layers=TRUE", {
        sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
        sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
        data <- data.frame(date = as.character(sample_dates), hs = sample_data)
        result <- nixmass(data, model = "delta.snow.dyn_rho_max", layers = TRUE)
        expect_true(length(result$swe$delta.snow.dyn_rho_max) == 5)
})

test_that("should return matrices for swe, snowdepth and age when layers is TRUE", {
        # Setup a mock dataset
        sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
        sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
        data <- data.frame(date = as.character(sample_dates), hs = sample_data)
        result <- nixmass(data, model = "delta.snow.dyn_rho_max", layers = TRUE)
        expect_true(inherits(result$swe$delta.snow.dyn_rho_max$SWE, "numeric"))
        expect_true(inherits(result$swe$delta.snow.dyn_rho_max$h, "matrix"))
        expect_true(inherits(result$swe$delta.snow.dyn_rho_max$swe, "matrix"))
        expect_true(inherits(result$swe$delta.snow.dyn_rho_max$age, "matrix"))
        expect_true(inherits(result$swe$delta.snow.dyn_rho_max$processes, "character"))
})

test_that("should return only the swe vector when layers is FALSE", {
        # Setup a mock dataset
        sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
        sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
        data <- data.frame(date = as.character(sample_dates), hs = sample_data)
        result <- nixmass(data)
        expect_true(length(result$swe$delta.snow) == nrow(data))
})

test_that("nixmass returns results for both models when layers=TRUE", {
        sample_dates <- seq.Date(from = as.Date("2000-11-01"), to = as.Date("2000-11-07"), by = 1)
        sample_data <- c(0, seq(0.1, 0.5, 0.1), 0)
        data <- data.frame(date = as.character(sample_dates), hs = sample_data)
        result <- nixmass(data, model = c("delta.snow", "delta.snow.dyn_rho_max"), layers = TRUE)
        expect_true(length(names(result$swe)) == 2)
        expect_true("delta.snow" %in% names(result$swe))
        expect_true("delta.snow.dyn_rho_max" %in% names(result$swe))
})