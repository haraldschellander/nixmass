test_that("summary.nixmass validates input correctly", {
        # Test for invalid object class
        invalid_object <- list(swe = list(model1 = c(1, 2, 3)))
        expect_error(summary.nixmass(invalid_object), 
                     "nixmass: Object must be of class 'nixmass'")
        
        # Test for object with no models computed
        valid_object_no_model <- structure(list(swe = list()), class = "nixmass")
        expect_error(summary.nixmass(valid_object_no_model), 
                     "nixmass: Cannot print anything. No model was computed.")
})

test_that("summary.nixmass computes summary statistics for a single model", {
        # Valid object with one model
        valid_object <- structure(list(swe = list(model1 = c(1, 2, 3, 4, 5))), class = "nixmass")
        
        # Capture output
        output <- capture.output(summary.nixmass(valid_object))
        
        # Check the output contains the correct headers
        expect_true(any(grepl("Min.", output)))
        expect_true(any(grepl("1st Qu.", output)))
        expect_true(any(grepl("Median", output)))
        expect_true(any(grepl("Mean", output)))
        expect_true(any(grepl("3rd Qu.", output)))
        expect_true(any(grepl("Max.", output)))
        
        # Check the statistics are correct
        expected_values <- c(min = 1, "1st Qu." = 2, median = 3, mean = 3, "3rd Qu." = 4, max = 5)
        expect_true(all(sapply(names(expected_values), function(name) any(grepl(as.character(expected_values[name]), output)))))
})

test_that("summary.nixmass computes summary statistics for multiple models", {
        # Valid object with multiple models
        valid_object <- structure(
                list(swe = list(
                        model1 = c(1, 2, 3, 4, 5),
                        model2 = c(6, 7, 8, 9, 10)
                )), 
                class = "nixmass"
        )
        
        # Capture output
        output <- capture.output(summary.nixmass(valid_object))
        
        # Check that the output includes statistics for both models
        expect_true(any(grepl("model1", output)))
        expect_true(any(grepl("model2", output)))
        
        # Check the headers for summary statistics
        expect_true(any(grepl("Min.", output)))
        expect_true(any(grepl("1st Qu.", output)))
        expect_true(any(grepl("Median", output)))
        expect_true(any(grepl("Mean", output)))
        expect_true(any(grepl("3rd Qu.", output)))
        expect_true(any(grepl("Max.", output)))
        
        # Validate expected statistics for model1 and model2
        expected_values_model1 <- c(min = 1, "1st Qu." = 2, median = 3, mean = 3, "3rd Qu." = 4, max = 5)
        expected_values_model2 <- c(min = 6, "1st Qu." = 7, median = 8, mean = 8, "3rd Qu." = 9, max = 10)
        
        expect_true(all(sapply(names(expected_values_model1), function(name) any(grepl(as.character(expected_values_model1[name]), output)))))
        expect_true(all(sapply(names(expected_values_model2), function(name) any(grepl(as.character(expected_values_model2[name]), output)))))
})
