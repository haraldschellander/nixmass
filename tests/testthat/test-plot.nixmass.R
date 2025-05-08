test_that("plot.nixmass validates input correctly", {
  # Test for invalid object class
  invalid_object <- list(swe = list(model1 = c(1, 2, 3)))
  expect_error(
    plot.nixmass(invalid_object),
    "nixmass: Object must be of class 'nixmass'"
  )

  # Test for object with no models computed
  valid_object_no_model <- structure(list(swe = list()), class = "nixmass")
  expect_error(
    plot.nixmass(valid_object_no_model),
    "nixmass: Cannot plot. No model was computed."
  )

  # Test for missing layer matrices in density mode
  valid_object_missing_layers <- structure(
    list(swe = list(model1 = list())),
    class = "nixmass"
  )
  expect_error(
    plot.nixmass(valid_object_missing_layers, density = TRUE),
    #"The layer matrices are missing in the modeled object.")
    "Density plot can only be created for the delta.snow or delta.snow.dyn_rho_max models."
  )
})

test_that("plot.nixmass generates time series plots for SWE", {
  # Valid object with SWE data for a single model
  valid_object <- structure(
    list(
      swe = list(model1 = c(1, 2, 3, 4, 5)),
      date = as.Date(c(
        "2024-01-01",
        "2024-02-01",
        "2024-03-01",
        "2024-04-01",
        "2024-05-01"
      )),
      hs = c(0.01, 0.02, 0.03, 0.04, 0.05)
    ),
    class = "nixmass"
  )

  # Capture the plot output
  expect_silent(plot.nixmass(valid_object))
})

test_that("plot.nixmass handles multiple models correctly", {
  # Valid object with SWE data for multiple models
  valid_object <- structure(
    list(
      swe = list(
        model1 = c(1, 2, 3, 4, 5),
        model2 = c(2, 3, 4, 5, 6)
      ),
      date = as.Date(c(
        "2024-01-01",
        "2024-02-01",
        "2024-03-01",
        "2024-04-01",
        "2024-05-01"
      )),
      hs = c(0.01, 0.02, 0.03, 0.04, 0.05)
    ),
    class = "nixmass"
  )

  # Capture the plot output
  expect_silent(plot.nixmass(valid_object))
})

test_that("plot.nixmass generates density plots when density = TRUE", {
  # Valid object with SWE and layer matrices for density plot
  valid_object_density <- structure(
    list(
      swe = list(
        # model1 = list(
        "delta.snow" = list(
          h = matrix(c(10, 20, 30, 40, 50), ncol = 5),
          swe = matrix(c(100, 200, 300, 400, 500), ncol = 5)
        )
      ),
      date = as.Date(c(
        "2024-01-01",
        "2024-02-01",
        "2024-03-01",
        "2024-04-01",
        "2024-05-01"
      )),
      hs = c(0.01, 0.02, 0.03, 0.04, 0.05)
    ),
    class = "nixmass"
  )

  # Capture the plot output
  #expect_silent({
  p <- plot.nixmass(valid_object_density, density = TRUE)
  expect_s3_class(p, "ggplot") # Verify it's a ggplot object
  #})
})

test_that("plot.nixmass handles title input", {
  # Valid object with a custom title
  valid_object <- structure(
    list(
      swe = list(model1 = c(1, 2, 3, 4, 5)),
      date = as.Date(c(
        "2024-01-01",
        "2024-02-01",
        "2024-03-01",
        "2024-04-01",
        "2024-05-01"
      )),
      hs = c(0.01, 0.02, 0.03, 0.04, 0.05)
    ),
    class = "nixmass"
  )

  # Check custom title inclusion
  expect_silent({
    plot.nixmass(valid_object, title = "Custom Title")
  })
})
