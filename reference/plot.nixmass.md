# Plot modeled SWE values of a nixmass object.

Plot modeled SWE values of a nixmass object.

## Usage

``` r
# S3 method for class 'nixmass'
plot(x, title = NULL, density = FALSE, ...)
```

## Arguments

- x:

  nixmass object.

- title:

  Main plot title.

- density:

  Shall a density plot be created?

- ...:

  Further graphical parameters may also be supplied as arguments. See
  [`plot`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Does not return anything. A plot is produced.

## Examples

``` r
data("hsdata")
plot(nixmass(hsdata, model = "delta.snow"))

plot(nixmass(hsdata, model = "delta.snow.dyn_rho_max", layers = TRUE), density = TRUE)
#> Warning: Removed 9720 rows containing missing values or values outside the scale range
#> (`geom_col()`).

```
