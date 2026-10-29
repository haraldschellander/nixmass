# Print summary of a nixmass object.

Print summary of a nixmass object.

## Usage

``` r
# S3 method for class 'nixmass'
summary(object, ...)
```

## Arguments

- object:

  A nixmass object.

- ...:

  Additional arguments affecting the summary produced.

## Value

Summary information of SWE values calculated with selected models is
printed to the screen.

## Examples

``` r
data("hsdata")
n <- nixmass(hsdata, model = c("delta.snow", "pi16"))
summary(n)
#>            Min. 1st Qu. Median     Mean  3rd Qu.     Max.
#> delta.snow    0       0      0 38.58482 28.34948 282.2533
#> pi16          0       0      0 44.71321 32.64000 410.8000
```
