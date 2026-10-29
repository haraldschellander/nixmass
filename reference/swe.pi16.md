# Statistical SWE modeling depending on the day-of-year

This model parameterizes bulk snow density with day-of-the-year as the
only input. It was calibrated for the region of South Tyrol, Italy, and
is therefore called ST model in the original reference.

## Usage

``` r
swe.pi16(data, rho_0 = 200, K = 1)
```

## Arguments

- data:

  A data.frame with at least two columns named `date` and `hs`. They
  should contain date and corresponding daily observations of snow depth
  \\hs \ge 0\\ measured at one site. The unit must be meters (m). No
  gaps or NA are allowed. Dates must be either of class \`character\`,
  \`Date\` or \`POSIXct\` and given in the format `YYYY-MM-DD`. No
  sub-daily resolution is allowed at the moment (see details).

- rho_0:

  Intercept of the linear regression between observed snow depths and
  SWE values. `rho_0` is set to 200 as default, which is the value from
  the original reference. It can however be set to any value according
  to regression modeling with other datasets.

- K:

  Slope of the linear regression between observed densities and the
  day-of-year as defined in the original reference. `K` is set to 1 as
  default, which is the value from the original reference. It can
  however be set to any value according to regression modeling with
  other datasets.

## Value

A vector with daily SWE values in mm.

## Details

`swe.pi16` This function uses only the day-of-year (DOY) as
parameterization for bulk snow density and hence SWE. Here, the datums
in the input data.frame are converted to DOY as defined in the original
reference: negative values between 1.10. and 31.12. DOY=-92 at 1.10. In
leap years 31.12. has DOY = 0, in non-leap years 31.12. has DOY = -1
with no day being 0. Non computable values are returned as NA.

## References

Pistocchi, A. (2016) 'Simple estimation of snow density in an Alpine
region', Journal of Hydrology: Regional Studies. Elsevier B.V.,
6(Supplement C), pp. 82 - 89. doi: 10.1016/j.ejrh.2016.03.004.

## Examples

``` r
data(hsdata)
swe <- swe.pi16(hsdata)
summary(swe)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    0.00    0.00    0.00   44.71   32.64  410.80 
```
