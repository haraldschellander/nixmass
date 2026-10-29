# Statistical SWE modeling based on a quadratic dependance on the day-of-year

This model parameterizes bulk snow density with day-of-the-year as the
only input similar to
[`swe.pi16`](https://haraldschellander.github.io/nixmass/reference/swe.pi16.md)
but adds a quadratic dependance. It was calibrated for the regions of
the whole Italian alps, and the subregions South-West, Central and
South-East. By setting the coefficients of the empirical regression it
can however be used with results from other datasets.

## Usage

``` r
swe.gu19(data, region.gu19, n0 = NA, n1 = NA, n2 = NA)
```

## Arguments

- data:

  A data.frame of daily observations with two columns named *date* and
  *hs* referring to day and snow depth at that day. The date column can
  be of class \`character\`, \`Date\` or \`POSIXct\` with the format
  `YYYY-MM-DD`. The hs column must be snow depth values \\\ge 0\\ in m.

- region.gu19:

  Must be one of the italian subalpine regions *italy*, *southwest*,
  *central* or *southeast*, defined in the original reference (see
  details), or *myregion*, in which case the coefficients n0, n1 and n2
  have to be set.

- n0:

  Intercept of an empirical regression between densities and the
  day-of-year (see details).

- n1:

  Slope of an empirical regression between densities and the day-of-year
  (see details).

- n2:

  Quadratic dependence of an empirical regression between densities and
  the day-of-year (see details).

## Value

A vector with daily SWE values in mm.

## Details

`swe.gu19` Similar to the model of Pistocchi (2016), this function uses
only the day-of-year (DOY) as parameterization for bulk snow density and
hence SWE. In contrast to the latter, here, a quadratic term for DOY was
added, to reflect non-linearity in the snow bulk density variability.
The datums in the input data.frame are converted to DOY as days spent
since November 1st. Regression coefficients depend on regions defined in
Guyennon et al. (2019), which are *italy* for the Italian Alps,
*southwest* for the South-western Italian Alps, *central* for the
Central Italian Alpes or *southeast* for the South-western Italian Alps.

If `region.gu19` is set to *myregion*, the coefficients `no`, `n1` and
`n2` must be set to values, obtained from a regression between densities
and day-of-year from another dataset. It has to have the form density ~
DOY + DOY^2, where DOY is the day-of-year as defined in the original
reference. Non computable values are returned as NA.

## References

Guyennon, N., Valt, M., Salerno, F., Petrangeli, A., Romano, E. (2019)
'Estimating the snow water equivalent from snow depth measurements in
the Italian Alps', Cold Regions Science and Technology. Elsevier, 167
(August), p. 102859. doi: 10.1016/j.coldregions.2019.102859.

Pistocchi, A. (2016) 'Simple estimation of snow density in an Alpine
region', Journal of Hydrology: Regional Studies. Elsevier B.V., 6
(Supplement C), pp. 82 - 89. doi: 10.1016/j.ejrh.2016.03.004.

## Examples

``` r
data(hsdata)
swe <- swe.gu19(hsdata, region = "italy")
summary(swe)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>   0.000   0.000   5.488  58.126  53.262 391.731      92 
```
