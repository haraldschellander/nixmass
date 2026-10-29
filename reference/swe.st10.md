# Statistical SWE modeling depending on day of year and a climatic region

The *Sturm* model parameterizes bulk snow density with day of the year
and a *snowclass.st10*. It was trained on historical snow depth -
density - SWE data from the United States, Canada, and Switzerland.

## Usage

``` r
swe.st10(
  data,
  snowclass.st10 = c("alpine", "maritime", "prairie", "tundra", "taiga")
)
```

## Arguments

- data:

  A data.frame with at least two columns named `date` and `hs`. They
  should contain date and corresponding daily observations of snow depth
  \\hs \ge 0\\ measured at one site. The unit must be meters (m). No
  gaps or NA are allowed. Dates must be either of class \`character\`,
  \`Date\` or \`POSIXct\` and given in the format `YYYY-MM-DD`. No
  sub-daily resolution is allowed at the moment (see details).

- snowclass.st10:

  Must be one of the following character strings:
  "alpine","maritime","prairie","tundra","taiga".

## Value

A vector with daily SWE values in mm.

## Details

This model converts snow depth to SWE using snow depth, day of year and
station location (from which a climate class of snow can be inferred.
The day of year (DOY) is the day-number of in the season 1.10. - 30.6.
The 1.10. refers to DOY = -92. The 1.2. would be DOY = 32, while 15.11.
would be DOY = -47. The *snowclass.st10* must be one out of the
character strings "alpine","maritime","prairie","tundra" and "taiga".
For the Alps probably "alpine" would be the most appropriate climate
classification. Non computable values are returned as NA.

## References

Sturm, M. et al. (2010) 'Estimating Snow Water Equivalent Using Snow
Depth Data and Climate Classes', Journal of Hydrometeorology, 11(6), pp.
1380 - 1394. doi: 10.1175/2010JHM1202.1.

## Examples

``` r
data(hsdata)
swe <- swe.st10(hsdata)
summary(swe)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    0.00    0.00    0.00   44.69   29.30  439.41 
```
