# Statistical SWE modeling depending on month and climatic region in Switzerland

Snow Water Equivalent (SWE) is modeled statistically depending on snow
depth, altitude, date and region in Switzerland.

## Usage

``` r
swe.jo09(data, alt, region.jo09)
```

## Arguments

- data:

  A data.frame of daily observations with two columns named *date* and
  *hs* referring to day and snow depth at that day. The date column must
  be either of class \`character\`, \`Date\` or \`POSIXct\` with the
  format `YYYY-MM-DD`. The hs column must be snow depth values \\\ge 0\\
  in m.

- alt:

  Station elevation in meters.

- region.jo09:

  Integer number of the Swiss region where the station belongs to,
  according to Fig. 1 in the original reference. Must be one of
  1,2,3,4,5,6,7.

## Value

A numeric vector with SWE values for each region in mm.

## Details

`swe.jo09` This model parameterizes bulk snow density using snow depth,
season (i.e. month), site altitude and site location. The location is
implemented by a density offset according to the region in Switzerland,
where the station belongs to. Non computable values are returned as NA.

## References

Jonas, T., Marty, C. and Magnusson, J. (2009) 'Estimating the snow water
equivalent from snow depth measurements in the Swiss Alps', Journal of
Hydrology, 378(1 - 2), pp. 161 - 167. doi:
10.1016/j.jhydrol.2009.09.021.

## Examples

``` r
data(hsdata)
swe <- swe.jo09(hsdata, 1500, 1)
summary(swe)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    0.00    0.00    0.00   40.29   26.55  381.16 
plot(swe)
```
