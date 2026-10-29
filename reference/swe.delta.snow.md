# SWE modeling from daily snow depth differences

Model daily values of Snow Water Equivalent solely from daily
differences of snow depth.

`swe.delta.snow` computes SWE solely from daily changes of snow depth at
an observation site.  
Compression of a snow layer without additional load on top is computed
on the basis of Sturm and Holmgren (1998), who regard snow as a viscous
fluid:  
\$\$\rho_i(t\_{i+1}) = \rho_i(t_i)\*(1+(SWE\*g)/\eta_0 \*
exp^{-k_2\*\rho_i(t_i)})\$\$ with \\\rho_i(t\_{i+1}) and \rho_i(t_i)\\
being tomorrow's and today's respective density of layer i, the
gravitational acceleration \\g = 9.8ms^{-2}\\, viscosity \\\eta_0\\ (Pa)
and factor \\k2 \[m^3kg^{-1}\\\], determining the importance of today's
for tomorrow's density.

## Usage

``` r
swe.delta.snow(
  data,
  model_opts = list(),
  dyn_rho_max = TRUE,
  layers = FALSE,
  strict_mode = TRUE,
  verbose = FALSE
)
```

## Arguments

- data:

  A data.frame with at least two columns named `date` and `hs`. They
  should contain date and corresponding daily observations of snow depth
  \\hs \ge 0\\ measured at one site. The unit must be meters (m). No
  gaps or NA are allowed. Dates must be either of class `character`,
  `Date` or `POSIXct` and given in the format `YYYY-MM-DD`. No sub-daily
  resolution is allowed at the moment (see details). Note that hs has to
  start with zero.

- model_opts:

  An empty list which can be populated with model coefficients specific
  to the original delta.snow model (Winkler et al., 2021) or a version,
  where the maximum layer and bulk snow densities are allowed to vary
  with age (see details).

- dyn_rho_max:

  Logical. If `TRUE`, the maximum bulk snow density is allowed to vary
  with layer age (see details). If `FALSE`, the original delta.snow
  model is used.

- layers:

  Should parameters snow depth, swe and age be returned layerwise? Can
  be `TRUE` or `FALSE`.

- strict_mode:

  Logical. If `TRUE`, the function checks if the data is strictly
  regular and if the snow depth series starts with zero.

- verbose:

  Should additional information be given during runtime? Can be `TRUE`
  or `FALSE`.

## Value

If `layers=FALSE`, a vector with daily SWE values in mm. If
`layers=TRUE`, a list with layerwise matrices of the parameters h (snow
depth), swe and age is returned in addition to the SWE vector. The
matrix rows correspond to `layers`, columns correspond to `dates`. swe
is in mm, h in m and age in days.

## Details

If `dyn_rho_max=TRUE`, the bulk snow density varies with layer age. As
activation function, `atan` is used, where the S-curve symmetrically
transitions from the lower to the upper density bound. In that case,
`model_opts` are extended by a lower density bound `rho_l`, an upper
density bound `rho_h`, a slope `sigma` and a midpoint `mu`, which
together with the other parameters have been found via the improved
optimization procedure of Werkgarner (2026). There, all ten parameters
were optimized simultaneously (Nelder-Mead) against daily SWE simulated
by SNOWPACK at eight alpine stations, using the Kling-Gupta efficiency
of SWE as objective. Among several calibration datasets and objective
functions, this set performed best in an independent validation against
SWE and new-snow water equivalent observations. Altering one or more of
the model parameters can be done with the option `model_opts`, which
must provide a named list with the altered parameters. Be aware that
altering only one parameter may have negative effects on model accuracy,
as during an optimization procedure, all parameters are balanced to
yield a minimum loss.

The following model coefficients must be provided:

`dyn_rho_max=FALSE`:

- `rho.max` Maximum density of an individual snow layer produced by the
  delta.snow model (kg/m3), \\rho.max \> 0\\. Default is
  rho.max=401.2588 kg/m3.

- `rho.null` Fresh snow density for a newly created layer (kg/m3),
  \\rho.null \> 0\\. Currently optimized for daily snow depth
  observations. Default is rho.null=81.19417 kg/m3.

- `c.ov` Overburden factor due to fresh snow (-), \\c.ov \> 0\\. Default
  is c.ov=0.0005104722.

- `k.ov` Defines the impact of the individual layer density on the
  compaction due to overburden (-), \\k.ov \in \[0,1\]\\. Default is
  k.ov=0.37856737.

- `k` Exponent of the exponential-law compaction (m3/kg), \\k \> 0\\.
  Default is k=0.02993175 m3/kg.

- `tau` Uncertainty bound (m), \\tau \> 0\\. Default is tau=0.02362476
  m.

- `eta.null` Effective compactive viscosity of snow for "zero-density"
  (Pa s). Default is eta.null=8523356 (Pa s).

- `timestep` Timestep between snow depth observations in hours. Default
  is 24 hours, i.e. daily snow depth observations. No sub-daily values
  are allowed at the moment (see details).

`dyn_rho_max=TRUE`:

Instead of a constant coefficient for `rho.max`, these four parameters
describe the smooth S-curve approximated by the `atan` trigonometric
function.

- `sigma` Steepness or slope of `atan` at its midpoint `mu`, (-),
  \\sigma \> 0\\. Default is sigma=0.0332.

- `mu` Central midpoint in days, where the steepest transition occurs
  (days), \\mu \> 0\\. Default mu=86.2 days.

- `rho_h` Upper density bound for a single layer and the whole snow pack
  (kg/m3), \\rho_h \> 0\\. Default is rho_h=688 kg/m3.

- `rho_l` Lower density bound for a single layer and the whole snow
  pack, where the transition begins (kg/m3), \\rho_l \> 0\\. Default is
  rho_l=265 kg/m3.

All other coefficients are needed as well. Note that they differ from
the constant rho_max version:

- `rho.null` Fresh snow density for a newly created layer (kg/m3),
  \\rho.null \> 0\\. Default is rho.null=107 kg/m3.

- `c.ov` Overburden factor due to fresh snow (-), \\c.ov \> 0\\. Default
  is c.ov=0.000585.

- `k.ov` Defines the impact of the individual layer density on the
  compaction due to overburden (-), \\k.ov \in \[0,1\]\\. Default is
  k.ov=0.398.

- `k` Exponent of the exponential-law compaction (m3/kg), \\k \> 0\\.
  Default is k=0.0285.

- `tau` Uncertainty bound (m), \\tau \> 0\\. Default is tau=0.0201 m.

- `eta.null` Effective compactive viscosity of snow for "zero-density"
  (Pa s). Default is eta.null=8900212 (Pa s).

- `timestep` Timestep between snow depth observations in hours. Default
  is 24 hours, i.e. daily snow depth observations.

The easiest way to call the original delta.snow model is
`swe.delta.snow(hsdata, dyn_rho_max = FALSE)`. Note that parameters
intrinsic to the dynamic density model provided with the original model
are silently ignored.

In principle, the model is able to cope with a sub-daily temporal
resolution, e.g. hourly snow depth observations. However, the model was
fitted to daily observations, and the model parameter `rho.null`
reflects that. In other words, if the observation frequency changes,
`rho.null` should change as well. Currently, no sub-daily resolution is
allowed.

## References

Gruber, S. (2014) "Modelling snow water equivalent based on daily snow
depths", Master's Thesis, Institute for Atmospheric and Cryospheric
Sciences, University of Innsbruck.  
  
Martinec, J., Rango, A. (1991) "Indirect evaluation of snow reserves in
mountain basins". Snow, Hydrology and Forests in High Alpine Areas. pp.
111-120.  
  
Schroeder, M. et al. (2024) "CONTINUOUS SNOW WATER EQUIVALENT MONITORING
ON GLACIERS USING COSMIC RAY NEUTRON SENSOR TECHNOLOGY A CASE STUDY ON
HINTEREISFERNER, AUSTRIA", Proceedings: International Snow Science
Workshop 2024, Tromsø, Norway, 1107 - 1114  
  
Sturm, M., Holmgren, J. (1998) "Differences in compaction behavior of
three climate classes of snow". Annals of Glaciology 26, 125-130.  
  
Werkgarner, J. (2026): Improving the delta.snow model, Master's Thesis,
University of Innsbruck.  
  
Winkler, M., Schellander, H., and Gruber, S. (2021): Snow water
equivalents exclusively from snow depths and their temporal changes: the
delta.snow model, Hydrol. Earth Syst. Sci., 25, 1165-1187, doi:
10.5194/hess-25-1165-2021.

## Author

Harald Schellander, Michael Winkler

## Examples

``` r
data(hsdata, package = "nixmass")

swe_dyn <- swe.delta.snow(hsdata)
swe <- swe.delta.snow(hsdata, dyn_rho_max = FALSE)
plot(seq_along(hsdata$date), swe_dyn, type = "l", ylab = "SWE (mm) / hs (cm)", xlab = "day")
lines(seq_along(hsdata$date), swe, type = "l", col = "red")
lines(seq_along(hsdata$date), hsdata$hs * 100, type = "l", lty = 2, col = "grey30")
legend(title = "delta.snow", "topleft", legend = c("SWE dyn", "SWE", "HS"),
 col = c("black", "red", "grey30"), lty = c(1, 1, 2))

```
