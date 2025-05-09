# nixmass

<!-- badges: start -->
<!--[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)-->
[![CRAN status](https://www.r-pkg.org/badges/version/nixmass)](https://CRAN.R-project.org/package=nixmass)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/nixmass?color=brightgreen)
![(https://img.shields.io/github/v/tag/haraldschellander/mevr)](https://img.shields.io/github/v/tag/haraldschellander/nixmass?include_prereleases)
[![Coverage](https://img.shields.io/codecov/c/github/haraldschellander/nixmass)](https://app.codecov.io/gh/haraldschellander/nixmass)
<!-- badges: end -->

R-package with functions for Snow Water Equivalent Modeling with the $\Delta\text{SNOW}$ Model, the HS2SWE Model and some Empirical Regression Models.

Snow water equivalent can be modeled either with the process based models $\Delta\text{SNOW}$ and HS2SWE, or with empirical regression models, which use relationships between density and diverse at-site parameters. 
The $\Delta\text{SNOW}$ model is described in [Winkler et al. (2021)](https://doi.org/10.5194/hess-25-1165-2021), 
the HS2SWE model is described in [Magnusson et al. (2025)](https://doi.org/10.1016/j.coldregions.2025.104435),
the empirical regression models in [Guyennon et al. (2019)](https://doi.org/10.1016/j.coldregions.2019.102859), [Pistocchi (2016)](https://doi.org/10.1016/j.ejrh.2016.03.0049), [Jonas et al. (2009)](https://doi.org/10.1016/j.jhydrol.2009.09.021) and [Sturm et al. (2010)](https://doi.org/10.1175/2010JHM1202.1).

<!-- Note that there exists a very [similar model](https://github.com/jannefiluren/HS2SWE) which has many ideas in common with the $\Delta\text{SNOW}$ model. -->


## Installation

The easiest way to get nixmass is to install it from CRAN. 
Note however that this version already lacks some new features and bug fixes.
```r 
install.packages("nixmass")
```


## Development version
To install the development version from GitHub

```r
# install.packages("pak")
pak::pak("haraldschellander/nixmass")
```
