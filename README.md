# nixmass

<!-- badges: start -->
<!--[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)-->
[![CRAN status](https://www.r-pkg.org/badges/version/nixmass)](https://CRAN.R-project.org/package=nixmass)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/nixmass?color=brightgreen)
![(https://img.shields.io/github/v/tag/haraldschellander/mevr)](https://img.shields.io/github/v/tag/haraldschellander/nixmass?include_prereleases)
[![Coverage](https://img.shields.io/codecov/c/github/haraldschellander/nixmass)](https://app.codecov.io/gh/haraldschellander/nixmass)
<!-- badges: end -->

R-package with functions for Snow Water Equivalent Modeling with the $\Delta\text{SNOW}$ Model and Empirical Regression Models.

Snow water equivalent is modeled with the process based $\Delta\text{SNOW}$ model and empirical regression models using relationships between density and diverse at-site parameters. The methods are described in [Winkler et al. (2021)](https://doi.org/10.5194/hess-25-1165-2021), [Guyennon et al. (2019)](https://doi.org/10.1016/j.coldregions.2019.102859), [Pistocchi (2016)](https://doi.org/10.1016/j.ejrh.2016.03.0049), [Jonas et al. (2009)](https://doi.org/10.1016/j.jhydrol.2009.09.021) and [Sturm et al. (2010)](https://doi.org/10.1175/2010JHM1202.1).

Note that there exists a very [similar model](https://github.com/jannefiluren/HS2SWE) utilizing the same principal ideas. 

## Installation

The easiest way to get nixmass is to install it from CRAN
```r 
install.packages("nixmass")
```


## Development version
To install the development version from GitHub

```r
# install.packages("pak")
pak::pak("haraldschellander/nixmass")
```
