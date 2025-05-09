#' Statistical SWE modeling based on a quadratic dependance on the day-of-year
#'
#' This model parameterizes bulk snow density with day-of-the-year as the only input similar to \code{\link{swe.pi16}} but adds a quadratic dependance. It was calibrated for the regions of the whole Italian alps, and the subregions South-West, Central and South-East. By setting the coefficients of the empirical regression it can however be used with results from other datasets.
#'
#' @param data A data.frame of daily observations with two columns named \emph{date} and \emph{hs}
#' referring to day and snow depth at that day. The date column can be of class `character`, `Date` or `POSIXct`
#' with the format \code{YYYY-MM-DD}. The hs column must be snow depth values \eqn{\ge 0} in m.
#' @param region.gu19 Must be one of the italian subalpine regions \emph{italy}, \emph{southwest}, \emph{central} or \emph{southeast}, defined in the original reference (see details), or \emph{myregion}, in which case the coefficients n0, n1 and n2 have to be set.
#' @param n0 Intercept of an empirical regression between densities and the day-of-year (see details).
#' @param n1 Slope of an empirical regression between densities and the day-of-year (see details).
#' @param n2 Quadratic dependence of an empirical regression between densities and the day-of-year (see details).
#'
#' @details
#' \code{swe.gu19} Similar to the model of Pistocchi (2016), this function uses only the day-of-year (DOY)
#' as parameterization for bulk snow density and hence SWE. In contrast to the latter, here, a quadratic term
#' for DOY was added, to reflect non-linearity in the snow bulk density variability.
#' The datums in the input data.frame are converted to DOY as days spent since November 1st.
#' Regression coefficients depend on regions defined in Guyennon et al. (2019), which are \emph{italy}
#' for the Italian Alps, \emph{southwest} for the South-western Italian Alps, \emph{central} for
#' the Central Italian Alpes or \emph{southeast} for the South-western Italian Alps.
#'
#' If \code{region.gu19} is set to \emph{myregion}, the coefficients \code{no}, \code{n1} and \code{n2}
#' must be set to values, obtained from a regression between densities and day-of-year from another dataset.
#' It has to have the form density ~ DOY + DOY^2, where DOY is the day-of-year as defined in the original reference.
#' Non computable values are returned as NA.
#'
#' @return A vector with daily SWE values in mm.
#' @export
#'
#' @references
#' Guyennon, N., Valt, M., Salerno, F., Petrangeli, A., Romano, E. (2019) 'Estimating the snow water equivalent from snow depth measurements in the Italian Alps', Cold Regions Science and Technology. Elsevier, 167 (August), p. 102859. doi: 10.1016/j.coldregions.2019.102859.
#'
#' Pistocchi, A. (2016) 'Simple estimation of snow density in an Alpine region', Journal of Hydrology: Regional Studies. Elsevier B.V., 6 (Supplement C), pp. 82 - 89. doi: 10.1016/j.ejrh.2016.03.004.
#' @examples
#' data(hsdata)
#' swe <- swe.gu19(hsdata, region = "italy")
#' summary(swe)
swe.gu19 <- function(data, region.gu19, n0 = NA, n1 = NA, n2 = NA) {
  if (!inherits(data, "data.frame"))
    stop("swe.gu19: data must be given as data.frame")

  if (!all("hs" %in% colnames(data) & "date" %in% colnames(data)))
    stop(
      "swe.gu19: data must contain at least two columns named 'hs' and 'date'"
    )

  Hobs <- data$hs
  if (any(is.na(Hobs))) stop("swe.gu19: snow depth data must not be NA")
  if (!all(Hobs >= 0)) stop("swe.gu19: snow depth data must not be negative")
  if (!is.numeric(Hobs)) stop("swe.gu19: snow depth data must be numeric")

  if (inherits(data$date, "character")) {
    if (any(is.na(as.POSIXlt(data$date, format = "%Y-%m-%d")))) {
      stop("date format must be '%Y-%m-%d'")
    } else {
      data$date <- as.Date(data$date)
    }
  } else if (inherits(data$date, "Date") | inherits(data$date, "POSIXct")) {
    if (any(is.na(as.POSIXlt(data$date, format = "%Y-%m-%d"))))
      stop("date format must be '%Y-%m-%d'")
  } else {
    stop("date column must be either of class 'character', 'Date' or 'POSIXct'")
  }

  # check regions
  if (missing(region.gu19)) stop("swe.gu19: region.gu19 must be given")
  if (
    !is.element(
      region.gu19,
      c("italy", "southwest", "central", "southeast", "myregion")
    )
  )
    stop(
      "swe.gu19: region.gu19 must be one of 'italy','southwest','central','southeast','myregion'"
    )

  guyennon.coeff <- list(
    "italy" = c(n0 = 294, n1 = -8.3e-1, n2 = 7.7e-3),
    "southwest" = c(n0 = 285.9, n1 = 1.3e-1, n2 = -0.1e-3),
    "central" = c(n0 = 288.9, n1 = -9.3e-1, n2 = 8.2e-3),
    "southeast" = c(n0 = 332.5, n1 = -16.8e-1, n2 = 13.5e-3),
    "myregion" = c(n0 = n0, n1 = n1, n2 = n2)
  )

  # dos: integer day from 1.9. - 31.8.}
  #dos <- ifelse( month(data$date)>8 & month(data$date) <=12, yday(data$date) - 243, yday(data$date) +122 )
  month <- as.numeric(format(as.POSIXct(data$date), "%m"))
  yday <- as.POSIXlt(data$date, format = "%Y-%m-%d")$yday + 1
  dos <- ifelse(month > 8 & month <= 12, yday - 243, yday + 122)
  doy <- dos - 122 # day of year

  # doy...days since 1.11.
  d <- data$date
  doys <- c()
  for (i in 1:nrow(data)) {
    # if(month(d[i]) %in% c(8,9,10,11,12)){
    #      doy <- as.integer( difftime(d[i],paste0(year(d[i]),"-11-01"), units="days") )
    # } else {
    #      doy <- as.integer( difftime(d[i],paste0(year(d[i])-1,"-11-01"), units="days") )
    # }
    m <- as.numeric(format(as.POSIXct(d[i]), "%m"))
    yd <- as.POSIXlt(d[i], format = "%Y-%m-%d")$yday + 1
    if (m %in% c(10, 11, 12)) {
      doy <- yd - 366
    } else if (m >= 7 & m <= 9) {
      # model isnt able to produce densities in summer
      doy <- NA
    } else {
      doy <- yd
    }

    doys <- c(doys, doy)
  }

  if (region.gu19 == "myregion" & any(is.na(c(n0, n1, n2)))) {
    stop("swe.gu19: at least one of the coefficients n0, n1, n2 is NULL")
  }

  n0 <- guyennon.coeff[[region.gu19]][1]
  n1 <- guyennon.coeff[[region.gu19]][2]
  n2 <- guyennon.coeff[[region.gu19]][3]
  rho <- n0 + n1 * (doys + 61) + n2 * (doys + 61)^2 # [kg/m3] bulk snow density
  swe <- rho * data$hs # [kg/m2] snow water equivalent
  return(swe)
}
