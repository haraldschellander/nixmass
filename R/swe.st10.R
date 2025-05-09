#' Statistical SWE modeling depending on day of year and a climatic region
#'
#' The \emph{Sturm} model parameterizes bulk snow density with day of the year and a \emph{snowclass.st10}. It was trained on historical snow depth - density - SWE data from the United States, Canada, and Switzerland.
#'
#' @param data A data.frame with at least two columns named \code{date} and \code{hs}.
#' They should contain date and corresponding daily observations of snow depth \eqn{hs \ge 0}
#' measured at one site. The unit must be meters (m). No gaps or NA are allowed.
#' Dates must be either of class `character`, `Date` or `POSIXct` and given in the format
#' \code{YYYY-MM-DD}. No sub-daily resolution is allowed at the moment (see details).
#' @param snowclass.st10 Must be one of the following character strings: "alpine","maritime","prairie","tundra","taiga".
#'
#' @details
#' This model converts snow depth to SWE using snow depth, day of year and station location (from which a climate class of snow can be inferred. The day of year (DOY) is the day-number of in the season 1.10. - 30.6. The 1.10. refers to DOY = -92. The 1.2. would be DOY = 32, while 15.11. would be DOY = -47. The \emph{snowclass.st10} must be one out of the character strings "alpine","maritime","prairie","tundra" and "taiga".
#' For the Alps probably "alpine" would be the most appropriate climate classification.
#' Non computable values are returned as NA.
#'
#' @return A vector with daily SWE values in mm.
#'
#' @references Sturm, M. et al. (2010) 'Estimating Snow Water Equivalent Using Snow Depth Data and Climate Classes', Journal of Hydrometeorology, 11(6), pp. 1380 - 1394. doi: 10.1175/2010JHM1202.1.
#' @export
#'
#' @examples
#' data(hsdata)
#' swe <- swe.st10(hsdata)
#' summary(swe)
#'
swe.st10 <- function(
  data,
  snowclass.st10 = c("alpine", "maritime", "prairie", "tundra", "taiga")
) {
  if (!inherits(data, "data.frame"))
    stop("swe.st10: data must be given as data.frame")

  if (!all("hs" %in% colnames(data) & "date" %in% colnames(data)))
    stop(
      "swe.st10: data must contain at least two columns named 'hs' and 'date'"
    )

  Hobs <- data$hs * 100 # hs must be in [cm]
  if (any(is.na(Hobs))) stop("swe.st10: snow depth data must not be NA")
  if (!all(Hobs >= 0)) stop("swe.st10: snow depth data must not be negative")
  if (!is.numeric(Hobs)) stop("swe.st10: snow depth data must be numeric")

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

  #-----------------------------------------------------------------------
  snowclass.st10 <- match.arg(snowclass.st10)
  #if(length(snowclass.st10) == 0 | length(snowclass.st10) > 1)
  #  stop("'arg' should be one of 'alpine','maritime','prairie','tundra','taiga'")

  sturm.params <- data.frame(
    "den.max" = c(0.5975, 0.5979, 0.5940, 0.3630, 0.2170),
    "den0" = c(0.2237, 0.2578, 0.2332, 0.2425, 0.2170),
    "k1" = c(0.0012, 0.0010, 0.0016, 0.0029, 0.0000),
    "k2" = c(0.0038, 0.0038, 0.0031, 0.0049, 0.0000)
  )
  row.names(sturm.params) <- c(
    "alpine",
    "maritime",
    "prairie",
    "tundra",
    "taiga"
  )

  d <- as.Date(data$date)
  doys <- c()
  for (i in 1:nrow(data)) {
    if (month(d[i]) %in% c(10, 11, 12)) {
      doy <- yday(as.Date(d[i])) - 366
    } else if (month(d[i]) >= 7 & month(d[i]) <= 9) {
      # model isnt able to produce densities in summer
      doy <- NA
    } else {
      doy <- yday(as.Date(d[i]))
    }
    doys <- c(doys, doy)
  }

  #-----------------------------------------------------------------------
  calc.swe <- function(x, snowclass.st10) {
    hs <- x[1]
    doy <- x[2]
    den.max <- sturm.params[snowclass.st10, ]$den.max
    den0 <- sturm.params[snowclass.st10, ]$den0
    k1 <- sturm.params[snowclass.st10, ]$k1
    k2 <- sturm.params[snowclass.st10, ]$k2
    bd <- ifelse(
      is.na(doy),
      NA,
      (den.max - den0) * (1 - exp(-k1 * hs - k2 * doy)) + den0
    ) # g/cm3
    swe <- ifelse(hs == 0, 0, bd * hs * 10) # mm or kg/m2
    return(swe)
  }
  df <- data.frame(hs = Hobs, doy = doys)
  swe <- apply(df, 1, calc.swe, snowclass.st10)
  return(swe)
}
