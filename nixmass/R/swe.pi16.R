#' Statistical SWE modeling depending on the day-of-year
#' 
#' This model parameterizes bulk snow density with day-of-the-year as the only input. It was calibrated for the region of South Tyrol, Italy, and is therefore called ST model in the original reference.
#'
#' @param data A data.frame with at least two columns named \code{date} and \code{hs}. 
#' They should contain date and corresponding daily observations of snow depth \eqn{hs \ge 0} 
#' measured at one site. The unit must be meters (m). No gaps or NA are allowed.
#' Dates must be either of class `character`, `Date` or `POSIXct` and given in the format 
#' \code{YYYY-MM-DD}. No sub-daily resolution is allowed at the moment (see details).
#' @param rho_0 Intercept of the linear regression between observed snow depths and SWE values. \code{rho_0} is set to 200 as default, which is the value from the original reference. It can however be set to any value according to regression modeling with other datasets.
#' @param K Slope of the linear regression between observed densities and the day-of-year as defined in the original reference. \code{K} is set to 1 as default, which is the value from the original reference. It can however be set to any value according to regression modeling with other datasets. 
#'
#' @details
#' \code{swe.pi16}{ This function uses only the day-of-year (DOY) as parameterization for bulk snow density and hence SWE. Here, the datums in the input data.frame are converted to DOY as defined in the original reference: negative values between 1.10. and 31.12. DOY=-92 at 1.10. In leap years 31.12. has DOY = 0, in non-leap years 31.12. has DOY = -1 with no day being 0. Non computable values are returned as NA.}
#' 
#' @return A vector with daily SWE values in mm.
#' @export
#' 
#' @references Pistocchi, A. (2016) 'Simple estimation of snow density in an Alpine region', Journal of Hydrology: Regional Studies. Elsevier B.V., 6(Supplement C), pp. 82 - 89. doi: 10.1016/j.ejrh.2016.03.004.
#' 
#' @examples
#' data(hsdata)
#' swe <- swe.pi16(hsdata)
#' summary(swe)
#' 
swe.pi16 <- function(data, rho_0=200, K=1){
  
  if(!inherits(data,"data.frame"))
    stop("swe.pi16: data must be given as data.frame")
  
  if(!all("hs" %in% colnames(data) & "date" %in% colnames(data)))
    stop("swe.pi16: data must contain at least two columns named 'hs' and 'date'")
  
  Hobs <- data$hs
  if(any(is.na(Hobs)))
    stop("swe.pi16: snow depth data must not be NA")
  if(!all(Hobs >= 0))
    stop("swe.pi16: snow depth data must not be negative")
  if(!is.numeric(Hobs))
    stop("swe.pi16: snow depth data must be numeric")
  
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
  # 
  # z <- zoo(Hobs,as.Date(data$date))
  # if(!is.regular(z, strict = TRUE))
  #   stop("swe.jonas: date column must be strictly regular")
  # 
  
  
  # dos: integer day from 1.9. - 31.8.}
  #dos <- ifelse( month(data$date)>8 & month(data$date) <=12,yday(data$date) - 243, yday(data$date) +122 )
  month <- as.numeric( format(as.POSIXct(data$date), "%m"))
  yday  <- as.POSIXlt(data$date, format="%Y-%m-%d")$yday + 1
  dos <- ifelse( month>8 & month<=12, yday-243, yday+122 )
  doy <- dos - 122
  # if(month(data$date) %in% c(10,11,12)){
  #      doy <- yday(as.Date(data$date))-366
  # } else if (month(data$date)>=7 & month(data$date)<=9) { # model isnt able to produce densities in summer 
  #      doy <- NA
  # } else {
  #      doy <- yday(data$date)
  # }
  #rho_0 <- 200                 # [kg/m3] bulk snow density at DOY = -62 (31 October)
  #K <- 1                       # [kg/m3/day] rate of (bulk snow) density increase
  rho <- rho_0 + K*(doy + 61)  # [kg/m3] bulk snow density
  swe <- rho*data$hs           # [kg/m2] snow water equivalent
  return(swe)
}
