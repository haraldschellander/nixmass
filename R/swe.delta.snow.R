utils::globalVariables(c("hs"))

#' SWE modeling from daily snow depth differences
#' 
#' @description Model daily values of Snow Water Equivalent solely from daily differences of snow depth.
#' 
#' \code{swe.delta.snow} computes SWE solely from daily changes of snow depth 
#' at an observation site. \cr
#' Compression of a snow layer without additional load on top is computed on 
#' the basis of Sturm and Holmgren (1998), who regard snow as a viscous fluid: \cr
#' \deqn{\rho_i(t_{i+1}) = \rho_i(t_i)*(1+(SWE*g)/\eta_0 * exp^{-k_2*\rho_i(t_i)})}
#' with \eqn{\rho_i(t_{i+1}) and \rho_i(t_i)} being tomorrow's and today's 
#' respective density of layer i, the gravitational acceleration 
#' \eqn{g = 9.8ms^{-2}}, viscosity \eqn{\eta_0} (Pa) and 
#' factor \eqn{k2 [m^3kg^{-1}}], determining the importance 
#' of today's for tomorrow's density.
#' 
#' @param data A data.frame with at least two columns named \code{date} and \code{hs}. 
#' They should contain date and corresponding daily observations of snow depth \eqn{hs \ge 0} 
#' measured at one site. The unit must be meters (m). No gaps or NA are allowed.
#' Dates must be either of class `character`, `Date` or `POSIXct` and given in the format 
#' \code{YYYY-MM-DD}. No sub-daily resolution is allowed at the moment (see details).
#' Note that hs has to start with zero.
#' @param model_opts An empty list which can be populated with model coefficients 
#' specific to the original delta.snow model (Winkler et al., 2021) or a version, where 
#' the maximum layer and bulk snow densities are allowed to vary with age (see details).
#' @param dyn_rho_max Logical. If `TRUE`, the maximum bulk snow density is allowed to vary with layer age (see details).
#' if `FALSE`, the original delta.snow model is used.
#' @param layers Should parameters snow depth, swe and age be returned layerwise? Can be \code{TRUE} or \code{FALSE}.
#' @param verbose Should additional information be given during runtime? Can be \code{TRUE} or \code{FALSE}.
#'
#' @details
#' If \code{dyn_rho_max=TRUE}, the bulk snow density varies with layer age. As activation function, 
#' `atan` is used, where the S-curve symmetrically transitions from the lower to the upper density bound. 
#' In that case, \code{model_opts} are extended by a lower density bound \code{rho_l}, 
#' an upper density bound \code{rho_h}, a slope \code{sigma} and a midpoint \code{mu}, 
#' which have been found via an optimization procedure (Winkler et al., 2021). 
#' Be aware that also the other model coefficients do slightly change.   
#' 
#' The following model coefficients must be provided:
#' 
#' \code{dyn_rho_max=FALSE}: 
#'  * \code{rho.max} Maximum density of an individual snow layer produced by 
#'  the delta.snow model (kg/m3), \eqn{rho.max > 0}
#'  * \code{rho.null} Fresh snow density for a newly created layer (kg/m3), \eqn{rho.null > 0}. 
#'  Currently optimized for daily snow depth observations.
#'  * \code{c.ov} Overburden factor due to fresh snow (-), \eqn{c.ov > 0}
#'  * \code{k.ov} Defines the impact of the individual layer density on the 
#'  compaction due to overburden (-), \eqn{k.ov \in [0,1]}.
#'  * \code{k} Exponent of the exponential-law compaction (m3/kg), \eqn{k > 0}.
#'  * \code{tau} Uncertainty bound (m), \eqn{tau > 0}.
#'  * \code{eta.null} Effective compactive viscosity of snow for "zero-density" (Pa s).
#'  * \code{timestep} Timestep between snow depth observations in hours. Default is 24 hours, i.e. daily snow depth observations.
#' No sub-daily values are allowed at the moment (see details).
#' 
#' \code{dy_rho_max=TRUE}: 
#' 
#' Instead of a constant coefficient for \code{rho.max}, these four
#' parameters describe the smooth S-curve approximated by the `atan` trigonometric function. 
#' * \code{sigma} Steepness or slope of `atan` at its midpoint \code{mu}, (-), \eqn{sigma > 0}.
#' * \code{mu} Central midpoint in days, where the steepest transition occurs (days), \eqn{mu > 0}.
#' * \code{rho_h} Upper density bound for a single layer and the whole snow pack (kg/m3), \eqn{rho_h > 0}. 
#' * \code{rho_l} Lower density bound for a single layer and the whole snow pack, where the transition begins 
#' (kg/m3), \eqn{rho_l > 0}. 
#' 
#' All other coefficients are needed as well. Be aware however that they are slightly different.
#' 
#' The easiest way to call the original delta.swe model is \code{swe.delta.snow(hsdata, dyn_rho_max = FALSE)}.
#' Note that parameters intrinsic to the dynamic density model provided with the original model 
#' are silently ignored. 
#' 
#' In principal, the model is able to cope with a sub-daily temporal resolution, 
#' e.g. hourly snow depth observations. However, the model was fitted to daily observations, 
#' and the model parameter \code{rho.null} reflects that. In other words, if the observation frequency changes, 
#'  \code{rho.null} should change as well. Currently, no sub-daily resolution is allowed.
#' 
#' 
#' @md
#' @return If \code{layears = FAlSE}, a vector with daily SWE values in mm. If \code{layers=TRUE}, a list with layerwise matrices 
#' of the parameters h (snow depth), swe and age is returned additionally to the SWE vector. The matrix holds `dates` on the x-axis and 
#' `layers` on the y-axis. swe is in mm, h in m and age in days. 
#'  I
#' @export
#' 
#' @references Gruber, S. (2014) "Modelling snow water equivalent based on daily snow depths", Masterthesis, Institute for Atmospheric and Cryospheric Sciences, University of Innsbruck.
#' \cr\cr
#' Martinec, J., Rango, A. (1991) "Indirect evaluation of snow reserves in mountain basins". Snow, Hydrology and Forests in High Alpine Areas. pp. 111-120.
#' \cr\cr
#' Sturm, M., Holmgren, J. (1998) "Differences in compaction behavior of three climate classes of snow". Annals of Glaciology 26, 125-130.
#' \cr\cr
#' Winkler, M., Schellander, H., and Gruber, S.: Snow water equivalents exclusively from snow depths and their temporal changes: the delta.snow model, Hydrol. Earth Syst. Sci., 25, 1165-1187, doi: 10.5194/hess-25-1165-2021, 2021.
#' \cr\cr
#' Schroeder, M.et al. (2024) "CONTINUOUS SNOW WATER EQUIVALENT MONITORING ON GLACIERS USING COSMIC RAY NEUTRON SENSOR TECHNOLOGY A CASE STUDY ON HINTEREISFERNER, AUSTRIA", Proceedings: International Snow Science Workshop 2024, Tromsø, Norway, 1107 - 1114
#'
#' @author Harald Schellander, Michael Winkler

#' @examples
#' data(hsdata, package = "nixmass")
#' 
#' swe_dyn <- swe.delta.snow(hsdata)
#' swe <- swe.delta.snow(hsdata, dyn_rho_max = FALSE)
#' plot(seq_along(hsdata$date), swe_dyn, type = "l", ylab = "SWE (mm) / hs (cm)", xlab = "day")
#' lines(seq_along(hsdata$date), swe, type = "l", col = "red") 
#' lines(seq_along(hsdata$date), hsdata$hs * 100, type = "l", lty = 2, col = "grey30") 
#' legend(title = "delta.snow", "topleft", legend = c("SWE dyn", "SWE", "HS"), 
#'  col = c("black", "red", "grey30"), lty = c(1, 1, 2))
#' 
swe.delta.snow <- function(data, model_opts = list(), dyn_rho_max = TRUE, layers = FALSE, verbose = FALSE) {
  
  if (dyn_rho_max) {
    model_opts_defaults <- list(
      sigma = 0.02986102, mu = 148.3291, rho_h = 588.6178, rho_l = 369.0934,
      rho.null = 80.73706, c.ov = 0.0005170964, k.ov = 0.3782312,
      k = 0.029297, tau = 0.02356521, eta.null = 8543502, timestep = 24
    )
  } else {
    model_opts_defaults <- list(
      rho.max = 401.2588, rho.null = 81.19417, c.ov = 0.0005104722,
      k.ov = 0.37856737, k = 0.02993175, tau = 0.02362476, eta.null = 8523356, timestep = 24
    )   
  }
  model_opts <- utils::modifyList(model_opts_defaults, model_opts)
  
 
  #---------------------------------------------------------------------
  # general checks
  
  if (!inherits(data, "data.frame"))
    stop("data must be of class 'data.frame'")
  
  if (!all((is.element(colnames(data), c("hs", "date")))))
    stop("data must contain at least two columns named 'date' and 'hs'") 
  
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
  
  if(length(which(data$hs < 0)) > 0)
    stop("data must not contain values < 0")
  
  if (any(is.na(data$hs)))
    stop("data must not contain NA")
  
  if (!is.numeric(data$hs))
    stop("snow depth data must be numeric")
  
  # first_hs <- data |> 
  #   dplyr::arrange(as.Date(date)) |> 
  #   dplyr::pull(hs) |> 
  #   head(1)
  first_hs <- data$hs[which.min(as.Date(data$date))]
  if (first_hs != 0)
    stop("snow depth observations must start with 0")
  
  z <- zoo(data$hs, as.Date(data$date))
  if (!is.regular(z, strict = TRUE))
    stop("snow depth data must be strictly regular")
  
  #---------------------------------------------------------------------
  # parameter checks
  
  if (dyn_rho_max) {
    if (model_opts$sigma <= 0)
      stop("'sigma' must not be negative or 0")  
    if (model_opts$mu <= 0)
      stop("'mu' must not be negative or 0")  
    if (model_opts$rho_h <= 0)
      stop("'rho_h' must not be negative or 0")  
    if (model_opts$rho_l <= 0)
      stop("'rho_l' must not be negative or 0")  
  } else {
  if (model_opts$rho.max <= 0)
    stop("'rho.max' must not be negative or 0")
  }
  
  if (model_opts$rho.null <= 0)
    stop("'rho.null' must not be negative or 0")
  if (model_opts$c.ov <= 0)
    stop("'c.ov' must not be negative or 0")
  if (model_opts$k.ov < 0)
    stop("'k.ov' must be > 0")
  if (model_opts$k.ov > 1)
    stop("'k.ov' must be < 1")
  if (model_opts$k <= 0)
    stop("'k' must not be negative or 0")
  if (model_opts$tau <= 0)
    stop("'tau' must not be negative or 0")
 
  # check timestep vs data
  if (model_opts$timestep < 24)
    stop("timestep must be >= 24 hours")
  d_secs <- abs(as.numeric(as.POSIXct(data$date[2])) - as.numeric(as.POSIXct(data$date[1])))
  if (d_secs / 60 / 60  != model_opts$timestep) 
    stop("provided timestep does not fit your data")
  
  
  nixmass.e <- new.env()
  nixmass.e$snowpack.dd <- NULL
  
  Hobs     <- data$hs                           # observed snow depths [m]
  H        <- numeric(length = length(Hobs))    # modeled total depth of snow at any day [m]
  SWE      <- numeric(length = length(Hobs))    # modeled total SWE at any day [kg/m2]
  ly       <- 1                                 # layer number [-]
  ts       <- model_opts$timestep * 3600                   # timestep between observations [s]
  g        <- 9.81                              # gravity [ms-2]
  
  # preallocate some variables   
  # one row for first layer and 3 cols for timestep-1, timestep, timestep+1
  h        <- matrix(0, 1, 3)                     # modeled depth of snow in all layers [m]
  swe      <- matrix(0, 1, 3)                     # modeled swe in all layers [kg/m2]
  age      <- matrix(0, 1, 3)                     # age of modeled layers [days]
  day.tot  <- length(Hobs)                        # total days from first to last snowfall [-]
  
  
  # helper
  if (verbose) m <- rep("", day.tot)            # vector of (verbose) messages
  prec     <- 10^-10                           # precision for arithmetic comparisons [-]
  
  if (layers) {                                   # return parameters layerwise
       h_layers <- list()
       swe_layers <- list()
       age_layers <- list()
  }
  
  #-------------------------------------------------------------------------
  # dynamic rho.max
  #-------------------------------------------------------------------------
  rho_max_dyn <- function(t){
    atan(model_opts$sigma * (t - model_opts$mu)) * (model_opts$rho_h - model_opts$rho_l) / pi + (model_opts$rho_h + model_opts$rho_l) / 2
  }
  
  
  #-------------------------------------------------------------------------
  # compaction of snowpack
  #-------------------------------------------------------------------------
  dry_compaction <- function(h.d, swe.d, age.d, ly.tot, ly, k, rho.max, ts, prec, g){
    # .d  -> today
    # .dd -> tomorrow
    # compute overburden for each layer
    # the overburden for the first layer is the layer itself
    swe.hat.d <- numeric(length = length(ly.tot))
    for(i in 1:ly.tot){
      swe.hat.d[i] <- sum(swe.d[i:ly.tot])
    }
    
    snowpack.d   <- data.frame(h  = h.d, swe = swe.d, swe.hat = swe.hat.d, age = age.d)
    H.d          <- sum(snowpack.d$h)
    
    a <- data.frame(t(apply(snowpack.d[(1:ly),], 1, compactH, H.d, k, rho.max, ts, prec, g, model_opts$eta.null)))
    b <- data.frame(rep(0,ly.tot-ly),rep(0,ly.tot-ly),rep(0,ly.tot-ly),rep(0,ly.tot-ly))
    colnames(a) <- colnames(b) <- c("h", "swe", "age", "rho")
    nixmass.e$snowpack.dd <<- rbind(a, b)
    rownames(nixmass.e$snowpack.dd) <<- paste0("dd.layer", 1:nrow(nixmass.e$snowpack.dd))
    return(nixmass.e$snowpack.dd)
  }
  
  
  
  #-------------------------------------------------------------------------
  # compaction of individual snow layers without additional load 
  # today's values are compacted to tomorrow's values
  #-------------------------------------------------------------------------
  compactH <- function(x, H.d, k, rho.max, ts, prec, g, eta.null){
    # .d  -> today
    # .dd -> tomorrow
    age.d <- ifelse(x[1] == 0, 0, x[4])
    if (dyn_rho_max)
      rho.max.d <- rho_max_dyn(age.d)
    h.dd <- x[1]/(1 + (x[3] * g * ts)/eta.null * exp(-k * x[2]/x[1]))
    h.dd <- ifelse(x[2]/h.dd > rho.max.d, x[2]/rho.max.d, h.dd)
    h.dd <- ifelse(x[1] == 0, 0, h.dd)
    swe.dd  <- x[2]
    age.dd  <- ifelse(x[1] == 0, 0, age.d + 1)
    rho.dd  <- ifelse(x[1] == 0, 0, swe.dd/h.dd)
    rho.dd  <- ifelse(rho.max.d - rho.dd < prec, rho.max.d, rho.dd)
    return(cbind(h = h.dd, swe = swe.dd, age = age.dd, rho = rho.dd))
  } 
  
  
  
  #-------------------------------------------------------------------------
  # assigns snowpack properties of the next timestep
  #-------------------------------------------------------------------------
  assignH <- function(sp.dd, h, swe, age, H, SWE, t, day.tot){
    if(t < day.tot){
      # next timestep is always 3                
      h[,3]    <- sp.dd$h
      swe[,3]  <- sp.dd$swe
      age[,3]  <- sp.dd$age
      H[t+1]   <- sum(h[,3])
      SWE[t+1] <- sum(swe[,3])
    }
    return(list(h=h, swe=swe, age=age, H=H, SWE=SWE))
  }
  
  
  
  drenchH <- function(t, ly, ly.tot, day.tot, Hobs, h, swe, age, H, SWE, rho.max, c.ov, k.ov, k, ts, prec, m){
    if (verbose) msg(m, t, paste("melt "))
    
    Hobs.d = Hobs[t]
    h.d = h[, 2]
    swe.d = swe[, 2]
    age.d = age[, 2]
    
    runoff <- 0
    
    # distribute mass top-down
    for(i in ly:1){
      if (dyn_rho_max)
        rho.max <- rho_max_dyn(age.d[i])
      if( sum(h.d[-i]) + swe.d[i]/rho.max - Hobs.d >= prec ){
        # layers is densified to rho.max
        h.d[i] <- swe.d[i]/rho.max  
      } else {
        # layer is densified as far as possible
        # but doesnt reach rho.max
        h.d[i] <- swe.d[i]/rho.max + abs(sum(h.d[-i]) + swe.d[i]/rho.max - Hobs.d) 
        break
      }
      
    }
    
    # all layers have rho.max
    if (dyn_rho_max)
      rho.max <- rho_max_dyn(age.d[1:ly])
    if( all(rho.max - swe.d[1:ly]/h.d[1:ly] <= prec) ){
      if (verbose) msg(m,t,paste("no further compaction "))
      
      # produce runoff if sum(h.d) - Hobs.d is still > 0
      if (verbose) msg(m,t,paste("runoff "))
      scale <- Hobs.d/sum(h.d)
      if (dyn_rho_max) {
        swe_total.d <- c()
        hobs.d <- c()
        runoff.d <- c()
        rho.max.d <- c()
        for(i in 1:ly){
          rho.max.d[i] <- rho_max_dyn(age.d[i])
          swe_total.d[i] <- h.d[i] * rho.max.d[i]
          hobs.d[i] <- Hobs.d * h.d[i]/sum(h.d)
          runoff.d[i] <- swe_total.d[i] - hobs.d[i] * rho.max.d[i]
        }
        runoff <- sum(runoff.d) 
        h.d <- h.d * scale                       # all layers are compressed (and have rho.max) [m]
        swe.d[1:ly] <- h.d[1:ly] * rho.max.d[1:ly]
      } else {
        runoff <- (sum(h.d) - Hobs.d) * rho.max  # excess is converted to runoff [kg/m2]
        h.d <- h.d * scale                       # all layers are compressed (and have rho.max) [m]
        swe.d <- swe.d * scale
      }
      
      
    } else {
      if (verbose) msg(m,t,paste("compaction "))
    }
    
    h[,2]   <- h.d
    swe[,2] <- swe.d
    age[,2] <- age.d
    H[t]    <- sum(h[,2])
    SWE[t]  <- sum(swe[,2])
    ly.tot  <- nrow(h)
    
    # no further compaction possible
    snowpack.tomorrow <- dry_compaction(h[,2], swe[,2], age[,2], ly.tot, ly, k, rho.max, ts, prec, g)
    
    # set values for next day
    rl  <- assignH(snowpack.tomorrow, h, swe, age, H, SWE, t, day.tot)
    h   <- rl$h
    swe <- rl$swe
    age <- rl$age
    H   <- rl$H
    SWE <- rl$SWE
    
    return(list(h=h, swe=swe, age=age, H=H, SWE=SWE))
  }
  
  scaleH <- function(t, ly, ly.tot, day.tot, deltaH, Hobs, h, swe, age, H, SWE, rho.max, k, ts, prec, m){
    # re-compact snowpack from previous timesteps values with adapted eta
    # .d  -> yesterday
    # .dd -> today
    # previous timestep is always 1
    Hobs.d  <- Hobs[t-1]
    Hobs.dd <- Hobs[t]
    h.d     <- h[,1]
    swe.d   <- swe[,1]
    age.d   <- age[,2]  
    if (dyn_rho_max)
      rho.max <- rho_max_dyn(age.d)

    # todays overburden   
    swe.hat.d <- numeric(length = length(ly.tot))
    for(i in 1:ly.tot){
      swe.hat.d[i] <- sum(swe.d[i:ly.tot])
    }
    
    # analytical solution for layerwise adapted viskosity eta
    # assumption: recompaction ~ linear depth change of yesterdays layers (see paper)
    eta.cor <- c()
    for(i in 1:ly.tot){
      rho.d <- swe.d[i]/h.d[i]
      x <- ts * g * swe.hat.d[i] * exp(-k*rho.d) # yesterday
      P <- h.d[i]/Hobs.d # yesterday
      eta.i <- Hobs.dd * x * P / (h.d[i] - Hobs.dd * P)
      eta.cor <- c(eta.cor, ifelse(is.na(eta.i), 0, eta.i))
    }
    
    # compute H of today with corrected eta
    # so that modeled H = Hobs
    h.dd.cor <- h.d/(1 + (swe.hat.d * g * ts)/eta.cor * exp(-k * swe.d/h.d)) 
    h.dd.cor[which(is.na(h.dd.cor))] <- 0
    H.dd.cor <- sum(h.dd.cor)
    
    # and check, if Hd.cor is the same as Hobs.d
    if(abs(H.dd.cor - Hobs.dd) > prec)
      warning(paste0("day ",t,": error in exponential re-compaction: H.dd.cor-Hobs.dd=",H.dd.cor - Hobs.dd))
    
    
    # which layers exceed rho.max?
    idx.max <- which(swe.d/h.dd.cor - rho.max > prec)
    
    if(length(idx.max) > 0){
      
      if(length(idx.max) < ly){
        # collect excess swe in those layers
        if (dyn_rho_max) {
          swe.excess <- swe.d[idx.max]-h.dd.cor[idx.max]*rho.max[idx.max]
        } else{
          swe.excess <- swe.d[idx.max]-h.dd.cor[idx.max]*rho.max
        }
        
        # set affected layer(s) to rho.max
        swe.d[idx.max] <- swe.d[idx.max] - swe.excess             
        
        # distribute excess swe to other layers top-down
        lys <- 1:ly
        lys <- lys[-idx.max]
        i <- lys[length(lys)]
        swe.excess.all <- sum(swe.excess)
        while(swe.excess.all > 0){
          if (dyn_rho_max) {
            swe.res <- h.dd.cor[i] * rho.max[i] - swe.d[i] # layer tolerates this swe amount to reach rho.max
          } else {
            swe.res <- h.dd.cor[i] * rho.max - swe.d[i] # layer tolerates this swe amount to reach rho.max  
          }
          
          if(swe.res > swe.excess.all){
            swe.res <- swe.excess.all
          }
          swe.d[i]  <- swe.d[i] + swe.res
          swe.excess.all  <- swe.excess.all - swe.res
          i <- i - 1
          if(i<=0 & swe.excess.all > 0){
            if (verbose) msg(m,t,paste(" runoff"))
            break
          }
        }
      } else {
        # if all layers have density > rho.max
        # remove swe.excess from all layers (-> runoff)
        # (this sets density to rho.max)
        swe.excess <- swe.d[idx.max]-h.dd.cor[idx.max]*rho.max[idx.max]
        swe.d[idx.max] <- swe.d[idx.max] - swe.excess
        if (verbose) msg(m,t,paste(" runoff"))
      }
    }
    
    h[,2]   <- h.dd.cor
    swe[,2] <- swe.d
    age[,2] <- age.d
    H[t]    <- sum(h[,2])
    SWE[t]  <- sum(swe[,2])
    ly.tot  <- nrow(h)
    
    # compact actual day 
    # if all layers already have maximum density rho.max
    # the snowpack will not be changed by the following step
    snowpack.tomorrow <- dry_compaction(h[,2], swe[,2], age[,2], ly.tot, ly, k, rho.max, ts, prec, g)
    
    # set values for next day
    rl  <- assignH(snowpack.tomorrow, h, swe, age, H, SWE, t, day.tot)
    h   <- rl$h
    swe <- rl$swe
    age <- rl$age
    H   <- rl$H
    SWE <- rl$SWE
    
    return(list(h=h, swe=swe, age=age, H=H, SWE=SWE))
    
  }
  
  
  #-------------------------------------------------------------------------
  # keep track of messages in a vector
  #-------------------------------------------------------------------------
  msg <- function(m,t,strg){
    cat(paste(strg))
    if(is.null(m[t])){
      m[t] <- strg  
    } else {
      m[t] <- paste(m[t],strg)
    }
  }
  
  
  if(verbose){
    cat("Using parameters:\n")
    print(unlist(model_opts))
  }
  
  for (t in 1:day.tot) {
    
    if (verbose) msg(m, t, paste0("day ", t, " (", data$date[t], "): "))
    
    # shift temporary matrices one step back in time
    h[, 1] <- h[, 2]
    h[, 2] <- h[, 3]
    h[, 3] <- 0
    swe[, 1] <- swe[, 2]
    swe[, 2] <- swe[, 3]
    swe[, 3] <- 0
    age[, 1] <- age[, 2]
    age[, 2] <- age[, 3]
    age[, 3] <- 0
    
    # snowdepth = 0, no snow cover
    if ( Hobs[t] == 0 ) {      
      if (t > 1){
        if (Hobs[t-1] == 0) {
          if (verbose) msg(m, t, paste0(""))        
        } else {
          if (verbose) msg(m, t, paste0("runoff"))
        }        
      } else {
        if (verbose) msg(m, t, paste0(""))         
      }
      H[t]    <- 0
      SWE[t]  <- 0
      h[,2]   <- 0
      swe[,2] <- 0
      
      if (layers) {
           h_layers[[t]] <- 0
           swe_layers[[t ]] <- 0 
           age_layers[[t]] <- 0  
      }
      
      # there is snow
    } else if (Hobs[t] > 0 ) {
      
      # first snow in/during season
      if ( Hobs[t-1] == 0 ) {
        ly <- 1
        if (verbose) msg(m,t,paste("produce layer",ly))
        age[ly,2] <- 1
        h[ly,2]   <- Hobs[t]
        H[t]      <- Hobs[t]
        swe[ly,2] <- model_opts$rho.null * Hobs[t]
        SWE[t]    <- swe[ly,2]
        ly.tot    <- nrow(h)
        
        # compact actual day 
        snowpack.tomorrow <- dry_compaction(h[,2], swe[,2], age[,2], ly.tot, ly, model_opts$k, model_opts$rho.max, ts, prec, g)
        
        # set values for next day
        rl  <- assignH(snowpack.tomorrow, h, swe, age, H, SWE, t, day.tot)
        h   <- rl$h
        swe <- rl$swe
        age <- rl$age
        H   <- rl$H
        SWE <- rl$SWE
        
        if (layers) {
             h_layers[[t]] <- h[, 2]  
             swe_layers[[t ]] <- swe[, 2]  
             age_layers[[t]] <- age[, 2]  
        }
        
        # non-first day of snow covered period
      } else if ( Hobs[t-1] > 0 ){
        
        deltaH <- Hobs[t] - H[t]
        
        if (is.na(deltaH)) {
          stop("Snow depth difference could not be calculated. Did you forget to convert hs into meters?")
        }
        
        if( deltaH > model_opts$tau ){
          if (verbose) msg(m,t,paste("create new layer",ly+1))
          sigma.null <- deltaH * model_opts$rho.null * g
          if (dyn_rho_max) {
            epsilon <- model_opts$c.ov * sigma.null * exp(-model_opts$k.ov * nixmass.e$snowpack.dd$rho/(rho_max_dyn(age[, 2]) - nixmass.e$snowpack.dd$rho))  
          } else {
            epsilon <- model_opts$c.ov * sigma.null * exp(-model_opts$k.ov * nixmass.e$snowpack.dd$rho/(model_opts$rho.max - nixmass.e$snowpack.dd$rho))  
          }
          
          h[,2]         <- (1 - epsilon) * h[,2]
          swe[,2]       <- swe[,1]
          age[(1:ly),2] <- age[(1:ly),1] + 1
          H[t]          <- sum(h[,2])
          SWE[t]        <- sum(swe[,2])
          
          # only for new layer add new layer to matrices
          h             <- rbind(h,rep(0,3))
          swe           <- rbind(swe,rep(0,3))
          age           <- rbind(age,rep(0,3))   
          ly            <- ly + 1
          h[ly,2]       <- Hobs[t] - H[t]
          swe[ly,2]     <- model_opts$rho.null * h[ly,2]
          age[ly,2]     <- 1
          
          # recompute
          H[t]          <- sum(h[,2])
          SWE[t]        <- sum(swe[,2])
          ly.tot        <- nrow(h)
          
          # compact actual day 
          snowpack.tomorrow <- dry_compaction(h[,2], swe[,2], age[,2], ly.tot, ly, model_opts$k, model_opts$rho.max, ts, prec, g)
          
          # set values for next day
          rl  <- assignH(snowpack.tomorrow, h, swe, age, H, SWE, t, day.tot)
          h   <- rl$h
          swe <- rl$swe
          age <- rl$age
          H   <- rl$H
          SWE <- rl$SWE
          
          if (layers) {
               h_layers[[t]] <- h[, 2]  
               swe_layers[[t ]] <- swe[, 2]  
               age_layers[[t]] <- age[, 2]  
          }
          
          # no mass gain or loss, but scaling
        } else if( deltaH >= -model_opts$tau & deltaH <= model_opts$tau ) {
          if (verbose) msg(m,t,paste("scaling: "))
          ly.tot <- nrow(h)
          rl  <- scaleH(t, ly, ly.tot, day.tot, deltaH, Hobs, h, swe, age, H, SWE, model_opts$rho.max, model_opts$k, ts, prec, m)
          h   <- rl$h
          swe <- rl$swe
          age <- rl$age
          H   <- rl$H
          SWE <- rl$SWE
          
          if (layers) {
               h_layers[[t]] <- h[, 2]  
               swe_layers[[t ]] <- swe[, 2]  
               age_layers[[t]] <- age[, 2]  
          }
          
        } else if ( deltaH < -model_opts$tau ){
          if (verbose) msg(m,t,paste("drenching: "))
          ly.tot <- nrow(h)
          rl  <- drenchH(t, ly, ly.tot, day.tot, Hobs, h, swe, age, H, SWE, model_opts$rho.max, model_opts$c.ov, model_opts$k.ov, model_opts$k, ts, prec, m)
          h   <- rl$h
          swe <- rl$swe
          age <- rl$age
          H   <- rl$H
          SWE <- rl$SWE
          
          if (layers) {
               h_layers[[t]] <- h[, 2]  
               swe_layers[[t ]] <- swe[, 2]  
               age_layers[[t]] <- age[, 2]  
          }
          
        } else {
          if (verbose) msg(m,t,"?")
        }
      }
    } 
    if (verbose) msg(m,t,"\n")
  }
  
  # compile layers
  if (layers) {
       h_layers <- lapply(h_layers, function(x) {
            length(x) <- ly
            x
       })
       h_layers <- do.call(cbind, h_layers)

       swe_layers <- lapply(swe_layers, function(x) {
            length(x) <- ly
            x
       })
       swe_layers <- do.call(cbind, swe_layers)

       age_layers <- lapply(age_layers, function(x) {
            length(x) <- ly
            x
       })
       age_layers <- do.call(cbind, age_layers)

       res <- list("SWE" = SWE, "h" = h_layers, "swe" = swe_layers, "age" = age_layers)
  } else {
       res <- SWE
  }
  
  return(res)
}

