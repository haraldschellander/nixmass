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
#' \eqn{g = 9.8ms^{-2}}, viscosity \eqn{\eta_0} [Pa] and 
#' factor \eqn{k2 [m^3kg^{-1}}], determining the importance 
#' of today's for tomorrow's density.
#'
#' @param data A data.frame with at least two columns named \code{date} and \code{hs}. 
#' They should contain date and corresponding daily observations of snow depth \eqn{hs \ge 0} 
#' measured at one site. The unit must be meters (m). No gaps or NA are allowed.
#' @param rho.max Maximum density of an individual snow layer produced by 
#' the deltasnow model [kg/m3], \eqn{rho.max > 0}
#' @param rho.null Fresh snow density for a newly created layer [kg/m3], \eqn{rho.null > 0}.
#' @param c.ov Overburden factor due to fresh snow [-], \eqn{c.ov > 0}
#' @param k.ov Defines the impact of the individual layer density on the 
#' compaction due to overburden [-], \eqn{k.ov \in [0,1]}.
#' @param k Exponent of the exponential-law compaction [m3/kg], \eqn{k > 0}.
#' @param tau Uncertainty bound [m], \eqn{tau > 0}.
#' @param eta.null Effective compactive viscosity of snow for "zero-density" [Pa s].
#' @param timestep Timestep between snow depth observations in hours. Default is 24 hours, i.e. daily snow depth observations.
#' @param verbose Should additional information be given during runtime? Can be \code{TRUE} or \code{FALSE}.
#'
#' @return A vector with daily SWE values in mm.
#' @export
#' 
#' @references Gruber, S. (2014) "Modelling snow water equivalent based on daily snow depths", Masterthesis, Institute for Atmospheric and Cryospheric Sciences, University of Innsbruck.
#' \cr\cr
#' Martinec, J., Rango, A. (1991) "Indirect evaluation of snow reserves in mountain basins". Snow, Hydrology and Forests in High Alpine Areas. pp. 111-120.
#' \cr\cr
#' Sturm, M., Holmgren, J. (1998) "Differences in compaction behavior of three climate classes of snow". Annals of Glaciology 26, 125-130.
#' \cr\cr
#' Winkler, M., Schellander, H., and Gruber, S.: Snow water equivalents exclusively from snow depths and their temporal changes: the delta.snow model, Hydrol. Earth Syst. Sci., 25, 1165-1187, doi: 10.5194/hess-25-1165-2021, 2021.
#'
#' 
#' @author Harald Schellander, Michael Winkler
swe.delta.snow <- function(data, rho.max=401.2588, rho.null=81.19417, c.ov=0.0005104722, k.ov=0.37856737, k=0.02993175, tau=0.02362476, eta.null=8523356, timestep=24, verbose=FALSE) {
     
     if(!inherits(data,"data.frame"))
          stop("swe.deltasnow: data must be of class data.frame")
     
     if(!any((is.element(colnames(data), c("hs","date")))))
          stop("swe.deltasnow: data must contain at least two columns named 'hs' and 'date'")
     
     Hobs <- data$hs
     if(any(is.na(Hobs)))
          stop("swe.deltasnow: snow depth data must not be NA")
     if(!all(Hobs >= 0))
          stop("swe.deltasnow: snow depth data must not be negative")
     if(!is.numeric(Hobs))
          stop("swe.deltasnow: snow depth data must be numeric")
     if(Hobs[1] != 0)
          stop("swe.deltasnow: snow depth observations must start with 0")
     
     if(!inherits(data$date,"character"))
          stop("swe.deltasnow: date column must be of class character")
     
     z <- zoo(Hobs,as.Date(data$date))
     if(!is.regular(z, strict = TRUE))
          stop("swe.deltasnow: date column must be strictly regular")
     
     nixmass.e <- new.env()
     nixmass.e$snowpack.dd <- NULL
     
     H        <- c()                              # modeled total depth of snow at any day [m]
     SWE      <- c()                              # modeled total SWE at any day [kg/m2]
     ly       <- 1                                # layer number [-]
     ts       <- timestep * 3600                  # timestep between observations [s]
     g        <- 9.81                             # gravity [ms-2]
     
     # preallocate some variables   
     # one row for first layer and 3 cols for timestep-1, timestep, timestep+1
     h        <- matrix(0,1,3)                     # modeled depth of snow in all layers [m]
     swe      <- matrix(0,1,3)                     # modeled swe in all layers [kg/m2]
     age      <- matrix(0,1,3)                     # age of modeled layers [days]
     day.tot  <- length(Hobs)                      # total days from first to last snowfall [-]
     #ly.tot   <- length(which(Hobs>0))            # maximum number of layers [-]
     
     
     # helper
     m        <- rep("",day.tot)                  # vector of (verbose) messages
     prec     <- 10^-10                           # precision for arithmetic comparisons [-]
     
     
     
     #-------------------------------------------------------------------------
     # compaction of snowpack
     #-------------------------------------------------------------------------
     dry_compaction <- function(h.d, swe.d, age.d, ly.tot, ly, k, rho.max, ts, prec, g){
          # .d  -> today
          # .dd -> tomorrow
          # compute overburden for each layer
          # the overburden for the first layer is the layer itself
          swe.hat.d <- c()
          for(i in 1:ly.tot){
               swe.hat.d[i] <- sum(swe.d[i:ly.tot])
          }
          
          snowpack.d   <- data.frame(h  = h.d, swe = swe.d, swe.hat = swe.hat.d, age = age.d)
          H.d          <- sum(snowpack.d$h)
          
          a <- data.frame(t(apply(snowpack.d[(1:ly),], 1, compactH, H.d, k, rho.max, ts, prec, g, eta.null)))
          b <- data.frame(rep(0,ly.tot-ly),rep(0,ly.tot-ly),rep(0,ly.tot-ly),rep(0,ly.tot-ly))
          colnames(a) <- colnames(b) <- c("h","swe","age","rho")
          nixmass.e$snowpack.dd <<- rbind(a,b)
          rownames(nixmass.e$snowpack.dd) <<- paste0("dd.layer",1:nrow(nixmass.e$snowpack.dd))
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
          h.dd <- x[1]/(1 + (x[3] * g * ts)/eta.null * exp(-k * x[2]/x[1]))
          h.dd <- ifelse(x[2]/h.dd > rho.max, x[2]/rho.max, h.dd)
          h.dd <- ifelse(x[1] == 0, 0, h.dd)
          swe.dd  <- x[2]
          age.dd  <- ifelse(x[1] == 0, 0, age.d + 1)
          rho.dd  <- ifelse(x[1] == 0, 0, swe.dd/h.dd)
          rho.dd  <- ifelse(rho.max - rho.dd < prec, rho.max, rho.dd)
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
             msg(m,t,paste("melt "))
             
             Hobs.d=Hobs[t]; h.d=h[,2]; swe.d=swe[,2]; age.d=age[,2]
             
             runoff <- 0
             
             # distribute mass top-down
             for(i in ly:1){
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
             if( all(rho.max - swe.d[1:ly]/h.d[1:ly] <= prec) ){
                     msg(m,t,paste("no further compaction "))
                     
                     # produce runoff if sum(h.d) - Hobs.d is still > 0
                     #if ( sum(h.d) - Hobs.d > prec ){
                     msg(m,t,paste("runoff "))
                     # decrease swe from all layers?
                     # or beginning with lowest?
                     #swe.d[1:ly] <- swe.d[1:ly] - (sum(h.d) - Hobs.d) * rho.max
                     scale <- Hobs.d/sum(h.d)
                     runoff <- (sum(h.d) - Hobs.d) * rho.max  # excess is converted to runoff [kg/m2]
                     h.d <- h.d * scale                       # all layers are compressed (and have rho.max) [m]
                     swe.d <- swe.d * scale
                     
                     # }
                     
             } else {
                     msg(m,t,paste("compaction "))
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
             
             # todays overburden   
             swe.hat.d <- c()
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
                             swe.excess <- swe.d[idx.max]-h.dd.cor[idx.max]*rho.max
                             
                             # set affected layer(s) to rho.max
                             swe.d[idx.max] <- swe.d[idx.max] - swe.excess             
                             
                             # distribute excess swe to other layers top-down
                             lys <- 1:ly
                             lys <- lys[-idx.max]
                             i <- lys[length(lys)]
                             swe.excess.all <- sum(swe.excess)
                             while(swe.excess.all > 0){
                                     swe.res <- h.dd.cor[i] * rho.max - swe.d[i] # layer tolerates this swe amount to reach rho.max
                                     if(swe.res > swe.excess.all){
                                             swe.res <- swe.excess.all
                                     }
                                     swe.d[i]  <- swe.d[i] + swe.res
                                     swe.excess.all  <- swe.excess.all - swe.res
                                     i <- i - 1
                                     if(i<=0 & swe.excess.all > 0){
                                             msg(m,t,paste(" runoff"))
                                             break
                                     }
                             }
                     } else {
                             # if all layers have density > rho.max
                             # remove swe.excess from all layers (-> runoff)
                             # (this sets density to rho.max)
                             swe.excess <- swe.d[idx.max]-h.dd.cor[idx.max]*rho.max
                             swe.d[idx.max] <- swe.d[idx.max] - swe.excess
                             msg(m,t,paste(" runoff"))
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
             if(verbose){
                     cat(paste(strg))
                     if(is.null(m[t])){
                             m[t] <- strg  
                     } else {
                             m[t] <- paste(m[t],strg)
                     }
             }  
     }
     
     
     
     
     if(verbose){
             cat("Using parameters:\n",
                 "rho.max  =",rho.max,"\n",
                 "rho.null =",rho.null,"\n",
                 "c.ov     =",c.ov,"\n",
                 "k.ov     =",k.ov,"\n",
                 "k        =",k,"\n",
                 "tau      =",tau,"\n",
                 "eta.null =",eta.null,"\n"
             )
     }
     
     
     
     for (t in 1:day.tot) {
             
             msg(m,t,paste("day",t,": "))
             
             # shift temporary matrices one step back in time
             h[,1] <- h[,2]; h[,2] <- h[,3]; h[,3] <- 0
             swe[,1] <- swe[,2]; swe[,2] <- swe[,3]; swe[,3] <- 0
             age[,1] <- age[,2]; age[,2] <- age[,3]; age[,3] <- 0
            
             # snowdepth = 0, no snow cover
             if( Hobs[t] == 0 ){      
                     if(t > 1){
                             if(Hobs[t-1] == 0){
                                     msg(m, t, paste0(""))        
                             } else {
                                     msg(m, t, paste0("runoff"))
                             }        
                     }else {
                             msg(m, t, paste0(""))         
                     }
                     H[t]    <- 0
                     SWE[t]  <- 0
                     h[,2]   <- 0
                     swe[,2] <- 0
                     
                     # there is snow
             } else if( Hobs[t] > 0 ){
                     
                     # first snow in/during season
                     if( Hobs[t-1] == 0 ){
                             ly <- 1
                             msg(m,t,paste("produce layer",ly))
                             age[ly,2] <- 1
                             h[ly,2]   <- Hobs[t]
                             H[t]      <- Hobs[t]
                             swe[ly,2] <- rho.null * Hobs[t]
                             SWE[t]    <- swe[ly,2]
                             ly.tot    <- nrow(h)
                             
                             # compact actual day 
                             snowpack.tomorrow <- dry_compaction(h[,2], swe[,2], age[,2], ly.tot, ly, k, rho.max, ts, prec, g)
                             
                             # set values for next day
                             rl  <- assignH(snowpack.tomorrow, h, swe, age, H, SWE, t, day.tot)
                             h   <- rl$h
                             swe <- rl$swe
                             age <- rl$age
                             H   <- rl$H
                             SWE <- rl$SWE
                             
                             
                             # non-first day of snow covered period
                     } else if ( Hobs[t-1] > 0 ){
                             
                             deltaH <- Hobs[t] - H[t]
                             
                             if( deltaH > tau ){
                                     msg(m,t,paste("create new layer",ly+1))
                                     
                                     sigma.null <- deltaH * rho.null * g
                                     epsilon <- c.ov * sigma.null * exp(-k.ov * nixmass.e$snowpack.dd$rho/(rho.max - nixmass.e$snowpack.dd$rho))
                                     
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
                                     swe[ly,2]     <- rho.null * h[ly,2]
                                     age[ly,2]     <- 1
                                     
                                     # recompute
                                     H[t]          <- sum(h[,2])
                                     SWE[t]        <- sum(swe[,2])
                                     ly.tot        <- nrow(h)
                                     
                                     # compact actual day 
                                     snowpack.tomorrow <- dry_compaction(h[,2], swe[,2], age[,2], ly.tot, ly, k, rho.max, ts, prec, g)
                                     
                                     # set values for next day
                                     rl  <- assignH(snowpack.tomorrow, h, swe, age, H, SWE, t, day.tot)
                                     h   <- rl$h
                                     swe <- rl$swe
                                     age <- rl$age
                                     H   <- rl$H
                                     SWE <- rl$SWE
                                     
                                     # no mass gain or loss, but scaling
                             } else if( deltaH >= -tau & deltaH <= tau ) {
                                     msg(m,t,paste("scaling: "))
                                     ly.tot <- nrow(h)
                                     rl  <- scaleH(t, ly, ly.tot, day.tot, deltaH, Hobs, h, swe, age, H, SWE, rho.max, k, ts, prec, m)
                                     h   <- rl$h
                                     swe <- rl$swe
                                     age <- rl$age
                                     H   <- rl$H
                                     SWE <- rl$SWE
                                     
                             } else if ( deltaH < -tau ){
                                     msg(m,t,paste("drenching: "))
                                     ly.tot <- nrow(h)
                                     rl  <- drenchH(t, ly, ly.tot, day.tot, Hobs, h, swe, age, H, SWE, rho.max, c.ov, k.ov, k, ts, prec, m)
                                     h   <- rl$h
                                     swe <- rl$swe
                                     age <- rl$age
                                     H   <- rl$H
                                     SWE <- rl$SWE
                                     
                                     
                             } else {
                                     msg(m,t,"?")
                             }
                     }
             } 
             msg(m,t,"\n")
     }
     return(SWE)
}
