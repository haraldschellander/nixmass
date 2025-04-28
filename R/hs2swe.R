# translation of https://github.com/jannefiluren/HS2SWE/blob/main/HS2SWE.m to R
#' A model which translates snow depth observations to snow water equivalents
#'
#' The HS2SWE model is based on similar principles as the \code{\ref{swe.delta.snow}} model.
#' It is described in the following publication:
#' Magnusson, J., B. Cluzet., L. Quéno, R. Mott, M. Oberrauch, G. Mazzotti, C. Marty, T. Jonas; 2025; Evaluating methods to estimate the water equivalent of new snow from daily snow depth recordings; Cold Regions Science and Technology, 233; 10.1016/j.coldregions.2025.104435.
#'
#' The model is also available as matlab function or python package from https://github.com/oshd-slf/HS2SWE
#'
#'
#' @param idata A numeric vector of snow depth observations in cm. NO negative values are allowed.
#' @param RhoNew The density of new snow in kg/m³. Default is 113.7 kg/m³.
#' @param RhoMax The maximum density of snow in kg/m³. Default is 571.6 kg/m³.
#' @param SnoTemp The temperature of the snow in °C. Default is -0.000 °C.
#' @param Visc The viscosity of the snow in kg/m²/s. Default is 6.051e7 kg/m²/s.
#' @param DQMultInc The increment of the densification rate. Default is 0.1.
#' @param DQMultMax The maximum densification rate. Default is 5.
#' @param HsAcc The threshold for new snow fall in cm. Default is 2 cm.
#' @param c1 The first coefficient for the densification rate. Default is 2.8e-6.
#' @param c2 The second coefficient for the densification rate. Default is 0.042.
#' @param c3 The third coefficient for the densification rate. Default is 0.046.
#' @param c4 The fourth coefficient for the densification rate. Default is 0.081.
#' @param c5 The fifth coefficient for the densification rate. Default is 0.018.
#' @param g The gravitational acceleration in m/s². Default is 9.81 m/s².
#' @param dt The time step in seconds. Default is 86400 seconds (1 day).
#'
#' @returns A numeric vector of simulated SWE in mm.
#' @export
#'
#' @examples
#' #' data(hsdata, package = "nixmass")
#'
#' swe_deltasnow <- swe.delta.snow(hsdata)
#' swe_hs2swe <- hs2swe(hsdata$hs * 100)
#' plot(seq_along(hsdata$date), swe_deltasnow, type = "l", ylab = "SWE (mm) / hs (cm)", xlab = "day")
#' lines(seq_along(hsdata$date), swe_hs2swe, type = "l", col = "red")
#' lines(seq_along(hsdata$date), hsdata$hs * 100, type = "l", lty = 2, col = "grey30")
#' legend("topleft", legend = c("deltaSNOW", "HS2SWE", "HS"),
#'  col = c("black", "red", "grey30"), lty = c(1, 1, 2))
hs2swe <- function(
  idata,
  RhoNew = 113.7,
  RhoMax = 571.6,
  SnoTemp = -0.000,
  Visc = 6.051e7,
  DQMultInc = 0.1,
  DQMultMax = 5,
  HsAcc = 2,
  c1 = 2.8e-6,
  c2 = 0.042,
  c3 = 0.046,
  c4 = 0.081,
  c5 = 0.018,
  g = 9.81,
  dt = 86400
) {
  # looping over continuous strings of non-NA data with HS > 0
  odata <- vector("numeric", length = length(idata))
  psdix <- which(!is.na(idata) & idata > 0)
  if (length(psdix) > 0) {
    sepix <- c(0, which(diff(psdix) > 1), length(psdix))

    for (tsrix in 1:(length(sepix) - 1)) {
      HS <- c(0, idata[psdix[(sepix[tsrix] + 1):sepix[tsrix + 1]]])

      MR <- list(
        HS = matrix(0, nrow = 1, ncol = length(HS)),
        RHO = matrix(RhoMax, nrow = 1, ncol = length(HS)),
        OVB = matrix(0, nrow = 1, ncol = length(HS)), # overburden
        AGE = matrix(0:(length(HS) - 1), nrow = 1, ncol = length(HS)),
        DIA = matrix(0, nrow = 5, ncol = length(HS))
      )

      for (tn in 2:length(HS)) {
        #---------------------------------------------------------------
        # step 1: densification of existing layer (limited by RhoMax);
        # ATTN: any changes of the below line need to be replicated in step 3.3
        rho_act <- MR$RHO[, tn - 1] +
          MR$RHO[, tn - 1] *
            dt *
            ((MR$OVB[, tn - 1] *
              g /
              (Visc * exp(c4 * SnoTemp + c5 * MR$RHO[, tn - 1]))) +
              c1 * exp(-c2 * SnoTemp - c3 * pmax(0, MR$RHO[, tn - 1] - RhoNew)))
        MR$RHO[, tn] <- pmin(rho_act, RhoMax)

        #---------------------------------------------------------------
        # step 2: settling of snow according to step 1 assuming constant SWE;
        # ATTN: any changes of the below line need to be replicated in step 3.3
        MR$HS[, tn] <- MR$HS[, tn - 1] / (MR$RHO[, tn] / MR$RHO[, tn - 1])

        #---------------------------------------------------------------
        #%step 3: assimilate measured HS (add new snow / melt snow)
        # step 3.0 if HSmeas > HSmod for first time step assume new snow fall and add layer
        if (HS[tn] > sum(MR$HS[, tn]) & tn == 2) {
          nlix <- nrow(MR$HS) + 1
          MR$HS <- rbind(MR$HS, matrix(0, nrow = 1, ncol = length(HS)))
          MR$HS[nlix, tn] <- HS[tn] - sum(MR$HS[, tn])
          MR$RHO <- rbind(MR$RHO, matrix(RhoNew, nrow = 1, ncol = length(HS)))
          MR$OVB <- rbind(MR$OVB, matrix(0, nrow = 1, ncol = length(HS)))
          MR$AGE <- rbind(MR$AGE, matrix(0, nrow = 1, ncol = length(HS)))
          MR$AGE[nlix, tn:length(HS)] <- 0:(length(HS) - tn)

          # if observed HS is decreasing while model adds new snow add a note in MR.DIA
          if (HS[tn] < HS[tn - 1]) {
            MR$DIA[1, tn] <- 1
          }

          #---------------------------------------------------------------
          # step 3.1 if HSmeas > HSmod + HSacc assume new snow fall and add layer
        } else if (HS[tn] > sum(MR$HS[, tn]) + HsAcc) {
          nlix <- nrow(MR$HS) + 1
          MR$HS <- rbind(MR$HS, matrix(0, nrow = 1, ncol = length(HS)))
          MR$HS[nlix, tn] <- HS[tn] - sum(MR$HS[, tn])
          MR$RHO <- rbind(MR$RHO, matrix(RhoNew, nrow = 1, ncol = length(HS)))
          MR$OVB <- rbind(MR$OVB, matrix(0, nrow = 1, ncol = length(HS)))
          MR$AGE <- rbind(MR$AGE, matrix(0, nrow = 1, ncol = length(HS)))
          MR$AGE[nlix, tn:length(HS)] <- 0:(length(HS) - tn)

          # if observed HS is decreasing while model adds new snow add a note in MR.DIA
          if (HS[tn] < HS[tn - 1]) {
            MR$DIA[1, tn] <- 1
          }

          #-------------------------------------------------------
          # step 3.2 if HSmeas == HSmod don't do anything
        } else if (HS[tn] == sum(MR$HS[, tn])) {
          # % note difference between HSmeas - HSmod if positive in MR.DIA
          MR$DIA[2, tn] <- 0

          #---------------------------------------------------------------
          # step 3.3 if HSmeas > HSmod reapply densification with gradually decreasing densification rate until HSmeas <= HSmod
        } else if (HS[tn] > sum(MR$HS[, tn])) {
          # note difference between HSmeas - HSmod before assimilation
          MR$DIA[2, tn] <- HS[tn] - sum(MR$HS[, tn])

          #step 3.3.1 decreasing densification rate
          DQMultCur <- 1
          while (
            mean(MR$RHO[, tn]) < RhoMax &
              HS[tn] > sum(MR$HS[, tn]) &
              DQMultCur < DQMultMax
          ) {
            DQMultCur <- DQMultCur + DQMultInc
            rho_act <- MR$RHO[, tn - 1] +
              MR$RHO[, tn - 1] *
                dt *
                ((MR$OVB[, tn - 1] *
                  g /
                  (Visc * exp(c4 * SnoTemp + c5 * MR$RHO[, tn - 1]))) +
                  c1 *
                    exp(
                      -c2 * SnoTemp - c3 * pmax(0, MR$RHO[, tn - 1] - RhoNew)
                    )) /
                DQMultCur
            MR$RHO[, tn] <- pmin(rho_act, RhoMax)
            MR$HS[, tn] <- MR$HS[, tn - 1] / (MR$RHO[, tn] / MR$RHO[, tn - 1])
          }
          # note difference between HSmeas - HSmod after assimilation
          MR$DIA[3, tn] <- HS[tn] - sum(MR$HS[, tn])

          # note assimilation steps required to match HSmeas (negative for decreasing densification)
          MR$DIA[4, tn] <- -DQMultCur

          #step 3.3.2 if still HSmeas > HSmod (because of RhoMax or because of MAXITER) don't do anything
          if (HS[tn] > sum(MR$HS[, tn])) {
            # don't do anything
            # later eventually add snow layer MR$DIA[5, tn]
            # note when densification was too high to meet HS
            MR$DIA[5, tn] <- -1
          }

          #---------------------------------------------------------------
          # step 3.4 if HSmeas < HSmod reapply densification with gradually increasing densification rate
          # until HSmeas >= HSmod or MR$RHO == RHOmax for all layers
        } else if (HS[tn] < sum(MR$HS[, tn])) {
          #note difference between HSmeas - HSmod before assimilation
          MR$DIA[2, tn] <- HS[tn] - sum(MR$HS[, tn])

          # step 3.4.1 increase densification rate
          DQMultCur <- 1
          while (
            mean(MR$RHO[, tn]) < RhoMax &
              HS[tn] < sum(MR$HS[, tn]) &
              DQMultCur < DQMultMax
          ) {
            DQMultCur <- DQMultCur + DQMultInc
            # MR$RHO[, tn] <- pmin(MR$RHO[, tn - 1] + MR$RHO[, tn - 1] * dt *
            #                              ((MR$OVB[, tn - 1] * g / (Visc * exp(c4 * SnoTemp + c5 * MR$RHO[, tn - 1]))) +
            #                                       c1 * exp(-c2 * SnoTemp - c3 * pmax(0, MR$RHO[, tn - 1] - RhoNew))) * DQMultCur, RhoMax)
            rho_act <- MR$RHO[, tn - 1] +
              MR$RHO[, tn - 1] *
                dt *
                ((MR$OVB[, tn - 1] *
                  g /
                  (Visc * exp(c4 * SnoTemp + c5 * MR$RHO[, tn - 1]))) +
                  c1 *
                    exp(
                      -c2 * SnoTemp - c3 * pmax(0, MR$RHO[, tn - 1] - RhoNew)
                    )) *
                DQMultCur
            MR$RHO[, tn] <- pmin(rho_act, RhoMax)
            MR$HS[, tn] <- MR$HS[, tn - 1] / (MR$RHO[, tn] / MR$RHO[, tn - 1])
          }
          #note difference between HSmeas - HSmod after assimilation
          MR$DIA[3, tn] <- HS[tn] - sum(MR$HS[, tn])

          # note assimilation steps required to match HSmeas (positive for increasing densification)
          MR$DIA[4, tn] <- DQMultCur

          # step 3.4.2 if still HSmeas < HSmod (because of RhoMax or MAXITER) start melting from above
          if (HS[tn] < sum(MR$HS[, tn])) {
            for (lix in seq(nrow(MR$HS), 1)) {
              MR$HS[lix, tn] <- HS[tn] - sum(MR$HS[1:(lix - 1), tn])
              if (MR$HS[lix, tn] >= 0) {
                break()
              } else {
                MR$HS[lix, tn] <- 0
              }
            }
            # note when melt conditions are met
            MR$DIA[5, tn] = 1
          }
          # this condition should not happen
        } else {
          stop()
        }

        # step 4 recalculate overburden
        nlix <- nrow(MR$HS)
        MR$OVB[nlix, tn] <- 0
        for (nlix in seq(nrow(MR$HS) - 1, 1)) {
          MR$OVB[nlix, tn] <- sum(
            MR$HS[(nlix + 1):nrow(MR$HS), tn] *
              MR$RHO[(nlix + 1):nrow(MR$HS), tn] /
              100
          ) # in mm = kg/m²
        }
      }
      SWE <- colSums(MR$HS * MR$RHO / 100)
      odata[psdix[(sepix[tsrix] + 1):sepix[tsrix + 1]]] = SWE[2:length(SWE)]
    }
  } else {
    stop("no data with snow depth > 0 found")
  }

  return(odata)
}
