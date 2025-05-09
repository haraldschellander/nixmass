# package nixmass

#' @importFrom zoo zoo is.regular
#' @importFrom lubridate month yday year
#' @importFrom dplyr arrange pull right_join mutate as_tibble tibble bind_rows across everything
#' @importFrom tidyr pivot_longer
#' @importFrom colorspace scale_fill_continuous_sequential
NULL

#' @import graphics
#' @import ggplot2
#' @import utils
#' @import stats
#' @import grDevices
NULL

#' Daily snow depth data for a northern alpine station
#'
#' Gapless daily snow depth observations for a winter season from 1.8. - 31.7. from a station situated in the northern earstern alps at an altitude of 600 m. For anonymization the years are intentionally set to 1900 - 1901.

#' This data series is free of gaps with a minimum of 0 and a maximum of 1.3 meters.
#' It is intended to be used as is as input data for the package \code{\link{nixmass}}
#' to calculate snow water equivalent and bulk snow density with the delta.snow method
#' and several empirical regression models from the literature.
#'
#' @docType data
#' @keywords datasets
#' @name hsdata
#' @usage data(hsdata)
#'
#' @format A `data.frame` named \code{data} with columns \code{date} and \code{hs}.
#' \describe{
#' \item{date}{The date column contains character strings of the format "YYYY-MM-DD"
#' and is of class \code{character}}.
#' \item{hs}{The hs column holds daily observed snow depths
#' in meters and is of class \code{numeric}.}
#' }
#'
#' @examples
#' ## Load example data
#' data("hsdata")
#'
#' ## explore dataset
#' head(hsdata)
#' plot(hsdata$hs, type="o")
#'
#' ## compute snow water equivalents
#' o <- nixmass(hsdata, model="delta.snow")
#' plot(o)
#'
#' o1 <- nixmass(hsdata, alt=600, region.jo09 = 6, region.gu19 = "central",
#' snowclass.st10 = "alpine", verbose = FALSE)
#' plot(o1)
#' summary(o1)
NULL


#' SWE modeling with the delta.snow process based model and several empirical regression models.
#'
#' Snow Water Equivalent (SWE) is modeled either exclusively from
#' daily snow depth changes or statistically, depending on
#' snow depth, elevation, date and climate class.
#'
#' @details This function is a wrapper for the simulation of SWE with different models.
#' The process based model \code{\link[=swe.delta.snow]{delta.snow}}
#' can be chosen in its original formulation (Winkler et al. 20219) and with a dynamically
#' increasing maximum bulk snow density (Schroeder et al., 2024). The \code{\link[=hs2swe]{hs2swe}}
#' model is an alternative formulation of the same physical concept used in delta.snow (Magnusson, et al., 2025).
#' Some empirical regression models can also be chosen:
#' \code{\link[=swe.jo09]{Jonas},\link[=swe.pi16]{Pistocchi}, \link[=swe.st10]{Sturm}} and \link[=swe.gu19]{Guyennon}.
#' For the `delta.snow` and `hs2swe` models and the ones of `Pistocchi` and `Guyennon`,
#' the needed parameters and coefficients from the original references are set as default.
#' They can however be changed according to results from other datasets.
#' For the other models of `Jonas` and `Sturm` regression coefficients are fixed.
#'
#' Computation is quite fast and there does not exist any restriction
#' regarding the length of the data. However, if many years have to be modeled at once,
#' it is recommended to split the computation into single years.
#'
#' @param data A data.frame with at least two columns named \code{date} and \code{hs}.
#' They should contain date and corresponding daily observations of snow depth \eqn{hs \ge 0}
#' measured at one site. The unit must be meters (m). No gaps or NA are allowed.
#' Dates must be either of class `character`, `Date` or `POSIXct` and given in the format
#' \code{YYYY-MM-DD}. No sub-daily resolution is allowed at the moment (see details).
#' @param model Defines model for SWE computation. Can be one, several or all of `delta.snow`,
#' `delta.snow.dyn_rho_max`, `hs2swe`, `jo09`, `pi16`, `st10`, `gu19`. If no model is given,
#' `delta.snow` will be taken.
#' @param alt Must be given in meter if one of model is `jo09`. Ignored otherwise.
#' @param region.jo09 Must be given if one of model is `jo09`, ignored otherwise. This must be an integer number
#' between 1 and 7 of the Swiss region where the station belongs to, according to Fig. 1 in the original reference.
#' @param region.gu19 If model contains `gu19` this must be one of `italy`, `southwest`, `central` or `southeast` as described in the original reference.
#' #' Ignored if model is not `gu19`.
#' @param snowclass.st10 Must be given if one of model is `st10`. Must be one of the following character strings:
#' `alpine`, `maritime`, `prairie`, `tundra`, `taiga` as outlined in the original reference.
#' Ignored if model is not `st10`.
#' @param layers Logical. Should parameters snow depth, swe and age be returned layerwise?.
#' @param strict_mode Logical. If `TRUE`, the function checks if the data is strictly regular and
#' if the snow depth series starts with zero.
#' @param verbose Logical. Should additional information be given during runtime?
#'
#' @return  A list of class \code{nixmass} with components:
#' \item{swe}{ Contains a list of numerical vectors. Each entry refers to SWE values computed with the selected model(s). }
#' \item{date}{Vector of date strings in the input class of format \code{YYYY-MM-DD}.}
#' \item{hs}{Vector of given snow depth values used to compute SWE. }
#'
#' @author Harald Schellander, Michael Winkler
#' @references
#'     Guyennon, N., Valt, M., Salerno, F., Petrangeli, A., Romano, E. (2019) 'Estimating the snow water equivalent from snow depth measurements in the Italian Alps', Cold Regions Science and Technology. Elsevier, 167 (August), p. 102859. doi: 10.1016/j.coldregions.2019.102859.
#' \cr\cr
#' Jonas, T., Marty, C. and Magnusson, J. (2009) "Estimating the snow water equivalent from snow depth measurements in the Swiss Alps"", Journal of Hydrology, 378(1 - 2), pp. 161 - 167. doi: 10.1016/j.jhydrol.2009.09.021.
#' \cr\cr
#' Pistocchi, A. (2016) "Simple estimation of snow density in an Alpine region", Journal of Hydrology: Regional Studies. Elsevier B.V., 6(Supplement C), pp. 82 - 89. doi: 10.1016/j.ejrh.2016.03.004.
#' \cr\cr
#' Sturm, M. et al. (2010) "Estimating Snow Water Equivalent Using Snow Depth Data and Climate Classes", Journal of Hydrometeorology, 11(6), pp. 1380 - 1394. doi: 10.1175/2010JHM1202.1.
#' \cr\cr
#' Winkler, M., Schellander, H., and Gruber, S.: Snow water equivalents exclusively from snow depths and their temporal changes: the delta.snow model, Hydrol. Earth Syst. Sci., 25, 1165-1187, doi: 10.5194/hess-25-1165-2021, 2021.
#' \cr\cr
#' Schroeder, M.et al. (2024) "CONTINUOUS SNOW WATER EQUIVALENT MONITORING ON GLACIERS USING COSMIC RAY NEUTRON SENSOR TECHNOLOGY A CASE STUDY ON HINTEREISFERNER, AUSTRIA", Proceedings: International Snow Science Workshop 2024, Tromsø, Norway, 1107 - 1114
#
#' @export
#'
#' @examples
#' # Load example data with realistic snow depth values
#' # from a station at 600 meters in the northern Alps
#' # Note that the winter season is set to an arbitrary date
#' # to mask its origin
#' data("hsdata")
#' o <- nixmass(hsdata, model="delta.snow",verbose=TRUE)
#' plot(o)
#'
#' o1 <- nixmass(hsdata, alt=600, region.jo09=6, region.gu19 = "central",
#'               snowclass.st10 = "alpine", verbose = FALSE)
#' plot(o1)
#' summary(o1)
#'
#' swe <- nixmass(hsdata, alt = 1000, region.jo09=1, snowclass.st10 = "tundra", region.gu19 = "italy")
#' summary(swe)
#'
nixmass <- function(
  data,
  model = c(
    "delta.snow",
    "delta.snow.dyn_rho_max",
    "hs2swe",
    "jo09",
    "pi16",
    "st10",
    "gu19"
  ),
  alt,
  region.jo09,
  region.gu19,
  snowclass.st10,
  layers = FALSE,
  strict_mode = TRUE,
  verbose = FALSE
) {
  if (missing(model)) {
    model <- "delta.snow"
  } else {
    model <- match.arg(model, several.ok = TRUE)
  }

  # if (any(grepl("HS2SWE", model))) {
  #   layers <- FALSE
  # }

  if (any(grepl("jo09", model))) {
    if (missing(alt)) stop("Argument 'alt' is missing")
    if (missing(region.jo09)) stop("Argument 'region.jo09' is missing")
  }

  # if (any(grepl("pi16", model))) {
  #   if (missing(snowclass.st10))
  #     stop("Argument 'snowclass.st10' is missing")
  # }

  if (any(grepl("st10", model))) {
    if (missing(snowclass.st10)) stop("Argument 'snowclass.st10' is missing")
  }

  if (any(grepl("gu19", model))) {
    if (missing(region.gu19)) stop("Argument 'region.gu19' is missing")
  }

  #-----------------------------------------------------------------------
  # split into different models
  swe <- list()
  for (m in model) {
    if (m == "delta.snow") {
      swe[["swe"]][[m]] <- swe.delta.snow(
        data,
        model_opts = list(),
        dyn_rho_max = FALSE,
        layers,
        strict_mode,
        verbose
      )
    } else if (m == "delta.snow.dyn_rho_max") {
      swe[["swe"]][[m]] <- swe.delta.snow(
        data,
        model_opts = list(),
        dyn_rho_max = TRUE,
        strict_mode,
        layers,
        verbose
      )
    } else if (m == "hs2swe") {
      swe[["swe"]][[m]] <- hs2swe(data)
    } else if (m == "jo09") {
      swe[["swe"]][[m]] <- swe.jo09(data, alt, region.jo09)
    } else if (m == "pi16") {
      swe[["swe"]][[m]] <- swe.pi16(data, rho_0 = 200, K = 1)
    } else if (m == "st10") {
      swe[["swe"]][[m]] <- swe.st10(data, snowclass.st10)
    } else if (m == "gu19") {
      swe[["swe"]][[m]] <- swe.gu19(
        data,
        region.gu19,
        n0 = NA,
        n1 = NA,
        n2 = NA
      )
    }
  }

  swe[["date"]] <- data$date
  swe[["hs"]] <- data$hs

  class(swe) <- "nixmass"
  return(swe)
}


#' Print summary of a nixmass object.
#'
#' @param object A nixmass object.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return Summary information of SWE values calculated with selected models is printed to the screen.
#' @export
#'
#' @examples
#' data("hsdata")
#' n <- nixmass(hsdata, model = c("delta.snow", "pi16"))
#' summary(n)
#'
summary.nixmass <- function(object, ...) {
  if (!inherits(object, "nixmass"))
    stop("nixmass: Object must be of class 'nixmass'.")

  models <- names(object$swe)
  if (length(models) == 0)
    stop("nixmass: Cannot print anything. No model was computed.")

  l <- lapply(object$swe, quantile, na.rm = T)
  lm <- lapply(object$swe, mean, na.rm = T)
  m <- do.call(rbind, l)
  mm <- sapply(lm, c)
  mmm <- cbind(m, mm)
  mmm <- mmm[, c(1:3, 6, 4, 5)]
  if (length(models) > 1) {
    mmm <- data.frame(mmm)
    colnames(mmm) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
  } else {
    names(mmm) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
  }
  print(mmm)
}


#' Plot modeled SWE values of a nixmass object.
#'
#' @param x nixmass object.
#' @param title Main plot title.
#' @param density Shall a density plot be created?
#' @param ... Further graphical parameters may also be supplied as arguments. See \code{\link[graphics]{plot}}.
#'
#' @return Does not return anything. A plot is produced.
#' @export
#'
#' @examples
#' data("hsdata")
#' plot(nixmass(hsdata, model = "delta.snow"))
#' plot(nixmass(hsdata, model = "delta.snow.dyn_rho_max", layers = TRUE), density = TRUE)
#'
plot.nixmass <- function(x, title = NULL, density = FALSE, ...) {
  if (!inherits(x, "nixmass"))
    stop("nixmass: Object must be of class 'nixmass'.")

  models <- names(x$swe)
  if (length(models) == 0) stop("nixmass: Cannot plot. No model was computed.")

  if (density) {
    if (length(models) > 1) {
      stop("Density plot can only be created for one model.")
    }

    if (!models %in% c("delta.snow", "delta.snow.dyn_rho_max")) {
      stop(
        "Density plot can only be created for the delta.snow or delta.snow.dyn_rho_max models."
      )
    }

    if (!all(c("h", "swe") %in% names(x$swe[[1]]))) {
      stop("The layer matrices are missing in the modeled object.")
    }
  }

  colors <- c(
    "#E16A86",
    "#C18500",
    "#799D00",
    "#00AB6E",
    "#00A9BE",
    "#6C8EE6",
    "#D169D0",
    "#a10999"
  )

  #Sys.setlocale("LC_TIME", locale = "English")

  if (!density) {
    # define maximum swe for plot outline
    if (is.null(names(x$swe[[1]]))) {
      ymax <- list()
      for (m in models) {
        ymax <- max(na.omit(c(x$swe[[m]])))
      }
      datevec <- as.Date(x$date)

      ymax <- max(ymax)
      plot(
        datevec,
        xaxt = "n",
        x$swe[[1]],
        type = "n",
        xlab = "",
        ylab = "HS (cm) / SWE (kg/m2)",
        ylim = c(0, ymax * 1.2)
      )
      n <- 1
      for (m in models) {
        lines(as.Date(x$date), x$swe[[m]], type = "l", col = colors[n])
        n <- n + 1
      }
    } else {
      ymax <- list()
      for (m in models) {
        ymax <- max(na.omit(c(x$swe[[m]]$SWE)))
      }
      ymax <- max(ymax)
      plot(
        as.Date(x$date),
        xaxt = "n",
        x$swe[[1]]$SWE,
        type = "n",
        xlab = "",
        ylab = "HS (cm) / SWE (kg/m2)",
        ylim = c(0, ymax * 1.2)
      )
      n <- 1
      for (m in models) {
        lines(as.Date(x$date), x$swe[[m]]$SWE, type = "l", col = colors[n])
        n <- n + 1
      }
    }
    axis.Date(
      1,
      at = seq(
        as.Date(x$date[1]),
        as.Date(x$date[length(x$date)]),
        by = "2 month"
      ),
      format = "%b"
    )
    lines(as.Date(x$date), x$hs * 100, type = "l", lty = 2, col = "black")

    # } else {
    #   lines(
    #     1:length(x$swe$HS2SWE),
    #     x$hs,
    #     type = "l",
    #     lty = 2,
    #     col = "black"
    #   )
    # }
    t <- ifelse(is.null(title), "SWE", title) #paste0("Chartreuse (",alts[s],"m)")
    legend(
      title = t,
      "topleft",
      legend = c(models, "HS"),
      col = c(colors[1:length(models)], "black"),
      lty = c(rep(1, length(models)), 2),
      cex = 0.8,
      bg = "transparent",
      bty = "n"
    )
  } else {
    # density plot
    m <- models[1]
    h <- data.frame(x$swe[[m]][["h"]])
    swe <- data.frame(x$swe[[m]][["swe"]])
    dens <- swe / h
    colnames(h) <- colnames(dens) <- x$date
    dens.m <- pivot_longer(
      dens,
      cols = everything(),
      names_to = "date",
      values_to = "dens"
    )
    snowpack <- pivot_longer(
      h,
      cols = everything(),
      names_to = "date",
      values_to = "h"
    )
    snowpack$dens <- dens.m$dens
    snowpack$date <- as.Date(snowpack$date)
    first.day <- sort(snowpack$date)[1]
    l <- nrow(subset(snowpack, date == first.day))
    snowpack$ly <- rep(1:l, length(unique(snowpack$date)))

    # height of sum of layers
    snowpack$h.sum <- NA
    for (day in unique(snowpack$date)) {
      s <- subset(snowpack, date == day)
      h.sum <- list()
      for (i in 1:l) {
        h.sum[[i]] <- sum(s$h[1:i])
      }
      h.sum <- do.call("c", h.sum)
      idx <- as.numeric(rownames(s))
      snowpack$h.sum[idx] <- h.sum
    }
    years <- unique(year(snowpack$date))

    hdata <- data.frame(date = as.Date(x$date), hs = x$hs)
    dens_br <- c(
      seq(0, max(snowpack$dens, na.rm = TRUE), 100),
      round(max(snowpack$dens, na.rm = TRUE) + 100, -1)
    )
    p <- ggplot(snowpack, aes(date, h * 100)) +
      geom_col(aes(fill = dens), width = 1) +
      scale_x_date(date_labels = "%b", date_breaks = "1 months") +
      scale_fill_continuous_sequential(
        palette = "Blues3",
        breaks = dens_br,
        labels = dens_br,
        limits = c(0, max(dens_br))
      ) +
      geom_line(data = hdata, aes(date, hs * 100), color = "grey50") +
      scale_y_continuous(
        breaks = seq(0, max(hdata$hs) * 110, 20),
        labels = seq(0, max(hdata$hs) * 110, 20)
      ) +
      guides(fill = guide_legend(position = "inside")) +
      ggtitle(title) +
      labs(
        x = "",
        y = "Snowdepth (cm)",
        fill = paste0("Density (kgm-3)\n", m)
      ) +
      theme_bw() %+%
        theme(legend.position.inside = c(0.2, 0.65))
    return(p)
  }

  invisible(x)
}
