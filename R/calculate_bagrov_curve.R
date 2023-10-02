# calculate_bagrov_curve -------------------------------------------------------

#' Calculate the curve(s) of the Bagrov relation
#'
#' @param effectivity Bagrov effectivity value (= n-value)
#' @param P_over_Ep_max maximum value of the ratio P/Ep with precipitation P and
#'   potential evaporation Ep.
#' @param Ep potential evaporation in mm/a (default: 650)
#' @param delta_Ea step of actual evaporation (default: 0.1)
#' @param dbg logical indicating whether or not to show debug messages
#' @return data frame with columns \code{P_over_Ep} (P/Ep), \code{Ea_over_Ep}
#'   (Ea/Ep), \code{effectivity} (n-value)
#' @export
#' @examples
#' \dontrun{
#' bagrov_data <- calculate_bagrov_curve(1:2)
#' plot(bagrov_data$P_over_Ep, bagrov_data$Ea_over_Ep)
#' }
calculate_bagrov_curve <- function(
    effectivity,
    P_over_Ep_max = 4,
    Ep = 650, # mm/a
    delta_Ea = 0.1,
    dbg = FALSE
)
{
  stopifnot(is.numeric(effectivity))

  # If more than one effectivity value is given, call this function for each
  # effectivity value, row-bind the data frames and return the resulting data
  # frame
  if (length(effectivity) > 1L) {

    return(do.call(rbind, lapply(
      X = effectivity,
      FUN = calculate_bagrov_curve,
      P_over_Ep_max = P_over_Ep_max,
      Ep = Ep,
      delta_Ea = delta_Ea,
      dbg = dbg
    )))
  }

  stopifnot(length(effectivity) == 1L)

  kwb.utils::catAndRun(
    paste("Calculating Bagrov curve for effectivity =", effectivity),
    dbg = dbg,
    expr = {

      Ea <- 0
      P <- 0
      Ea_over_Ep <- 0

      # Initialise result list
      results <- list()

      while(P <= P_over_Ep_max * Ep && Ea_over_Ep <= 1) {

        # Update delta_P
        delta_P <- delta_Ea / (1 - Ea_over_Ep^effectivity)

        # Increment P and Ea with their corresponding deltas
        P <- P + delta_P
        Ea <- Ea + delta_Ea

        # Calculate quotients Ea/Ep and P/Ep
        P_over_Ep <- P / Ep
        Ea_over_Ep <- Ea / Ep

        # Put the quotients and the effectivity-value into a named vector and add the
        # vector to the end of the result list
        results[[length(results) + 1L]] <- data.frame(
          P_over_Ep = P_over_Ep,
          Ea_over_Ep = Ea_over_Ep,
          effectivity = effectivity
        )
      }

      # Row-bind all data frames
      do.call(rbind, results)
    }
  )
}
