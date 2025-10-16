
#' GetReportingObjectDS
#'
#' Transports a reporting object from server to client. Its name must be on a defined list of permitted object names to ensure data privacy.
#'
#' Server-side AGGREGATE method
#'
#' @param ObjectName.S \code{string} - Name of reporting object on server
#'
#' @return The reporting object
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetReportingObjectDS <- function(ObjectName.S)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(ObjectName.S))

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Object <- eval(parse(text = ObjectName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  # To prevent disclosure, only the following objects are allowed to be handed to client
  PermittedObjectNames <- c("AugmentationReport",
                            "CurationReport",
                            "Messages",
                            "P21.AugmentationReport",
                            "P21.CurationReport")

  if (ObjectName.S %in% PermittedObjectNames)
  {
      return(Object)

  } else {

      ClientMessage <- "NOT PERMITTED due to data privacy concerns."
      stop(ClientMessage, call. = FALSE)
  }
}
