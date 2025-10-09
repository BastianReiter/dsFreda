
#' GetObjectStatusDS
#'
#' Similar to \code{dsBase::testObjExistsDS()}. Tests whether a certain object exists physically on server and provides meta info about it.
#'
#' Server-side AGGREGATE method
#'
#' @param ObjectName.S \code{string} - Name of object on server
#'
#' @return A \code{list} containing information about existence and class of object
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetObjectStatusDS <- function(ObjectName.S)
{
  # --- Argument Validation ---
  assert_that(is.string(ObjectName.S))

#-------------------------------------------------------------------------------

  ObjectExists <- FALSE
  ObjectClass <- NULL

  if (exists(ObjectName.S, envir = parent.frame()))
  {
      ObjectExists <- TRUE
      ObjectClass <- class(get(ObjectName.S, envir = parent.frame()))
  }

#-------------------------------------------------------------------------------
  return(list(ObjectExists = ObjectExists,
              ObjectClass = ObjectClass))
}
