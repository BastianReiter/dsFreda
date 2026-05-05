
#' FormatData
#'
#' Format data based on feature type specifications defined in meta data
#'
#' @param TargetVector \code{vector} - Vector that formatting is performed on
#' @param Type \code{string} - Defining data type
#'
#' @return A \code{vector}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FormatData <- function(TargetVector,
                       Type)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.vector(TargetVector) | is.date(TargetVector),
              is.string(Type))

#-------------------------------------------------------------------------------

  suppressWarnings({
      if (Type == "character") { TargetVector <- as.character(TargetVector) }
      if (Type == "date") { TargetVector <- lubridate::as_date(TargetVector) }
      if (Type == "double") { TargetVector <- as.double(TargetVector) }
      if (Type == "integer") { TargetVector <- as.integer(TargetVector) }
      if (Type == "logical") { TargetVector <- as.logical(TargetVector) }
      if (Type == "numeric") { TargetVector <- as.numeric(TargetVector) } })

#-------------------------------------------------------------------------------
  return(TargetVector)
}
