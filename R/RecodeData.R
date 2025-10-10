
#' RecodeData
#'
#' Uses a dictionary in the form of a named vector to perform recoding on a target vector.
#'
#' @param TargetVector \code{vector} that recoding is performed on
#' @param Dictionary A named \code{character} vector. Vector names are look-up values, vector values are substitute values.
#'
#' @return A \code{vector}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RecodeData <- function(TargetVector,
                       Dictionary)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.vector(TargetVector),
              is.character(Dictionary))

#-------------------------------------------------------------------------------

  # Look-up Values come from names(Dictionary)
  LookupValues <- paste0("^",      # Embrace Lookup Values with "^" and "$" to isolate them, so that for example "R" is not recognized (and replaced) in "CR", but only when it stands isolated
                         str_escape(names(Dictionary)),      # Introduce escaping of special characters, for example "(" and "/"
                         "$")

  # Add "Protection Prefix" to mark freshly changed Values, so that they won't be looked up themselves
  NewValues <- paste0("NEW_", Dictionary)
  names(NewValues) <- LookupValues

  Output <- str_replace_all(TargetVector, NewValues)
  Output <- str_remove_all(Output, "NEW_")      # Remove the introduced "Protection Prefix"

#-------------------------------------------------------------------------------
  return(Output)
}
