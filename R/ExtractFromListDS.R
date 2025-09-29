
#' ExtractFromListDS
#'
#' Auxiliary function to extract objects from list on server
#'
#' Server-side ASSIGN method
#'
#' @param ListName.S String | Name of a list object on server
#' @param ObjectName.S String | Name of object inside list
#'
#' @return The object to be extracted
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ExtractFromListDS <- function(ListName.S,
                              ObjectName.S)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)

  # --- Argument Assertions ---
  assert_that(is.string(ListName.S),
              is.string(ObjectName.S))

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Object <- eval(parse(text = paste0(ListName.S, "$", ObjectName.S)), envir = parent.frame())

#-------------------------------------------------------------------------------
  return(Object)
}
