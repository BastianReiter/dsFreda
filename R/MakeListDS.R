
#' MakeListDS
#'
#' Bundle objects in a \code{list} object.
#'
#' Server-side ASSIGN method
#'
#' @param ObjectNames.S \code{character} - Names of objects to be bundled in a list. Can be a \code{named vector} if names in list should differ from symbol names (with names in named vector being the names in list).
#'
#' @return A \code{data.frame} resulting from filter operation
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MakeListDS <- function(ObjectNames.S)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)

  # --- Argument Assertions ---
  assert_that(is.character(ObjectNames.S))

#-------------------------------------------------------------------------------

  List <- list()

  for (i in 1:length(ObjectNames.S))
  {
      if (exists(ObjectNames.S[i], where = parent.frame()) == TRUE)
      {
          Object <- get(ObjectNames.S[i], envir = parent.frame())
          NameInList <- names(ObjectNames.S)[i]
          List[[NameInList]] <- Object

      } else {
          stop(paste0("Object '", ObjectNames.S[i], "' does not exist in Server Workspace."), call. = FALSE)
      }
  }

#-------------------------------------------------------------------------------
  return(List)
}
