
#' JoinTablesDS
#'
#' Join tables on server, making use of \code{dplyr} mutating join operations.
#'
#' Server-side ASSIGN method
#'
#' @param TableNameA.S \code{string} - Name of Table A on server
#' @param TableNameB.S \code{string} - Name of Table B on server
#' @param ByStatement.S \code{string} - The insides of a \code{dplyr::join_by()}-Statement defining how to join tables
#' @param JoinType.S \code{string} - Name of \code{dplyr::join}-function used, one of:
#'                     \itemize{\item 'left_join' (Default)
#'                              \item 'right_join'
#'                              \item 'full_join'
#'                              \item 'inner_join'}
#'
#' @return A \code{data.frame} resulting from join operation
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
JoinTablesDS <- function(TableNameA.S,
                         TableNameB.S,
                         ByStatement.S,
                         JoinType.S = "left_join")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)

  # --- For Testing Purposes ---
  # TableNameA.S <- "ADS_Patient"
  # TableNameB.S <- "ADS_Diagnosis"
  # ByStatement.S <- "PatientID"
  # JoinType.S <- "left_join"

  # --- Argument Assertions ---
  assert_that(is.string(TableNameA.S),
              is.string(TableNameB.S),
              is.string(ByStatement.S),
              JoinType.S %in% c("left_join", "right_join", "full_join", "inner_join"))

#-------------------------------------------------------------------------------

  # Get local objects: Parse expression and evaluate
  TableA <- eval(parse(text = TableNameA.S), envir = parent.frame())
  TableB <- eval(parse(text = TableNameB.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  if (JoinType.S %in% c("left_join", "right_join", "full_join", "inner_join"))
  {
      Output <- eval(parse(text = paste0(JoinType.S, "(TableA, TableB, by = join_by(", ByStatement.S, "))")))

  } else {
      stop(paste0("ERROR: 'JoinType.S' does not provide a valid join operation!"))
  }

#-------------------------------------------------------------------------------
  return(as.data.frame(Output))
}
