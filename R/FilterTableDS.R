
#' FilterTableDS
#'
#' Filter a table (\code{data.frame}) making use of \code{dplyr} and \code{stringr}.
#'
#' Server-side ASSIGN method
#'
#' @param TableName.S \code{string} - Name of \code{data.frame}
#' @param FilterExpression.S \code{string} - \code{dplyr::filter} expression as string
#' @param GroupBy.S \code{string} - Optional \code{dplyr::group_by} expression as string
#'
#' @return A \code{data.frame} resulting from filter operation
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FilterTableDS <- function(TableName.S,
                          FilterExpression.S,
                          GroupBy.S = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TableName.S <- "CDS$Patient"
  # FilterExpression.S <- "LastVitalStatus == 'Alive'"
  # FilterExpression.S <- "str_starts(ICD10Code, 'C34')"
  # GroupBy.S <- "Gender"

  # --- Argument Validation ---
  assert_that(is.string(TableName.S),
              is.string(FilterExpression.S))
  if (!is.null(GroupBy.S)) { assert_that(is.string(GroupBy.S)) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

  # Optionally employ grouping
  if (!is.null(GroupBy.S))
  {
      Table <- eval(parse(text = paste0("dplyr::group_by(Table, ", GroupBy.S, ")")))
  }

  # Decode 'FilterExpression.S'
  FilterExpression.S <- .decode_tidy_eval(FilterExpression.S, .get_encode_dictionary())

  # Perform filter operation
  Table <- eval(parse(text = paste0("dplyr::filter(Table, ", FilterExpression.S, ")")))

  # Ungroup tibble
  Table <- eval(parse(text = "dplyr::ungroup(Table)"))

  # To prevent data disclosure
  if (nrow(Table) < 5) { stop("Disclosure Warning: The resulting data.frame has less rows than allowed.")}

  # Return as data.frame
  return(as.data.frame(Table))
}
