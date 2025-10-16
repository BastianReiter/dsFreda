
#' GetTableCheckDS
#'
#' Wrapper function for \code{dsFreda::CheckTable()}. Checks out a \code{data.frame} and returns an informative list object.
#'
#' Server-side AGGREGATE method
#'
#' @param TableName.S \code{string} - Name of \code{data.frame} or \code{tibble}
#' @param RequiredFeatureNames.S Optional \code{character} - Optional names of required features - Default: \code{names()} applied to Table evaluated from \code{TableName.S}
#' @param EligibleValueSets.S Optional \code{list} of \code{character vectors} containing sets of eligible values for corresponding features
#' @param GetTemplate.S \code{logical} - If set to \code{TRUE}, the function returns a template incorporating required feature names without actually checking an existing table
#'
#' @return A \code{list} containing informative meta data about a \code{data.frame}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetTableCheckDS <- function(TableName.S,
                            RequiredFeatureNames.S = NULL,
                            EligibleValueSets.S = NULL,
                            GetTemplate.S = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(TableName.S),
              is.flag(GetTemplate.S))
  if (!is.null(RequiredFeatureNames.S)) { assert_that(is.character(RequiredFeatureNames.S)) }
  if (!is.null(EligibleValueSets.S)) { assert_that(is.list(EligibleValueSets.S)) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  # Initiate output object
  TableCheck <- NULL

  # If 'Table' is neither data.frame nor tibble, return NULL
  if (!("data.frame" %in% class(Table))) { return(NULL) }

  # Create only template if one of the conditions below is met
  if (length(Table) == 0 || GetTemplate.S == TRUE)
  {
      TableCheck <- CheckTable(Table = NULL,
                               RequiredFeatureNames = RequiredFeatureNames.S)

  } else {

      TableCheck <- CheckTable(Table = Table,
                               RequiredFeatureNames = RequiredFeatureNames.S,
                               EligibleValueSets = EligibleValueSets.S)
  }

#-------------------------------------------------------------------------------
  return(TableCheck)
}
