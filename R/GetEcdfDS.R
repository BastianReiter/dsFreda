
#' GetEcdfDS
#'
#' Return empirical cumulative distribution function for a numeric feature.
#'
#' @param TableName.S \code{string} - Name of the \code{data.frame} holding relevant feature(s)
#' @param FeatureName.S \code{string} - Name of numeric feature
#' @param GroupingFeatureName.S \code{string} - Name of optional grouping feature
#'
#' @return A function of class \code{ecdf}, computed by \code{stats::ecdf()}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetEcdfDS <- function(TableName.S,
                      FeatureName.S,
                      GroupingFeatureName.S = NULL,
                      RemoveNA.S = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- Analysis
  # FeatureName.S <- "PatientAgeAtDiagnosis"
  # GroupingFeatureName.S <- "UICCStageCategory"
  # RemoveNA.S <- TRUE

  # --- Argument Validation ---
  assert_that(is.string(TableName.S),
              is.string(FeatureName.S),
              is.flag(RemoveNA.S))
  if (!is.null(GroupingFeatureName.S)) { assert_that(is.string(GroupingFeatureName.S))
                                         assert_that(GroupingFeatureName.S != FeatureName.S,
                                                     msg = "Values for 'GroupingFeatureName.S' and 'FeatureName.S' can not be identical.") }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

  # Stop if 'FeatureName.S' is not part of 'Table' or if does not have a suitable class
  assert_that(FeatureName.S %in% names(Table),
              msg = paste0("'", FeatureName.S, "' is not a valid feature name in '", TableName.S, "'."))
  assert_that(class(Table[[FeatureName.S]]) %in% c("numeric", "integer", "double"),
              msg = paste0("The specified feature '", FeatureName.S, "' is of class '", class(Table[[FeatureName.S]]), "' and therefore not suitable."))

  # Perform corresponding validation for 'GroupingFeatureName.S'
  if (!is.null(GroupingFeatureName.S))
  {
      assert_that(GroupingFeatureName.S %in% names(Table),
                  msg = paste0("'", GroupingFeatureName.S, "' is not a valid feature name in '", TableName.S, "'."))
      assert_that(class(Table[[GroupingFeatureName.S]]) %in% c("character", "logical", "factor"),
                  msg = paste0("The specified grouping feature '", GroupingFeatureName.S, "' is of class '", class(Table[[GroupingFeatureName.S]]), "' and therefore not suitable."))
  }

#-------------------------------------------------------------------------------

  # Get ECDF with stats::ecdf()
  ECDF <- Table %>%
              { if (RemoveNA.S == TRUE) { filter(., !is.na({{ FeatureName.S }})) } else {.} } %>%
              { if (!is.null(GroupingFeatureName.S)) { group_by(., !!sym(GroupingFeatureName.S)) } else {.} } %>%
              summarize(ECDF = list(ecdf(.data[[FeatureName.S]])))

#-------------------------------------------------------------------------------
  return(ECDF)
}


