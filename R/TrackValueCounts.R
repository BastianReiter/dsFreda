
#' TrackValueCounts
#'
#' Track occurrence of a feature's unique values, their eligibility and frequency
#'
#' @param DataFrame \code{data.frame}
#' @param FeatureNames \code{character} vector containing names of features to be tracked
#' @param EligibleValues An optional \code{list} of vectors containing eligible value sets. Name of vector object gives name of feature, vector values give eligible values.
#' @param TransformationStage String representing transformation stage (usually "Raw" / "Transformed" / "Final")
#'
#' @return A \code{tibble}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TrackValueCounts <- function(DataFrame,
                             FeatureNames,
                             EligibleValues = NULL,
                             TransformationStage = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataFrame <- df_CDS_Diagnosis
  # FeatureNames <- names(ls_MonitorFeatures_All$Diagnosis)
  # TransformationStage <- "Final"

  # --- Argument Validation ---
  assert_that(is.data.frame(DataFrame),
              is.character(FeatureNames))
  if (!is.null(EligibleValues)) { assert_that(is.list(EligibleValues)) }
  if (!is.null(TransformationStage)) { assert_that(is.string(TransformationStage)) }

#-------------------------------------------------------------------------------

  df_Output <- tibble(Feature = character(),
                      Value = character(),
                      IsValueEligible = logical(),
                      TransformationStage = character(),
                      Frequency = numeric())

  for (feature in FeatureNames)
  {
      vc_ContingencyTable <- table(DataFrame[[feature]], useNA = "always")      # table() returns a contingency table in the form of a named vector. Vector element names are the occurring values, vector element values are the corresponding absolute frequencies.

      df_FeatureRows <- tibble::tibble(Feature = feature,
                                       Value = names(vc_ContingencyTable),      # Get all distinct values from contingency table
                                       IsValueEligible = NA,
                                       TransformationStage = TransformationStage,
                                       Frequency = as.numeric(vc_ContingencyTable)) %>%      # Get absolute frequencies from contingency table
                                    arrange(Value)

      # If optional set of eligible values is passed
      if (!is.null(EligibleValues))
      {
          vc_EligibleValues <- EligibleValues[[feature]]      # Get eligible values for current feature

          df_FeatureRows <- df_FeatureRows %>%
                                mutate(IsValueEligible = ifelse(is.null(vc_EligibleValues),      # If set of eligible values is empty...
                                                                NA,                              # ... set eligibility NA ...
                                                                Value %in% vc_EligibleValues)) %>%      # ... otherwise look up which values are in it
                                arrange(IsValueEligible, Value)
      }

      df_Output <- bind_rows(df_Output, df_FeatureRows)
  }

#-------------------------------------------------------------------------------
  return(df_Output)
}
