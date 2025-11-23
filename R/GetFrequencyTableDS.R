
#' GetFrequencyTableDS
#'
#' Return table of absolute and relative value frequencies for a nominal / ordinal feature.
#'
#' @param TableName.S \code{string} - Name of the Data frame that contains the feature
#' @param FeatureName.S \code{string} - Name of feature
#' @param GroupingFeatureName.S \code{string} - Name of optional grouping feature
#' @param RemoveNA.S \code{logical} - Indicating whether missing values should be removed prior to frequency calculation - Default: \code{FALSE}
#'
#' @return A \code{tibble} containing absolute and relative frequencies
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetFrequencyTableDS <- function(TableName.S,
                                FeatureName.S,
                                GroupingFeatureName.S = NULL,
                                RemoveNA.S = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- ADS$Diagnosis
  # FeatureName.S <- "TNM.T"
  # GroupingFeatureName.S <- "Grading"

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
  assert_that(class(Table[[FeatureName.S]]) %in% c("character", "logical", "factor"),
              msg = paste0("The specified feature '", FeatureName.S, "' is of class '", class(Table[[FeatureName.S]]), "' and therefore not suitable."))

  # Optionally perform corresponding validation for 'GroupingFeatureName.S'
  if (!is.null(GroupingFeatureName.S))
  {
      assert_that(GroupingFeatureName.S %in% names(Table),
                  msg = paste0("'", GroupingFeatureName.S, "' is not a valid feature name in '", TableName.S, "'."))
      assert_that(class(Table[[GroupingFeatureName.S]]) %in% c("character", "logical", "factor"),
                  msg = paste0("The specified grouping feature '", GroupingFeatureName.S, "' is of class '", class(Table[[GroupingFeatureName.S]]), "' and therefore not suitable."))
  }

  # Get Freda privacy settings
  PrivacyProfile = dsFreda::Set.Privacy$Profile
  NThreshold <- dsFreda::Set.Privacy$NThreshold

  # If 'PrivacyProfile' is 'loose', set NThreshold to -1, which effectively prevents subsequent masking
  if (PrivacyProfile == "loose") { NThreshold <- -1 }

#-------------------------------------------------------------------------------

  # Grouping of 'Table' depending on passing of additional grouping variable
  if (!is.null(GroupingFeatureName.S))
  {
      FrequencyTable <- Table %>%
                            group_by(!!sym(GroupingFeatureName.S),
                                     !!sym(FeatureName.S))
  } else {

      FrequencyTable <- Table %>%
                            group_by(!!sym(FeatureName.S))
  }

  # Tibble containing absolute and relative frequencies
  FrequencyTable <- FrequencyTable %>%
                            summarize(AbsoluteFrequency = n()) %>%
                            mutate(RelativeFrequency = AbsoluteFrequency / sum(AbsoluteFrequency)) %>%
                            arrange(desc(AbsoluteFrequency), .by_group = TRUE) %>%
                        ungroup() %>%
                        mutate(NThreshold = NThreshold,
                               IsMasked = case_when(AbsoluteFrequency <= NThreshold ~ TRUE,
                                                    .default = FALSE),
                               AbsoluteFrequency = case_when(IsMasked == TRUE ~ NA,
                                                             .default = AbsoluteFrequency),
                               RelativeFrequency = case_when(IsMasked == TRUE ~ NA,
                                                             .default = RelativeFrequency))

#-------------------------------------------------------------------------------
  return(as.data.frame(FrequencyTable))
}


