
#' GetFrequencyTableDS
#'
#' Return table of absolute and relative value frequencies for a nominal / ordinal feature.
#'
#' @param TableName.S \code{string} - Name of the Data frame that contains the feature
#' @param FeatureName.S \code{string} - Name of feature
#' @param GroupingFeatureName.S \code{string} - Name of optional grouping feature
#'
#' @return A \code{tibble} containing absolute and relative frequencies
#' @export
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetFrequencyTableDS <- function(TableName.S,
                                FeatureName.S,
                                GroupingFeatureName.S = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)
  require(rlang)
  require(stats)

  # --- For Testing Purposes ---
  # Table <- ADS$Patients
  # FeatureName.S <- "TNM_T"
  # GroupingFeatureName.S <- "LastVitalStatus"

  # --- Argument Assertions ---
  assert_that(is.string(TableName.S),
              is.string(FeatureName.S))
  if (!is.null(GroupingFeatureName.S)) { assert_that(is.string(GroupingFeatureName.S)) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  # Evaluate feature in question
  Feature <- Table[[FeatureName.S]]

  # Stop if Feature is of class 'numeric' or similar
  if (class(Feature) %in% c("double", "integer", "numeric")) { stop(paste0("The specified feature '", FeatureName.S, "' is of class '", class(Feature), "' and therefore not suitable."), call. = FALSE) }


  # Initiate FrequencyTable object
  FrequencyTable <- tibble()

  # Get count of valid (non-missing) values in Feature
  N_Valid <- sum(!is.na(Feature))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CHARACTER feature
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (class(Feature) == "character" & N_Valid > 0)
  {
      # Tibble containing absolute and relative frequencies
      FrequencyTable <- as_tibble(table(Feature, useNA = "no")) %>%
                            rename(c(AbsoluteFrequency = "n",
                                     Value = "Feature")) %>%
                            arrange(desc(AbsoluteFrequency)) %>%
                            mutate(RelativeFrequency = AbsoluteFrequency / N_Valid)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LOGICAL feature
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (class(Feature) == "logical" & N_Valid > 0)
  {
      # Tibble containing absolute and relative frequencies
      FrequencyTable <- as_tibble(table(Feature, useNA = "no")) %>%
                            rename(c(AbsoluteFrequency = "n",
                                     Value = "Feature")) %>%
                            mutate(RelativeFrequency = AbsoluteFrequency / N_Valid)
  }


#-------------------------------------------------------------------------------
  return(FrequencyTable)
}


