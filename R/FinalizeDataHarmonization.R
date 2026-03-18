
#' FinalizeDataHarmonization
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' Ineligible data (including data that could not be transformed) is turned into NA per default.
#' Optional factor conversion establishes level order where appropriate.
#'
#' @param TargetVector \code{vector} - Feature to be finalized
#' @param EligibleValueSet \code{data.frame} - Containing set of eligible values
#' @param UnharmonizedValues.Substitute \code{logical} - Whether to substitute all unharmonized (ineligible) values \code{NA} - Default: \code{FALSE}
#' @param UnharmonizedValues.Substitution \code{string} - Which string should substitute unharmonized (ineligible) values? If set "NA", the values will be substituted by \code{NA}
#' @param ConvertToFactor \code{logical} - Whether to convert vector to factor - Default: \code{FALSE}
#' @param AssignFactorLabels \code{logical} - Whether to assign factor labels during factor conversion - Default: \code{FALSE}
#'
#' @return A \code{vector} (or factor)
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FinalizeDataHarmonization <- function(TargetVector,
                                      EligibleValueSet,
                                      UnharmonizedValues.Substitute = TRUE,
                                      UnharmonizedValues.Substitution = ".Ineligible",
                                      ConvertToFactor = FALSE,
                                      AssignFactorLabels = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TargetVector <- DataSet$RadiationTherapy$ApplicationType
  # EligibleValueSet
  # UnharmonizedValues.Substitute = TRUE
  # UnharmonizedValues.Substitution = ".Ineligible"
  # ConvertToFactor = FALSE
  # AssignFactorLabels = FALSE

  # --- Argument Validation ---
  assert_that(is.vector(TargetVector),
              is.data.frame(EligibleValueSet),
              is.flag(UnharmonizedValues.Substitute),
              is.string(UnharmonizedValues.Substitution),
              is.flag(ConvertToFactor),
              is.flag(AssignFactorLabels))

#-------------------------------------------------------------------------------

  if (AssignFactorLabels == TRUE) { ConvertToFactor <- TRUE }

  if (UnharmonizedValues.Substitute == TRUE)
  {
      EligibleValueSet <- EligibleValueSet %>%
                              arrange(FactorRank)

      # Get eligible values (and their labels in case the feature should be factorized)
      EligibleValues <- EligibleValueSet$Value.Curated
      EligibleValueLabels <- EligibleValueSet$Label.Curated

      # Get logical vector indicating eligibility of values
      IsEligible <- TargetVector %in% EligibleValues

      Substitution <- ifelse(UnharmonizedValues.Substitution == "NA",
                             NA_character_,
                             UnharmonizedValues.Substitution)

      # Substitute non-missing ineligible values
      TargetVector[!is.na(TargetVector) & IsEligible == FALSE] <- Substitution
  }

  # if (ConvertToFactor == TRUE)
  # {
  #     TargetVector <- factor(TargetVector,
  #                            levels = EligibleValues,
  #                            labels = ifelse(AssignFactorLabels == TRUE,
  #                                            EligibleValueLabels,
  #                                            EligibleValues))
  # }

#-------------------------------------------------------------------------------
  return(TargetVector)
}
