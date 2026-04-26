
#' FinalizeDataRemediation
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' Unremediated / Ineligible data is turned into NA per default.
#' Optional factor conversion establishes level order where appropriate.
#'
#' @param TargetVector \code{vector} - Feature to be finalized
#' @param EligibleValueSet \code{data.frame} - Containing set of eligible values
#' @param UnremediatedValues.Substitute \code{logical} - Whether to substitute all unremediated (ineligible) values \code{NA} - Default: \code{TRUEs}
#' @param UnremediatedValues.Substitution \code{string} - Which string should substitute unremediated (ineligible) values? If set "NA", the values will be substituted by \code{NA}
#' @param ConvertToFactor \code{logical} - Whether to convert vector to factor - Default: \code{FALSE}
#' @param AssignFactorLabels \code{logical} - Whether to assign factor labels during factor conversion - Default: \code{FALSE}
#'
#' @return A \code{vector} (or factor)
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FinalizeDataRemediation <- function(TargetVector,
                                    EligibleValueSet,
                                    UnremediatedValues.Substitute = TRUE,
                                    UnremediatedValues.Substitution = ".Ineligible",
                                    ConvertToFactor = FALSE,
                                    AssignFactorLabels = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # TargetVector <- DataSet$RadiationTherapy$ApplicationType
  # EligibleValueSet
  # UnremediatedValues.Substitute = TRUE
  # UnremediatedValues.Substitution = ".Ineligible"
  # ConvertToFactor = FALSE
  # AssignFactorLabels = FALSE

  # --- Argument Validation ---
  assert_that(is.vector(TargetVector),
              is.data.frame(EligibleValueSet),
              is.flag(UnremediatedValues.Substitute),
              is.string(UnremediatedValues.Substitution),
              is.flag(ConvertToFactor),
              is.flag(AssignFactorLabels))

#-------------------------------------------------------------------------------

  if (AssignFactorLabels == TRUE) { ConvertToFactor <- TRUE }

  if (UnremediatedValues.Substitute == TRUE)
  {
      EligibleValueSet <- EligibleValueSet %>%
                              arrange(FactorRank)

      # Get eligible values (and their labels in case the feature should be factorized)
      EligibleValues <- EligibleValueSet$Value.Curated
      EligibleValueLabels <- EligibleValueSet$Label.Curated

      # Get logical vector indicating eligibility of values
      IsEligible <- TargetVector %in% EligibleValues

      Substitution <- ifelse(UnremediatedValues.Substitution == "NA",
                             NA_character_,
                             UnremediatedValues.Substitution)

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
