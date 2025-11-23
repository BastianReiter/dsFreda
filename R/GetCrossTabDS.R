
#' GetCrossTabDS
#'
#' Perform cross tabulation with arbitrary number of table features.
#'
#' Server-side AGGREGATE method
#'
#' @param TableName.S \code{string} - Name of \code{data.frame}
#' @param FeatureNames.S \code{string} - Names of features to be crossed, separated by ','
#' @param RemoveNA.S \code{logical} - Indicating whether missing values should be removed prior to cross tabulation - Default: \code{FALSE}
#'
#' @return A \code{list} containing
#'            \itemize{ \item CrossTab (\code{data.frame})
#'                      \item ChiSq.PValue (\code{numeric}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetCrossTabDS <- function(TableName.S,
                          FeatureNames.S,
                          RemoveNA.S = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- AugmentationOutput$AugmentedDataSet$Patient
  # FeaturesNames.S <- c("Sex", "CountDiagnoses")

  # --- Argument Validation ---
  assert_that(is.string(TableName.S),
              is.string(FeatureNames.S),
              is.logical(RemoveNA.S))

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

  # Decode 'FeatureNames.S' and get separate feature names
  FeatureNames <- .decode_tidy_eval(FeatureNames.S, .get_encode_dictionary())
  FeatureNames <- strsplit(FeatureNames, ",")[[1]] %>% str_trim()

  # Check if features are part of 'Table' and if they have a suitable class
  for (featurename in FeatureNames)
  {
      assert_that(featurename %in% names(Table),
                  msg = paste0("'", featurename, "' is not a valid feature name in '", TableName.S, "'."))
      assert_that(class(Table[[featurename]]) %in% c("character", "logical", "factor"),
                  msg = paste0("The specified feature '", featurename, "' is of class '", class(Table[[featurename]]), "' and therefore not suitable."))
  }

  # Get Freda privacy settings
  PrivacyProfile = dsFreda::Set.Privacy$Profile
  NThreshold <- dsFreda::Set.Privacy$NThreshold

  # If 'PrivacyProfile' is 'loose' lower NThreshold to -1 which effectively prevents subsequent masking
  if (PrivacyProfile == "loose") { NThreshold <- -1 }

  # Depending on argument 'RemoveNA.S' define option that will control removal of NAs in cross tabulation
  OptionUseNA <- "ifany"
  if (RemoveNA.S == TRUE) { OptionUseNA <- "no" }

#-------------------------------------------------------------------------------

  # Get cross tab with joint counts
  CrossTab <- do.call(table, c(Table[FeatureNames], list(useNA = OptionUseNA))) %>%
                  as.data.frame() %>%
                  rename(JointCount = "Freq")

  # Calculate marginal counts and add them to 'CrossTab'
  for (featurename in FeatureNames)
  {
      MarginalCounts <- CrossTab %>%
                            group_by(across(all_of(featurename))) %>%
                                summarize(!!sym(paste0("MargCount.", featurename)) := sum(JointCount, na.rm = TRUE)) %>%
                            ungroup()

      CrossTab <- CrossTab %>%
                      left_join(MarginalCounts,
                                by = join_by(!!sym(featurename)))
  }


  # Mask all Counts that are below 'NThreshold'
  CrossTab <- CrossTab %>%
                  mutate(across(c("JointCount", starts_with("MargCount.")),
                                ~ case_when(.x <= NThreshold ~ TRUE,
                                            .default = FALSE),
                                .names = "IsMasked.{.col}")) %>%
                  mutate(across(c("JointCount", starts_with("MargCount.")),
                                ~ case_when(.x <= NThreshold ~ NA,
                                            .default = .x)))


  # Add relative frequencies (including marginal frequencies)
  # CrossTab <- CrossTab %>%
  #                 mutate(JointRelFreq = JointCount / sum(JointCount, na.rm = TRUE), .after = JointCount) %>%
  #                 mutate(across(starts_with("MargCount."),
  #                               ~ .x / sum(JointCount, na.rm = TRUE),
  #                               .names = "MargRelFreq.{.col}")) %>%
  #                 rename_with(.cols = starts_with("MargRelFreq.MargCount."),
  #                             .fn = ~ str_remove(.x, "MargCount."))


  # Perform Chi-Squared-Test and retrieve p-value
  ChiSq.PValue <- do.call(table, c(Table[FeatureNames])) %>%      # The cross tab used for test ignores NA values
                      as.matrix(.) %>%
                      chisq.test(x = .) %>%
                      pluck("p.value")


#-------------------------------------------------------------------------------
  if (PrivacyProfile == "loose")
  {
      # Return list with CrossTab and ChiSq.PValue
      return(list(CrossTab = as.data.frame(CrossTab),
                  ChiSq.PValue = ChiSq.PValue))
  } else {
      # !!! TO DO: Implement cases for other Disclosure profiles
      return(list(ChiSq.PValue = ChiSq.PValue))
  }
}
