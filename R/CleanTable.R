
#' CleanTable
#'
#' Perform exclusion of invalid table entries.
#'
#' @param Table \code{data.frame} or \code{tibble} - The table object to be cleaned
#' @param TableNameLookup \code{string} - The table name(s) to be looked up in 'FeatureObligations$RuleSet'. Can be a single string or a character vector.
#' @param FeatureObligations \code{list}
#'        \itemize{\item RuleSet \code{data.frame}
#'                 \item RuleSet.Profile \code{character} - Profile name defining strict and trans-feature rules for obligatory feature content. Profile name must be stated in \code{FeatureObligations$RuleSet}}
#' @param IDFeatureName \code{string} - Name of table feature that has no semantic meaning and can be ignored when determining redundance - Default: NULL
#' @param RemoveEmptyStrings \code{logical} - Whether empty strings ('') should be removed and replaced by \code{NA} - Default: \code{TRUE}
#' @param RemoveRedundantEntries \code{logical} - Whether redundant entries should be removed
#'
#' @return \code{tibble} - Clean table
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CleanTable <- function(Table,
                       TableNameLookup,
                       FeatureObligations,
                       IDFeatureName = NULL,
                       RemoveEmptyStrings = TRUE,
                       RemoveRedundantEntries = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSet$BioSampling
  # TableNameLookup <- "BioSampling"
  # RemoveEmptyStrings <- TRUE
  # RemoveRedundantEntries <- TRUE
  # FeatureObligations$RuleSet <- Meta_FeatureObligations
  # FeatureObligations$RuleSet.Profile <- "Default"
  # IDFeatureName <- ""

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.character(TableNameLookup),
              is.list(FeatureObligations),
              is.flag(RemoveEmptyStrings),
              is.flag(RemoveRedundantEntries))
  if (!is.null(IDFeatureName)) { assert_that(is.string(IDFeatureName)) }

#-------------------------------------------------------------------------------

  # 1) Remove empty strings in character features (replace them with NAs)
  #-----------------------------------------------------------------------------
  if (RemoveEmptyStrings == TRUE)
  {
      Table <- Table %>%
                    mutate(across(where(is.character),
                                  ~ case_when(.x == "" ~ NA_character_,
                                              .default = .x)))
  }


  # 2) Remove entries that have missing values in strictly obligatory features
  #-----------------------------------------------------------------------------

  # Get table's set of (strictly) obligatory features from meta data passed to function
  ObligatoryFeatures <- FeatureObligations$RuleSet %>%
                            rename(Rule = all_of(FeatureObligations$RuleSet.Profile)) %>%      # Renaming feature based on passed argument
                            filter(Table %in% TableNameLookup, Rule == "Obligatory") %>%
                            pull(Feature)

  # Filter out entries that have NAs in any of strictly obligatory features
  CleanedTable <- Table %>%
                      filter(if_all(all_of(ObligatoryFeatures), ~ !(is.na(.x) | (is.character(.x) & .x == ""))))


  # 3) Remove duplicate entries (optional)
  #-----------------------------------------------------------------------------

  if (RemoveRedundantEntries == TRUE)
  {
      # OLD
      # Get table's primary key feature (ID column), which is being ignored when determining if table entries are redundant
      # IDFeature <- dsCCPhos::Meta_Features %>%
      #                   filter(TableName_Curated %in% TableName, IsPrimaryKey == TRUE) %>%
      #                   pull(FeatureName_Curated)

      CleanedTable <- CleanedTable %>%
                          distinct(across(-all_of(IDFeatureName)), .keep_all = TRUE)
  }


  # 4) Remove entries that are not consistent with special trans-feature obligation rules
  #-----------------------------------------------------------------------------

  # Get current table's set of trans-feature obligation rules (stated as pseudo-code), if there are any defined in meta data passed to function
  TransFeatureRules <- FeatureObligations$RuleSet %>%
                            rename(Rule = all_of(FeatureObligations$RuleSet.Profile)) %>%
                            filter(Table %in% TableNameLookup, !is.na(Rule), Rule != "NA", Rule != "Obligatory") %>%
                            distinct(Rule) %>%
                            pull()

  # If there are any of these rules for the current table, filter out entries that are not consistent with them
  if (!is_empty(TransFeatureRules))
  {
      TransFeatureRules <- TransFeatureRules %>%
                                CompileTransFeatureRules() %>%      # ... use CompileTransFeatureRule() to turn the pseudo-code into evaluable strings...
                                lapply(., rlang::parse_expr) %>%      # ... then transform them into a list of expressions
                                setNames(paste0("TransFeatureRule_", 1:length(TransFeatureRules)))      # The keys of the list will be the column names of auxiliary features used to determine whether entries are consistent with rules (s. proceedings)

      CleanedTable <- CleanedTable %>%
                          mutate(!!!TransFeatureRules) %>%      # Use list of expressions created earlier to apply trans-feature rules to all entries...
                          filter(if_all(starts_with("TransFeatureRule"), ~ .x == TRUE)) %>%      # ... and filter out any entries that are not consistent with those rules
                          select(-starts_with("TransFeatureRule"))      # Remove auxiliary columns
  }

#-------------------------------------------------------------------------------
  return(CleanedTable)
}
