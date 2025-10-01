
#' HarmonizeFeature
#'
#' This function performs harmonizing data transformation on an isolated feature based on passed settings.
#' Harmonization is attempted in a step-wise approach, incorporating various methods:
#' \enumerate{\item Transformative expressions (e.g. functions like \code{str_to_upper()})
#'            \item Fuzzy String Matching
#'            \item Dictionary look-up}
#' The selection of these methods and the order in which they are applied can be determined in the 'Methods' argument.
#'
#' @param Feature \code{vector} containing data to be transformed
#' @param FeatureName \code{string} - Optionally passed if some transformative expressions assume hosting data.frame context
#' @param ContextDataFrame \code{data.frame} - Optionally passed if some transformative expressions assume hosting data.frame context
#' @param Methods \code{list} - Contains data on the selection of methods to use for each feature, their application order and more feature-specific settings
#' @param EligibleValueSet \code{character} vector containing set of eligible feature values
#' @param TransformativeExpressions \code{data.frame} - Contains set of expressions like functions used to transform data values
#' @param FuzzyStringMatching \code{list} - Contains settings for Fuzzy String Matching
#' @param Dictionary \code{character} - Contains dictionary data used to look up and replace data values
#'
#' @return The input \code{vector} with transformed data values
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HarmonizeFeature <- function(Feature,
                             FeatureName = NULL,
                             ContextDataFrame = NULL,
                             Methods,
                             EligibleValueSet = NULL,
                             TransformativeExpressions = NULL,
                             FuzzyStringMatching = NULL,
                             Dictionary = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)
  require(tidyr)
  require(purrr)
  require(stringr)

  # --- For Testing Purposes ---
  # Feature <- RawDataSet$RDS.Staging$uicc_stadium
  # FeatureName <- "UICCStage"
  # ContextDataFrame <- RawDataSet$RDS.Staging
  # names(ContextDataFrame) <- dsCCPhos::Meta.Features %>% filter(TableName.Curated == "Staging") %>% pull(FeatureName.Curated)
  # tablename <- "Staging"
  # EligibleValueSet <- dsCCPhos::Meta.Values %>%
  #                           filter(Table == tablename,
  #                                  Feature == FeatureName) %>%
  #                           pull(Value.Curated)
  # Methods <- as.list(dsCCPhos::Set.DataHarmonization %>% filter(Table == tablename, Feature == FeatureName))
  # TransformativeExpressions = dsCCPhos::Set.TransformativeExpressions %>% filter(Table == tablename, Feature == FeatureName)
  # FuzzyStringMatching <- as.list(dsCCPhos::Set.FuzzyStringMatching %>% filter(Table == tablename, Feature == FeatureName))
  # Dictionary <- dsCCPhos::Set.Dictionary %>% filter(Table == tablename, Feature == FeatureName) %>% pull(var = NewValue, name = LookupValue)

  # --- Argument Assertions ---
  assert_that(is.vector(Feature),
              is.list(Methods))
  if (!is.null(FeatureName)) { assert_that(is.string(FeatureName),
                                           !is.null(ContextDataFrame),
                                           FeatureName %in% names(ContextDataFrame)) }
  if (!is.null(ContextDataFrame)) { assert_that(is.data.frame(ContextDataFrame),
                                                !is.null(FeatureName)) }
  if (!is.null(EligibleValueSet)) { assert_that(is.character(EligibleValueSet)) }
  if (!is.null(TransformativeExpressions)) { assert_that(is.data.frame(TransformativeExpressions)) }
  if (!is.null(Dictionary)) { assert_that(is.character(Dictionary)) }
  if (!is.null(FuzzyStringMatching)) { assert_that(is.list(FuzzyStringMatching)) }

#===============================================================================

#-------------------------------------------------------------------------------
# First, define functions that apply each method on 'Feature'
#-------------------------------------------------------------------------------
#   - RunTransformativeExpressions()
#   - RunFuzzyStringMatching()
#   - RunDictionary()
#-------------------------------------------------------------------------------

  RunTransformativeExpressions <- function(Vector,
                                           FeatureName.C = FeatureName,
                                           ContextDataFrame.C = ContextDataFrame,
                                           TransformativeExpressions.C = TransformativeExpressions)
  {
      require(stringr)

      Expressions <- TransformativeExpressions.C %>%
                          arrange(EvaluationOrder) %>%
                          filter(!is.na(Feature) & !is.na(Expression)) %>%
                          pull(Expression)

      if (length(Expressions) > 0)
      {
          for (expression in Expressions)
          {
              if (length(ContextDataFrame.C) > 0 && !is.null(FeatureName.C))
              {
                  ContextDataFrame.C <- ContextDataFrame.C %>%
                                            eval(expr = parse(text = paste0("mutate(., ",
                                                                            FeatureName.C,
                                                                            " = ",
                                                                            str_replace(expression, ".X", FeatureName.C),
                                                                            ")")))
                  Vector <- ContextDataFrame.C %>% pull({{ FeatureName.C }})

              } else {

                  Vector <- eval(expr = parse(text = str_replace(expression, ".X", "Vector")))
              }
          }
      }

      return(Vector)
  }

#-------------------------------------------------------------------------------

  RunFuzzyStringMatching <- function(Vector,
                                     EligibleValueSet.C = EligibleValueSet,
                                     MatchToDictionaryLookups = Methods$MatchToDictionaryLookupsInFSM,      # Note: Dictionary data can be useful in string matching, see below
                                     Dictionary.C = Dictionary,
                                     FuzzyStringMatching.C = FuzzyStringMatching)
  {
      EligibleStrings <- EligibleValueSet.C
      if (MatchToDictionaryLookups == TRUE) { EligibleStrings <- unique(c(EligibleStrings, names(Dictionary.C))) }      # The look-up values in dictionary data can be included in the set of target string matches

      if (length(EligibleStrings) > 0 && length(FuzzyStringMatching.C) > 0)
      {
          Vector <- GetFuzzyStringMatches(Vector = Vector,
                                          EligibleStrings = EligibleStrings,
                                          PreferredMethod = FuzzyStringMatching.C$PreferredMethod,
                                          FindBestMethod = FuzzyStringMatching.C$FindBestMethod,
                                          Tolerance = FuzzyStringMatching.C$Tolerance,
                                          Preprocessing.FlattenCase = FuzzyStringMatching.C$Preprocessing.FlattenCase,
                                          Preprocessing.RemoveAllWhiteSpace = FuzzyStringMatching.C$Preprocessing.RemoveAllWhiteSpace,
                                          Preprocessing.SquishWhiteSpace = FuzzyStringMatching.C$Preprocessing.SquishWhiteSpace,
                                          StringdistArguments = list(useBytes = FuzzyStringMatching.C$Stringdist.useBytes,
                                                                     weight = c(d = as.numeric(FuzzyStringMatching.C$Stringdist.weight.d),
                                                                                i = as.numeric(FuzzyStringMatching.C$Stringdist.weight.i),
                                                                                s = as.numeric(FuzzyStringMatching.C$Stringdist.weight.s),
                                                                                t = as.numeric(FuzzyStringMatching.C$Stringdist.weight.t)),
                                                                     q = as.numeric(FuzzyStringMatching.C$Stringdist.q),
                                                                     p = as.numeric(FuzzyStringMatching.C$Stringdist.p),
                                                                     bt = as.numeric(FuzzyStringMatching.C$Stringdist.bt)))
      }

      return(Vector)
  }

#-------------------------------------------------------------------------------

  RunDictionary <- function(Vector,
                            Dictionary.C = Dictionary)
  {
      if (length(Dictionary.C) > 0)
      {
          Vector <- if_else(is.na(Dictionary.C[Vector]),
                            Vector,
                            Dictionary.C[Vector])
      }

      return(Vector)
  }

#===============================================================================

  # Get selection and order of harmonization methods to be applied on 'Feature'
  Process <- as.data.frame(as.list(Methods)) %>%
                  slice_head() %>%      # Make sure 'Methods' contains only one row for current feature
                  select(starts_with("Method.")) %>%
                  pivot_longer(everything(), names_to = "Method", values_to = "Order") %>%
                  filter(!is.na(Order)) %>%
                  arrange(Order) %>%
                  mutate(Function = case_match(Method,
                                               "Method.TransformativeExpressions" ~ "RunTransformativeExpressions",
                                               "Method.FuzzyStringMatching" ~ "RunFuzzyStringMatching",
                                               "Method.Dictionary" ~ "RunDictionary"))

  # Apply selection of harmonization methods on 'Feature' as defined in 'Process'
  if (length(Process) > 0 && nrow(Process) > 0)
  {
      for (Function in Process$Function)
      {
          Feature <- do.call(Function, list("Vector" = Feature))
      }
  }

#===============================================================================
  return(Feature)
}
