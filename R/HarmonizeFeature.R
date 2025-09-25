
#' HarmonizeFeature
#'
#' This function performs harmonizing data transformation on an isolated feature based on passed settings.
#' Harmonization is attempted in a step-wise approach, incorporating various methods in a defined order:
#' \enumerate{\item Transformative expressions (e.g. functions like \code{str_to_upper()})
#'            \item Dictionary look-up
#'            \item Fuzzy String Matching}
#'
#' @param Feature \code{vector} containing data to be transformed
#' @param EligibleValueSet \code{character} vector containing set of eligible feature values
#' @param Methods \code{list} - Contains data on the selection of methods to use for each feature
#' @param TransformativeExpressions \code{data.frame} - Contains set of expressions like functions used to transform data values
#' @param Dictionary \code{character} - Contains dictionary data used to look up and replace data values
#' @param FuzzyStringMatching \code{list} - Contains settings for Fuzzy String Matching
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
                             Dictionary = NULL,
                             FuzzyStringMatching = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)
  require(tidyr)
  require(purrr)

  # --- For Testing Purposes ---
  # Feature <- RawDataSet$RDS.Staging$uicc_stadium
  # tablename <- "Staging"
  # featurename <- "UICCStage"
  # EligibleValueSet <- dsCCPhos::Meta.Values %>%
  #                           filter(Table == tablename,
  #                                  Feature == featurename) %>%
  #                           pull(Value.Curated)
  # Methods <- as.list(dsCCPhos::Set.DataHarmonization %>% filter(Table == tablename, Feature == featurename))
  # TransformativeExpressions = dsCCPhos::Set.TransformativeExpressions %>% filter(Table == tablename, Feature == featurename)
  # Dictionary <- dsCCPhos::Set.Dictionary %>% filter(Table == tablename, Feature == featurename)
  # FuzzyStringMatching <- as.list(dsCCPhos::Set.FuzzyStringMatching %>% filter(Table == tablename, Feature == featurename))

  # --- Argument Assertions ---
  assert_that(is.vector(Vector),
              is.list(Methods))
  if (!is.null(EligibleValueSet)) { assert_that(is.character(EligibleValueSet)) }
  if (!is.null(TransformativeExpressions)) { assert_that(is.data.frame(TransformativeExpressions)) }
  if (!is.null(Dictionary)) { assert_that(is.character(Dictionary)) }
  if (!is.null(FuzzyStringMatching)) { assert_that(is.list(FuzzyStringMatching)) }

#===============================================================================

# First, define functions that apply each method on 'Vector'
#-----------------------------------------------------------
#   - RunTransformativeExpressions()
#   - RunDictionary()
#   - RunFuzzyStringMatching()

#-------------------------------------------------------------------------------

  RunTransformativeExpressions <- function(Vector.C,
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
              if (length(ContextDataFrame) > 0)
              {
                  Vector.C <- DataFrame %>%
                                  eval(expr = parse(text = paste0("mutate(., ",
                                                                  FeatureName,
                                                                  " = ",
                                                                  str_replace(expression, ".X", FeatureName),
                                                                  ")"))) %>%
                                  pull({{ FeatureName }})
              } else {

                  Vector.C <- eval(expr = parse(text = str_replace(expression, ".X", "Vector.C")))
              }
          }
      }

      return(Vector.C)
  }

#-------------------------------------------------------------------------------

  RunDictionary <- function(Vector.C,
                            Dictionary.C = Dictionary)
  {
      if (length(Dictionary.C) > 0)
      {
          Vector.C <- Dictionary.C[Vector.C]
      }

      return(Vector.C)
  }

#-------------------------------------------------------------------------------

  RunFuzzyStringMatching <- function(Vector.C,
                                     EligibleValueSet.C = EligibleValueSet,
                                     FuzzyStringMatching.C = FuzzyStringMatching)
  {
      GetFuzzyStringMatches(Vector = Vector.C,
                            EligibleStrings = EligibleValueSet.C,
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

#===============================================================================

  # Get order of harmonization methods to be applied on 'Vector'
  Process <- as.data.frame(as.list(Methods)) %>%
                  slice_head() %>%      # Make sure 'Methods' contains only one row for current feature
                  select(starts_with("Method.")) %>%
                  pivot_longer(everything(), names_to = "Method", values_to = "Order") %>%
                  filter(!is.na(Order)) %>%
                  arrange(Order) %>%
                  mutate(Function = case_match(Method,
                                               "Method.TransformativeExpressions" ~ "RunTransformativeExpressions",
                                               "Method.Dictionary" ~ "RunDictionary",
                                               "Method.FuzzyStringMatching" ~ "RunFuzzyStringMatching"))

  # Apply each method on 'Vector' in the order passed by settings
  if (length(Process) > 0 && nrow(Process) > 0)
  {
      walk(Process$Function,
           function(Function)
           {
              Feature <- do.call(Function, list(Vector.C = Feature))
           })
  }

#===============================================================================
  return(Vector)
}
