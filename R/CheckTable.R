
#' CheckTable
#'
#' Checks out a \code{data.frame} and returns an informative \code{list} object.
#'
#' @param Table \code{data.frame} or \code{tibble}
#' @param RequiredFeatureNames \code{character} - Optional names of required features - Default: \code{names(Table)}
#' @param EligibleValueSets \code{list} of character vectors containing sets of eligible values for corresponding feature
#'
#' @return A \code{list} containing informative meta data about a \code{data.frame}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CheckTable <- function(Table = NULL,
                       RequiredFeatureNames = NULL,
                       EligibleValueSets = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSet$RDS.Case
  # RequiredFeatureNames <- RequiredFeatureNames$Case
  # EligibleValueSets <- EligibleValueSets$Case

  # --- Argument Validation ---
  if (!is.null(Table)) { assert_that(is.data.frame(Table)) }
  if (!is.null(RequiredFeatureNames)) { assert_that(is.character(RequiredFeatureNames)) }
  if (!is.null(EligibleValueSets)) { assert_that(is.list(EligibleValueSets)) }

#-------------------------------------------------------------------------------

  # If no 'RequiredFeatureNames' are passed, assign feature names of Table per default
  if (is.null(RequiredFeatureNames)) { RequiredFeatureNames <- names(Table) }

  # Initiate output object
  TableCheck <- NULL

  # Create template-only, if Table is NULL or data.frame(0)
  if (length(Table) == 0)
  {
      TableCheck <- list(TableExists = FALSE,
                         TableComplete = FALSE,
                         FeatureCheckOverview = tibble(Feature = RequiredFeatureNames,
                                                       Exists = FALSE,
                                                       Type = NA,
                                                       NonMissingValueCount = NA,
                                                       NonMissingValueRate = NA,
                                                       EligibleValueCount = NA,
                                                       EligibleValueRate = NA),
                         MissingFeatures = RequiredFeatureNames,
                         RowCount = NA)

  } else {

      # Identify missing features
      PresentFeatureNames <- names(Table)
      MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]

      TableComplete <- (length(MissingFeatures) == 0)

      # Get table row count
      RowCount <- nrow(Table)

      # Get summarizing data.frame that contains info about existence of table features
      FeatureExistence <- tibble(Feature = RequiredFeatureNames) %>%
                               mutate(Exists = case_when(Feature %in% PresentFeatureNames ~ TRUE,
                                                         .default = FALSE))

      # Get types/classes of table features
      FeatureTypes <- Table %>%
                          summarize(across(everything(), ~ typeof(.x))) %>%
                          pivot_longer(cols = everything(),
                                       names_to = "Feature",
                                       values_to = "Type")

      # Get absolute count of non-missing values per table feature
      NonMissingValueCounts <- Table %>%
                                  summarize(across(everything(), ~ sum(!(is.na(.x) | (is.character(.x) & .x == ""))))) %>%
                                  pivot_longer(cols = everything(),
                                               names_to = "Feature",
                                               values_to = "NonMissingValueCount")

      # Get rate of non-missing values per table feature
      NonMissingValueRates <- Table %>%
                                  summarize(across(everything(), ~ sum(!(is.na(.x) | (is.character(.x) & .x == ""))) / n())) %>%
                                  pivot_longer(cols = everything(),
                                               names_to = "Feature",
                                               values_to = "NonMissingValueRate")

      # Get absolute count of eligible values per table feature
      EligibleValueCounts <- Table %>%
                                  summarize(across(everything(),
                                                   ~ ifelse(!is.null(EligibleValueSets[[cur_column()]]),      # If a set of eligible values is passed for current feature...
                                                            sum(.x %in% EligibleValueSets[[cur_column()]]),      # ... count all occurring values that are part of this set
                                                            NA))) %>%
                                  pivot_longer(cols = everything(),
                                               names_to = "Feature",
                                               values_to = "EligibleValueCount")

      # Get rate of eligible values per table feature
      EligibleValueRates <- Table %>%
                                summarize(across(everything(),
                                                 ~ ifelse(!is.null(EligibleValueSets[[cur_column()]]),      # If a set of eligible values is passed for current feature...
                                                          sum(.x %in% EligibleValueSets[[cur_column()]]) / n(),      # ... calculate rate of occurring eligible values
                                                          NA))) %>%
                                pivot_longer(cols = everything(),
                                             names_to = "Feature",
                                             values_to = "EligibleValueRate")

      # Consolidate feature meta data in one data.frame
      FeatureCheckOverview <- FeatureExistence %>%
                                  left_join(FeatureTypes, by = join_by(Feature)) %>%
                                  left_join(NonMissingValueCounts, by = join_by(Feature)) %>%
                                  left_join(NonMissingValueRates, by = join_by(Feature)) %>%
                                  left_join(EligibleValueCounts, by = join_by(Feature)) %>%
                                  left_join(EligibleValueRates, by = join_by(Feature))

      # List for return statement
      TableCheck <- list(TableExists = TRUE,
                         TableComplete = TableComplete,
                         FeatureCheckOverview = FeatureCheckOverview,
                         MissingFeatures = MissingFeatures,
                         RowCount = RowCount)
  }

#-------------------------------------------------------------------------------
  return(TableCheck)
}
