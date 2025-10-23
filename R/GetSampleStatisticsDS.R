
#' GetSampleStatisticsDS
#'
#' Calculate common sample statistics for a feature of a data.frame.
#'
#' @param TableName.S \code{string} - Name of the table that contains the feature
#' @param MetricFeatureName.S \code{string} - Name of feature
#' @param GroupingFeatureName.S \code{string} - Name of optional grouping feature
#' @param RemoveMissings.S \code{logical} - With numeric features: Whether NA values should be removed before calculating statistics
#'
#' @return A \code{tibble} containing parametric and non-parametric sample statistics
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetSampleStatisticsDS <- function(TableName.S,
                                  MetricFeatureName.S,
                                  GroupingFeatureName.S = NULL,
                                  RemoveMissings.S = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- ADS$Diagnosis
  # MetricFeatureName.S <- "NumberLymphnodesAffected"
  # GroupingFeatureName.S <- NULL
  # RemoveMissings.S = TRUE

  # --- Argument Validation ---
  assert_that(is.string(TableName.S),
              is.string(MetricFeatureName.S),
              is.flag(RemoveMissings.S))
  if (!is.null(GroupingFeatureName.S)) { assert_that(is.string(GroupingFeatureName.S)) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  # Evaluate feature in question
  Feature <- Table[[MetricFeatureName.S]]

  # Stop if Feature is not of class 'numeric'
  if (!(class(Feature) %in% c("double", "integer", "numeric"))) { stop(paste0("The specified feature '", MetricFeatureName.S, "' is of class '", class(Feature), "' and therefore not suitable."), call. = FALSE) }

  # Initiate output object
  Statistics <- NULL

  # Get count of valid (non-missing) values in Feature
  N.Valid <- sum(!is.na(Feature))

  if (N.Valid > 0)
  {
      # Calculate parametric and non-parametric sample statistics
      Statistics <- as_tibble(Table) %>%
                        summarize(N = N.Valid,
                                  q5 = stats::quantile(.data[[MetricFeatureName.S]], probs = 0.05, na.rm = RemoveMissings.S),
                                  Q1 = stats::quantile(.data[[MetricFeatureName.S]], probs = 0.25, na.rm = RemoveMissings.S),
                                  Median = stats::median(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                  Q3 = stats::quantile(.data[[MetricFeatureName.S]], probs = 0.75, na.rm = RemoveMissings.S),
                                  q95 = stats::quantile(.data[[MetricFeatureName.S]], probs = 0.95, na.rm = RemoveMissings.S),
                                  MAD = stats::mad(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                  Mean = mean(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                  SD = stats::sd(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                  SEM = SD / sqrt(N)) %>%
                        mutate(Mean = case_when(is.nan(Mean) ~ NA,
                                                .default = Mean))

      # If GroupingFeatureName.S is passed...
      if (!is.null(GroupingFeatureName.S))
      {
          # Get group-specific output
          Groupwise <- as_tibble(Table) %>%
                            group_by(., .data[[GroupingFeatureName.S]]) %>%
                            summarize(q5 = stats::quantile(.data[[MetricFeatureName.S]], probs = 0.05, na.rm = RemoveMissings.S),
                                      Q1 = stats::quantile(.data[[MetricFeatureName.S]], probs = 0.25, na.rm = RemoveMissings.S),
                                      Median = stats::median(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                      Q3 = stats::quantile(.data[[MetricFeatureName.S]], probs = 0.75, na.rm = RemoveMissings.S),
                                      q95 = stats::quantile(.data[[MetricFeatureName.S]], probs = 0.95, na.rm = RemoveMissings.S),
                                      MAD = stats::mad(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                      Mean = mean(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                      SD = stats::sd(.data[[MetricFeatureName.S]], na.rm = RemoveMissings.S),
                                      SEM = SD / sqrt(N))

          # Create Extra column for later rbinding
          Statistics <- Statistics %>%
                            mutate(dummy = "All", .before = 1)

          # Harmonize column names for rbinding
          colnames(Statistics) <- colnames(Groupwise)

          Statistics <- rbind(Statistics,
                              Groupwise)
      }
  }

#-------------------------------------------------------------------------------
  return(Statistics)
}


