
#' GetSampleStatisticsDS
#'
#' Calculate common sample statistics for a feature of a data.frame.
#'
#' @param TableName.S \code{string} - Name of the table that contains the feature
#' @param FeatureName.S \code{string} - Name of feature
#' @param GroupingFeatureName.S \code{string} - Name of optional grouping feature
#' @param RemoveNA.S \code{logical} - Whether NA values should be removed before calculating statistics
#' @param ReturnECDF.S \code{logical} - Whether an empirical cumulative distribution function should be returned additionally to sample statistics
#'
#' @return A \code{tibble} containing parametric and non-parametric sample statistics
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetSampleStatisticsDS <- function(TableName.S,
                                  FeatureName.S,
                                  GroupingFeatureName.S = NULL,
                                  RemoveNA.S = TRUE,
                                  ReturnECDF.S = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- ADS$Diagnosis
  # FeatureName.S <- "PatientAgeAtDiagnosis"
  # GroupingFeatureName.S <- "TNM.M"
  # RemoveNA.S <- TRUE
  # ReturnECDF.S <- TRUE

  # --- Argument Validation ---
  assert_that(is.string(TableName.S),
              is.string(FeatureName.S),
              is.flag(RemoveNA.S),
              is.flag(ReturnECDF.S))
  if (!is.null(GroupingFeatureName.S)) { assert_that(is.string(GroupingFeatureName.S))
                                         assert_that(GroupingFeatureName.S != FeatureName.S,
                                                     msg = "Values for 'GroupingFeatureName.S' and 'FeatureName.S' can not be identical.") }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

  # Stop if 'FeatureName.S' is not part of 'Table' or if does not have a suitable class
  assert_that(FeatureName.S %in% names(Table),
              msg = paste0("'", FeatureName.S, "' is not a valid feature name in '", TableName.S, "'."))
  assert_that(class(Table[[FeatureName.S]]) %in% c("numeric", "integer", "double"),
              msg = paste0("The specified feature '", FeatureName.S, "' is of class '", class(Table[[FeatureName.S]]), "' and therefore not suitable."))

  # Perform corresponding validation for 'GroupingFeatureName.S'
  if (!is.null(GroupingFeatureName.S))
  {
      assert_that(GroupingFeatureName.S %in% names(Table),
                  msg = paste0("'", GroupingFeatureName.S, "' is not a valid feature name in '", TableName.S, "'."))
      assert_that(class(Table[[GroupingFeatureName.S]]) %in% c("character", "logical", "factor"),
                  msg = paste0("The specified grouping feature '", GroupingFeatureName.S, "' is of class '", class(Table[[GroupingFeatureName.S]]), "' and therefore not suitable."))
  }

#-------------------------------------------------------------------------------

  # Initiate output object
  Statistics <- NULL

  # Get count of non-missing values in Feature
  N.Valid <- sum(!is.na(Table[[FeatureName.S]]))

  if (N.Valid > 0)
  {
      # Calculate parametric and non-parametric sample statistics
      Statistics <- as_tibble(Table) %>%
                        { if (RemoveNA.S == TRUE) { filter(., !is.na({{ FeatureName.S }})) } else {.} } %>%
                        summarize(N = n(),
                                  q5 = stats::quantile(.data[[FeatureName.S]], probs = 0.05, na.rm = RemoveNA.S),
                                  Q1 = stats::quantile(.data[[FeatureName.S]], probs = 0.25, na.rm = RemoveNA.S),
                                  Median = stats::median(.data[[FeatureName.S]], na.rm = RemoveNA.S),
                                  Q3 = stats::quantile(.data[[FeatureName.S]], probs = 0.75, na.rm = RemoveNA.S),
                                  q95 = stats::quantile(.data[[FeatureName.S]], probs = 0.95, na.rm = RemoveNA.S),
                                  MAD = stats::mad(.data[[FeatureName.S]], na.rm = RemoveNA.S),
                                  Mean = mean(.data[[FeatureName.S]], na.rm = RemoveNA.S),
                                  SD = stats::sd(.data[[FeatureName.S]], na.rm = RemoveNA.S),
                                  SEM = SD / sqrt(N)) %>%
                        mutate(Mean = case_when(is.nan(Mean) ~ NA,
                                                .default = Mean)) %>%
                        ungroup()

      # If GroupingFeatureName.S is passed...
      if (!is.null(GroupingFeatureName.S))
      {
          # Get group-specific output
          Groupwise <- as_tibble(Table) %>%
                            { if (RemoveNA.S == TRUE) { filter(., !is.na({{ FeatureName.S }})) } else {.} } %>%
                            group_by(., !!sym(GroupingFeatureName.S)) %>%
                            summarize(N = n(),
                                      q5 = stats::quantile(.data[[FeatureName.S]], probs = 0.05, na.rm = RemoveNA.S),
                                      Q1 = stats::quantile(.data[[FeatureName.S]], probs = 0.25, na.rm = RemoveNA.S),
                                      Median = stats::median(.data[[FeatureName.S]], na.rm = RemoveNA.S),
                                      Q3 = stats::quantile(.data[[FeatureName.S]], probs = 0.75, na.rm = RemoveNA.S),
                                      q95 = stats::quantile(.data[[FeatureName.S]], probs = 0.95, na.rm = RemoveNA.S),
                                      MAD = stats::mad(.data[[FeatureName.S]], na.rm = RemoveNA.S),
                                      Mean = mean(.data[[FeatureName.S]], na.rm = RemoveNA.S),
                                      SD = stats::sd(.data[[FeatureName.S]], na.rm = RemoveNA.S),
                                      SEM = SD / sqrt(N)) %>%
                            mutate(Mean = case_when(is.nan(Mean) ~ NA,
                                                .default = Mean)) %>%
                            ungroup()

          # Create Extra column for later rbinding
          Statistics <- Statistics %>%
                            mutate(dummy = "AcrossGroups", .before = 1)

          # Harmonize column names for rbinding
          colnames(Statistics) <- colnames(Groupwise)

          Statistics <- rbind(Statistics,
                              Groupwise)
      }
  }

#-------------------------------------------------------------------------------

  ECDF <- NULL

  # if (ReturnECDF.S == TRUE)
  # {
  #       ECDF <- Table %>%
  #                 { if (RemoveNA.S == TRUE) { filter(., !is.na({{ FeatureName.S }})) } else {.} } %>%
  #                 { if (!is.null(GroupingFeatureName.S)) { group_by(., !!sym(GroupingFeatureName.S)) } else {.} } %>%
  #                 summarize(N = n(),
  #                           ECDF = list(ecdf(.data[[FeatureName.S]])))
  # }

#-------------------------------------------------------------------------------
  return(list(Statistics = Statistics,
              ECDF = ECDF))
}


