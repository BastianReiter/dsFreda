
#' GetTTEModelDS
#'
#' Return Time-to-Event model implemented in the \code{survival} package
#'
#' @param TableName.S \code{string} - Name of the data frame that holds time and event features
#' @param TimeFeature.S \code{string} - Name of time feature
#' @param EventFeature.S \code{string} - Name of event feature
#' @param ModelType.S \code{string} - Function name of different TTE models implemented in \code{survival} package:
#'                                    \itemize{\item 'survfit'
#'                                             \item 'survdiff'
#'                                             \item 'coxph'}
#' @param CovariateA.S \code{string} - Name of optional Covariate A
#' @param CovariateB.S \code{string} - Name of optional Covariate B
#' @param CovariateC.S \code{string} - Name of optional Covariate C
#' @param MinFollowUpTime.S \code{integer} - Optional minimum of observed follow up time
#'
#' @return A Time-to-Event model object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetTTEModelDS <- function(TableName.S,
                          TimeFeature.S,
                          EventFeature.S,
                          ModelType.S = "survfit",
                          CovariateA.S = NULL,
                          CovariateB.S = NULL,
                          CovariateC.S = NULL,
                          MinFollowUpTime.S = 1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- Analysis
  # TimeFeature.S <- "TimeFollowUp"
  # EventFeature.S <- "IsDocumentedDeceased"
  # ModelType.S <- "survfit"
  # CovariateA.S <- "UICCStageCategory"
  # CovariateB.S <- "Sex"
  # CovariateC.S <- NULL
  # MinFollowUpTime.S <- 10

  # --- Argument Validation ---
  assert_that(is.string(TableName.S),
              is.string(TimeFeature.S),
              is.string(EventFeature.S),
              is.string(ModelType.S),
              is.number(MinFollowUpTime.S))
  if (!is.null(CovariateA.S)) { assert_that(is.string(CovariateA.S)) }
  if (!is.null(CovariateB.S)) { assert_that(is.string(CovariateB.S)) }
  if (!is.null(CovariateC.S)) { assert_that(is.string(CovariateC.S)) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  Table <- eval(parse(text = TableName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  # Initiate Messaging object
  Messages <- list()

#-------------------------------------------------------------------------------
# Preparing data used for model fit
#-------------------------------------------------------------------------------

  # Construct data used for model fit
  Data <- Table %>%
              select({{ TimeFeature.S }},
                     {{ EventFeature.S }},
                     {{ CovariateA.S }},
                     {{ CovariateB.S }},
                     {{ CovariateC.S }}) %>%
              rename(Time = {{ TimeFeature.S }},
                     Event = {{ EventFeature.S }},
                     CovariateA = {{ CovariateA.S }},
                     CovariateB = {{ CovariateB.S }},
                     CovariateC = {{ CovariateC.S }})

  # How many rows are in the 'raw' data
  AvailableRows <- nrow(Data)

  # Filtering data
  Data <- Data %>%
              filter(!is.na(Time) & !is.na(Event) & Time > 0) %>%      # Filter out invalid data
              filter(Time >= MinFollowUpTime.S)      # Optionally filter for a minimum of observed follow up time

  # How many rows remain after filtering
  EligibleRows <- nrow(Data)

  # How many rows were dropped
  Messages$DroppedRows <- AvailableRows - EligibleRows


#-------------------------------------------------------------------------------
# Creating Surv object
#-------------------------------------------------------------------------------

  # Using survival::Surv() to create Surv object
  SurvObject <- with(Data, survival::Surv(time = Time,
                                          event = Event,
                                          type = "right"))

#-------------------------------------------------------------------------------
# Model Fit
#-------------------------------------------------------------------------------

  ModelFormulaString <- "SurvObject ~ 1"

  if ("CovariateA" %in% names(Data)) { ModelFormulaString <- "SurvObject ~ CovariateA" }
  if (all(c("CovariateA", "CovariateB") %in% names(Data))) { ModelFormulaString <- "SurvObject ~ CovariateA + CovariateB" }
  if (all(c("CovariateA", "CovariateB", "CovariateC") %in% names(Data))) { ModelFormulaString <- "SurvObject ~ CovariateA + CovariateB + CovariateC" }

  Model <- NULL
  if (ModelType.S == "survfit") { Model <- survival::survfit(formula(ModelFormulaString), data = Data) }
  if (ModelType.S == "survdiff") { Model <- survival::survdiff(formula(ModelFormulaString), data = Data) }
  if (ModelType.S == "coxph") { Model <- survival::coxph(formula(ModelFormulaString), data = Data) }

#-------------------------------------------------------------------------------
# Life table
#-------------------------------------------------------------------------------

  # SurvfitSummary <- summary(object = Model,
  #                           times = seq(from = floor(min(Model$time)),
  #                                       to = ceiling(max(Model$time)),
  #                                       by = 1))
  #                           data.frame = TRUE)

  # If at least one stratifying covariate is included in the model and therefore the model summary data.frame
  # if (!is.null(SurvfitSummary$strata))
  # {
  #     split_vector <- SurvfitSummary$strata
  #     df_list <- split(df, split_vector)
  #     names(df_list) <- levels(SurvfitSummary$strata)
  #
  # } else {
  #
  #     df_list <- list(df)
  #     names(df_list) <- "life.table_no_strata"
  # }


  # library(ggsurvfit)
  #
  # Plot <- Model %>%
  #             ggsurvfit() +
  #             xlim(0, 5 * 365) +
  #             labs(x = "Days",
  #                  y = "Overall survival probability") +
  #             add_confidence_interval()

#-------------------------------------------------------------------------------
  return(Model)
}


