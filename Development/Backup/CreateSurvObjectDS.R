
#' CreateSurvObjectDS
#'
#'  in the \code{survival} package
#'
#' @param TableName.S \code{string} - Name of the data frame that holds time and event features
#' @param TimeFeature.S \code{string} - Name of time feature
#' @param EventFeature.S \code{string} - Name of event feature
#' @param MinFollowUpTime.S \code{integer} - Optional minimum of observed follow up time
#'
#' @return A \code{survival::Surv()} object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CreateSurvObjectDS <- function(TableName.S,
                               TimeFeature.S,
                               EventFeature.S,
                               MinFollowUpTime.S = 1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- ADS$Patients
  # TimeFeature.S <- "TimeFollowUp"
  # EventFeature.S <- "IsDocumentedDeceased"
  # MinFollowUpTime <- 10

  # --- Argument Validation ---
  assert_that(is.string(TableName.S),
              is.string(TimeFeature.S),
              is.string(EventFeature.S),
              is.number(MinFollowUpTime.S))

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
                     {{ EventFeature.S }}) %>%
              rename(Time = {{ TimeFeature.S }},
                     Event = {{ EventFeature.S }})

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
  # return(list(SurvObject = SurvObject,
  #             Model = Model))
  return(SurvObject)
}


