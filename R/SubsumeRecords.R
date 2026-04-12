
#' SubsumeRecords
#'
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#'
#' Auxiliary function within \code{\link{CurateDataDS}}.
#' Carries out record subsumption procedures, mainly finding semantic record redundancies.
#'
#' @param Table \code{data.frame} containing data to be transformed
#' @param TableName \code{string} - The table's name, used for command line messaging
#' @param PrimaryKey \code{character vector} - Name of feature(s) that serve(s) as table's primary key
#' @param RootSubjectKey \code{character vector} - Names of features that identify root subjects in current table, functioning as a foreign key (usually primary key of data set root subjects)
#' @param DistinctiveFeatures \code{character vector} - Names of features that are used to strictly distinguish different table records (used in \code{group_by}-statement)
#' @param NegligibleFeatures \code{character vector} - Names of features that are considered negligible in the informational value of a table record
#' @param NegligibleValues \code{list} - Which feature values do not have informational value? List with element names being feature names and list elements being character vectors with negligible values of the feature.
#' @param PrintMessages \code{logical} - Whether to print report messages during function proceedings
#'
#' @return A \code{list} containing:
#'              \itemize{ \item Table (\code{data.frame})
#'                        \item Tracker (\code{data.frame})
#'                        \item Log (\code{data.frame}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SubsumeRecords <- function(Table,
                           TableName = NA_character_,
                           PrimaryKey,
                           RootSubjectKey,
                           SubsumptionRedundancies.Detect = TRUE,
                           SubsumptionRedundancies.Remove = TRUE,
                           SubsumptionRedundancies.DistinctiveFeatures,
                           SubsumptionRedundancies.NegligibleFeatures = NULL,
                           SubsumptionRedundancies.NegligibleValues = NULL,
                           PrintMessages = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSet$SystemicTherapy
  # TableName <- "SystemicTherapy"
  # PrimaryKey <- c("SystemicTherapyID")
  # RootSubjectKey <- c("PatientID", "DiagnosisID")
  # SubsumptionRedundancies.Detect <- TRUE
  # SubsumptionRedundancies.Remove <- TRUE
  # SubsumptionRedundancies.DistinctiveFeatures <- c("PatientID", "DiagnosisID", "SystemicTherapyStartDate", "SystemicTherapyEndDate")
  # SubsumptionRedundancies.NegligibleFeatures <- "SystemicTherapyID"
  # SubsumptionRedundancies.NegligibleValues <- Settings$DataHarmonization$Process %>% filter(Table == "SystemicTherapy") %>% select(Feature, UnharmonizedValues.Substitution) %>% tibble::deframe() %>% as.list()
  # PrintMessages <- TRUE

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.string(TableName),
              is.character(PrimaryKey),
              is.character(RootSubjectKey),
              is.flag(SubsumptionRedundancies.Detect),
              is.flag(SubsumptionRedundancies.Remove),
              is.character(SubsumptionRedundancies.DistinctiveFeatures),
              is.character(SubsumptionRedundancies.NegligibleFeatures),
              is.list(SubsumptionRedundancies.NegligibleValues),
              is.flag(PrintMessages))
  stopifnot("ERROR: 'PrimaryKey' must contain column names of 'Table'." = (PrimaryKey %in% names(Table)))
  if (length(RootSubjectKey) > 0) { assert_that(is.character(RootSubjectKey))
                                    stopifnot("ERROR: 'RootSubjectKey' must contain column names of 'Table'." = (all(RootSubjectKey %in% names(Table)))) }

#-------------------------------------------------------------------------------

  Detector.SubsumptionRedundancies <- NULL
  Tracker.SubsumptionRedundancies <- NULL

  Log.SubsumptionRedundancies <- Log.New(Table = TableName,
                                         ProcessTopic = "Record subsumption",
                                         ProcessExecution = "Omitted",
                                         Message = "Omitted detection and removal.",
                                         MessageClass = "Info")

  if (SubsumptionRedundancies.Detect == TRUE)
  {
      # Find and mark table records considered redundant by subsumption using dsFreda::FindSubsumptionRedundancies()
      Table <- Table %>%
                  dsFreda::FindSubsumptionRedundancies(PrimaryKey = PrimaryKey,
                                                       DistinctiveFeatures = SubsumptionRedundancies.DistinctiveFeatures,
                                                       NegligibleFeatures = SubsumptionRedundancies.NegligibleFeatures,
                                                       NegligibleValues = SubsumptionRedundancies.NegligibleValues)

      # Create DETECTOR containing all records considered redundant by subsumption
      Detector.SubsumptionRedundancies <- Table %>%
                                              filter(.IsRedundant == TRUE) %>%
                                              mutate(.Nonconformance = "Redundant by subsumption",
                                                     .HasBeenRemoved = FALSE)

      # Create TRACKER entry from Detector
      Tracker.SubsumptionRedundancies <- Detector.SubsumptionRedundancies %>%
                                              summarize(Table = TableName,
                                                        ProcessTopic = "Record subsumption",
                                                        CountLevel = "Topic",
                                                        CountRecords.Detected = n(),
                                                        CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                        Message = paste0("Detected ", CountRecords.Detected, " records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                                        MessageClass = "Info") %>%
                                              Tracker.Make()

      # EXECUTE REMOVAL of records considered redundant by subsumption (also remove informative columns about redundancy)
      if (SubsumptionRedundancies.Remove == TRUE && nrow(Detector.SubsumptionRedundancies) > 0)
      {
          Table <- Table %>%
                      filter(!(.IsRedundant == TRUE)) %>%
                      select(-.IsRedundant,
                             -starts_with(".Reference."))

          # Mark records in DETECTOR as removed
          Detector.SubsumptionRedundancies <- Detector.SubsumptionRedundancies %>%
                                                  mutate(.HasBeenRemoved = TRUE)

          # Modify TRACKER after executed removal of subsumption redundancies
          Tracker.SubsumptionRedundancies <- Tracker.SubsumptionRedundancies %>%
                                                  mutate(CountRecords.Removed = CountRecords.Detected,
                                                         Message = paste0("Removed ", CountRecords.Removed, " records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                                         MessageClass = "Success",
                                                         Timestamp = Sys.time())
      }

      # Create LOG entry
      Log.SubsumptionRedundancies <- Tracker.SubsumptionRedundancies %>%
                                          Log.Make() %>%
                                          mutate(ProcessExecution = "Executed")
  }

  # Print log report message
  if (PrintMessages == TRUE) { Log.Print(Log.SubsumptionRedundancies) }

#-------------------------------------------------------------------------------
  return(list(Table = Table,
              NonconformingRecords = Detector.SubsumptionRedundancies,
              Tracker = Tracker.SubsumptionRedundancies,
              Log = Log.SubsumptionRedundancies))
}
