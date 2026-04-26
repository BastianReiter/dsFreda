
#' TrackCounts
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' Assess changes in table record and root subject counts
#'
#' @param DataSet.Prior \code{list} - The data set as it was before a transforming procedure
#' @param TransformationReturn \code{list} - The returned list from a transforming procedure, including the updated data set as well as sets of non-conforming records
#' @param RootSubjectKeys \code{list} - Containing the feature names for each data set table that function as key identifying root subjects
#' @param SeedSubjectKey \code{string} - The name of the feature identifying seed subjects in a table
#' @param PrintMessages \code{logical} - Whether to print report messages after tracking procedure
#'
#' @return A \code{data.frame}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TrackCounts <- function(DataSet.Prior,
                        TransformationReturn,
                        RootSubjectKeys,
                        SeedSubjectKey,
                        PrintMessages = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataSet.Prior <- DataSet[RootTableNames[RootTableNames != SeedTableName]]
  # TransformationReturn <- RecordSubsumption.Root
  # RootSubjectKeys <- RootSubjectKeys
  # SeedSubjectKey <- "PatientID"
  # PrintMessages <- TRUE

  # --- Argument Validation ---
  assert_that(is.list(DataSet.Prior),
              is.list(TransformationReturn),
              is.list(RootSubjectKeys),
              is.string(SeedSubjectKey),
              is.flag(PrintMessages))

  # Make sure two lists have exactly the same element names (aligned in order)
  stopifnot("ERROR: Names of 'DataSet.Prior' and 'TransformationReturn' must be exactly aligned." = (all(names(DataSet.Prior) == names(TransformationReturn))))

#-------------------------------------------------------------------------------

  Counter <- pmap(.l = list(DataSet.Prior,
                            TransformationReturn,
                            names(DataSet.Prior)),
                  .f = function(PriorTable, TableTransformationReturn, tablename)
                       {
                          Counts.Prior <- PriorTable %>%
                                              summarize(CountRecords.Prior = n(),
                                                        CountRootSubjects.Prior = n_distinct(pick(RootSubjectKeys[[tablename]])),
                                                        CountSeedSubjects.Prior = n_distinct(pick(SeedSubjectKey)))

                          Counts.Post <- TableTransformationReturn$Table %>%
                                              summarize(CountRecords.Post = n(),
                                                        CountRootSubjects.Post = n_distinct(pick(RootSubjectKeys[[tablename]])),
                                                        CountSeedSubjects.Post = n_distinct(pick(SeedSubjectKey)))

                          Counts.Nonconforming <- tibble(CountRecords.Nonconforming = 0,
                                                         CountRootSubjects.Affected = 0,
                                                         CountSeedSubjects.Affected = 0)

                          if (!is.null(TableTransformationReturn$NonconformingRecords))
                          {
                              Counts.Nonconforming <- TableTransformationReturn$NonconformingRecords %>%
                                                          summarize(CountRecords.Nonconforming = n(),
                                                                    CountRootSubjects.Affected = n_distinct(pick(RootSubjectKeys[[tablename]])),
                                                                    CountSeedSubjects.Affected = n_distinct(pick(SeedSubjectKey)))
                          }

                          bind_cols(Counts.Prior,
                                    Counts.Post,
                                    Counts.Nonconforming) %>%
                              mutate(CountRecords.Change = CountRecords.Post - CountRecords.Prior,
                                     CountRootSubjects.Change = CountRootSubjects.Post - CountRootSubjects.Prior,
                                     CountSeedSubjects.Change = CountSeedSubjects.Post - CountSeedSubjects.Prior,
                                     Message = paste0("'", tablename, "': ",
                                                      case_when(CountRecords.Change > 0 ~ "Added ",
                                                                .default = "Removed "),
                                                      case_when(CountRecords.Change == 0 ~ "no",
                                                                .default = as.character(abs(CountRecords.Change))),
                                                      " records, affecting ",
                                                      case_when(CountRootSubjects.Affected == 0 ~ "no",
                                                                .default = as.character(CountRootSubjects.Affected)),
                                                      " <Root subjects> and ",
                                                      case_when(CountSeedSubjects.Affected == 0 ~ "no",
                                                                .default = as.character(CountSeedSubjects.Affected)),
                                                      " <Seed subjects>."),
                                     MessageClass = case_when(CountRecords.Change == 0 ~ "Info",
                                                              .default = "Success"),
                                     ConsistencyCheck = case_when(CountRecords.Change <= 0 ~ CountRecords.Nonconforming == abs(CountRecords.Change),      # Checking if the number of non-conforming records is equal to the change in record count post transformation
                                                                  .default = NA),
                                     Timestamp = Sys.time())

                        }) %>%
                  set_names(names(DataSet.Prior)) %>%
                  list_rbind(names_to = "Table")

  # Print messages
  if (PrintMessages == TRUE)
  {
      PrintMessages(Counter %>%
                        select(MessageClass,
                               Message) %>%
                        tibble::deframe())
  }

#-------------------------------------------------------------------------------
  return(Counter)
}
