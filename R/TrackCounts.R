
#' TrackCounts
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' Assess changes in table record and root subject counts
#'
#' @param DataSet.Prior \code{list} - The data set as it was before a transforming procedure
#' @param TransformationReturn \code{list} - The returned list from a transforming procedure, including the updated data set as well as sets of non-conforming records
#' @param RootSubjectKeys \code{list} - Containing the feature names for each data set table that function key identifying root subjects
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
                        PrintMessages = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # DataSet.Prior <- DataSet[RootTableNames[RootTableNames != SeedTableName]]
  # TransformationReturn <- RecordSubsumption.Root
  # PrintMessages <- TRUE

  # --- Argument Validation ---
  assert_that(is.list(DataSet.Prior),
              is.list(TransformationReturn),
              is.flag(PrintMessages))

  # Make sure two lists have exactly the same element names (aligned in order)
  stopifnot("ERROR: Names of 'DataSet.Prior' and 'TransformationReturn' must be exactly aligned." = (all(names(DataSet.Prior) == names(TransformationReturn))))

#-------------------------------------------------------------------------------

  Tracker <- pmap(.l = list(DataSet.Prior,
                            TransformationReturn,
                            names(DataSet.Prior)),
                  .f = function(PriorTable, TableTransformationReturn, tablename)
                       {
                          Counts.Prior <- PriorTable %>%
                                              summarize(CountRecords.Prior = n(),
                                                        CountRootSubjects.Prior = n_distinct(pick(RootSubjectKeys[[tablename]])))

                          Counts.Post <- TableTransformationReturn$Table %>%
                                              summarize(CountRecords.Post = n(),
                                                        CountRootSubjects.Post = n_distinct(pick(RootSubjectKeys[[tablename]])))

                          Counts.Nonconforming <- tibble(CountRecords.Nonconforming = 0,
                                                         CountRootSubjects.Affected = 0)

                          if (!is.null(TableTransformationReturn$NonconformingRecords))
                          {
                              Counts.Nonconforming <- TableTransformationReturn$NonconformingRecords %>%
                                                          summarize(CountRecords.Nonconforming = n(),
                                                                    CountRootSubjects.Affected = n_distinct(pick(RootSubjectKeys[[tablename]])))
                          }

                          bind_cols(Counts.Prior,
                                    Counts.Post,
                                    Counts.Nonconforming) %>%
                              mutate(Change.CountRecords = CountRecords.Post - CountRecords.Prior,
                                     Change.CountRootSubjects = CountRootSubjects.Post - CountRootSubjects.Prior,
                                     Message = paste0("'", tablename, "': ",
                                                      case_when(Change.CountRecords > 0 ~ "Added ",
                                                                .default = "Removed "),
                                                      case_when(Change.CountRecords == 0 ~ "no",
                                                                .default = as.character(abs(Change.CountRecords))),
                                                      " records, affecting ",
                                                      case_when(CountRootSubjects.Affected == 0 ~ "no",
                                                                .default = as.character(CountRootSubjects.Affected)),
                                                      " root subjects."),
                                     MessageClass = case_when(Change.CountRecords == 0 ~ "Info",
                                                              .default = "Success"),
                                     ConsistencyCheck = case_when(Change.CountRecords <= 0 ~ CountRecords.Nonconforming == abs(Change.CountRecords),      # Checking if the number of non-conforming records is equal to the change in record count post transformation
                                                                  .default = NA),
                                     Timestamp = Sys.time())

                        }) %>%
                  set_names(names(DataSet.Prior)) %>%
                  list_rbind(names_to = "Table")

  # Print messages
  if (PrintMessages == TRUE)
  {
      PrintMessages(Tracker %>%
                        select(MessageClass,
                               Message) %>%
                        tibble::deframe())
  }

#-------------------------------------------------------------------------------
  return(Tracker)
}
