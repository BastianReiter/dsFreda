
#' SubsumeRecords
#'
#' Auxiliary function within \code{\link{CurateDataDS}}
#'
#' Identify and remove any records that contain less essential information compared to a previous record.
#'
#' @param Table \code{data.frame} or \code{tibble}
#' @param PrimaryKey \code{character vector} - Name of feature(s) that serve(s) as table's primary key
#' @param DistinctiveFeatures \code{character vector} - Names of features that are used to strictly distinguish different table records (used in \code{group_by}-statement)
#' @param NegligibleFeatures \code{character vector} - Names of features that are considered negligible in the informational value of a table record
#'
#' @return \code{data.frame} or \code{tibble} cleaned of redundant records
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SubsumeRecords <- function(Table,
                           PrimaryKey,
                           DistinctiveFeatures,
                           NegligibleFeatures = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSet$Staging
  # PrimaryKey <- c("StagingID")
  # DistinctiveFeatures <- c("PatientID", "DiagnosisID", "StagingDate")
  # NegligibleFeatures <- "StagingID"

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.character(PrimaryKey),
              is.character(DistinctiveFeatures),
              is.character(NegligibleFeatures))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Create auxiliary ID feature to keep track of table records throughout function
  if (!(".AuxID" %in% names(Table)))
  {
      Table <- Table %>% ungroup() %>% mutate(.AuxID = row_number())

  } else {    # In the (unlikely) case of preexistence of a feature named '.AuxID' stop and print error message

      stop("ERROR: The passed table contains a feature called '.AuxID' which interferes with function protocol. Please rename this feature and try again.", call. = FALSE)
  }

  # Define 'EssentialFeatures' as names of table features that are not negligible
  EssentialFeatures <- names(Table)[!(names(Table) %in% NegligibleFeatures)]

  # Reorder feature names in 'DistinctiveFeatures' according to correct stratification, defined by column order in 'Table'
  DistinctiveFeatures <- names(Table)[names(Table) %in% DistinctiveFeatures]

  # Reduce table to subset of records that are potentially redundant to decrease computational workload
  PotentialRedundancies <- Table %>%
                              group_by(across(all_of(DistinctiveFeatures))) %>%      # Group by features defined in 'DistinctiveFeatures' to perform sensible initial stratification
                                  summarize(RecordCount = n()) %>%
                              filter(RecordCount > 1) %>%      # Filter out records that have no potential redundancies
                              select(-RecordCount) %>%
                              ungroup() %>%
                              left_join(Table) %>%      # Per default the join operation is performed on all common features
                              suppressMessages()

  # Initially all records in 'PotentialRedundancies' need to be screened
  IDsToCheck <- PotentialRedundancies %>% pull(.AuxID)

  # Data frame that holds pairs of AuxIDs of redundant records and respective reference records
  RedundancyPairs <- NULL

  # Vector that holds the persisting (so non-redundant) record AuxIDs throughout function
  FinalPersistentIDs <- NULL


  while (length(IDsToCheck) > 0)
  {
      # Subsetting of records to be investigated in current loop instance
      CurrentRecords <- PotentialRedundancies %>%
                            filter(.AuxID %in% IDsToCheck) %>%
                            mutate(CountRelevantNAs = rowSums(is.na(across(all_of(EssentialFeatures))))) %>%
                            group_by(across(all_of(DistinctiveFeatures))) %>%
                                arrange(CountRelevantNAs, .by_group = TRUE) %>%      # Put record with least missing values on first position ('head')
                                mutate(.AuxReferenceID = .AuxID[row_number() == 1])

      # Pull out and temporarily save IDs of all current 'reference records' which are the ones that have the least missing values
      CurrentReferenceIDs <- CurrentRecords %>%
                                  #--- <Still grouped> ---
                                      slice_head() %>%
                                  ungroup() %>%
                                  pull(.AuxID)

      # Obtain IDs (based on introduced '.AuxID') of persisting table records
      # Using combination of fill() and distinct() has the effect that all records that have less or equal informational value compared to 'reference record' are considered redundant and therefore removed by 'distinct' command
      CurrentPersistentIDs <- CurrentRecords %>%
                                  #--- <Still grouped> ---
                                      fill(all_of(EssentialFeatures), .direction = "down") %>%
                                      distinct(across(all_of(EssentialFeatures)), .keep_all = TRUE) %>%
                                  ungroup() %>%
                                  pull(.AuxID)

      # Obtain pairs of removed AuxIDs and respective AuxReferenceIDs
      CurrentRemovedIDs <- CurrentRecords %>%
                                ungroup() %>%
                                filter(!(.AuxID %in% CurrentPersistentIDs)) %>%
                                select(.AuxID, .AuxReferenceID)

      # New vector of IDs to be checked
      IDsToCheck <- CurrentPersistentIDs %>% base::setdiff(CurrentReferenceIDs)

      # Update 'RedundancyPairs'
      RedundancyPairs <- rbind(RedundancyPairs, CurrentRemovedIDs)

      # Update 'FinalPersistentIDs'
      FinalPersistentIDs <- c(FinalPersistentIDs, CurrentReferenceIDs)
  }


  # Continue with finalization only if 'RedundancyPairs' is not empty, e.g. if any redundancies were found
  if (length(RedundancyPairs) > 0 && nrow(RedundancyPairs) > 0)
  {
      # Lookup table for AuxIDs and corresponding table-specific IDs (needed only for redundancy pairs)
      AuxIDLookup <- Table %>%
                        select(.AuxID, all_of(PrimaryKey)) %>%
                        rename_with(~ paste0(".Reference.", PrimaryKey), all_of(PrimaryKey))

      # Modify 'RedundancyPairs' before linking it with 'Table'
      RedundancyPairs <- RedundancyPairs %>%
                              mutate(.IsRedundant = TRUE) %>%      # This will mark all records in 'Table' that are considered redundant
                              select(.AuxID, .IsRedundant, .AuxReferenceID)

      # Mark redundant records in original table preserving info about their referenced records
      Table <- Table %>%
                  left_join(RedundancyPairs, by = join_by(.AuxID)) %>%      # This adds column '.AuxReferenceID' to Table, containing AuxIDs of referenced records for redundant records
                  mutate(.IsRedundant = coalesce(.IsRedundant, FALSE)) %>%      # Replaces all missing values in '.IsRedundant' with 'FALSE'
                  left_join(AuxIDLookup, by = join_by(.AuxReferenceID == .AuxID)) %>%      # Turn AuxIDs into original primary keys using previously created 'AuxIDLookup'
                  arrange(.AuxID) %>%       # Reestablish original table order
                  select(-.AuxID,
                         -.AuxReferenceID)      # Auxiliary IDs not needed anymore
  } else {

      Table <- Table %>%
                  mutate(.IsRedundant = FALSE,
                         across(all_of(PrimaryKey), ~ NA, .names = ".Reference.{.col}")) %>%
                  select(-.AuxID)
  }


#-------------------------------------------------------------------------------
  return(Table)
}
