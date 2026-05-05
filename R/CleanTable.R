
#' CleanTable
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Detect and/or remove invalid table records.
#'
#' @param Table \code{data.frame} or \code{tibble} - The table object to be cleaned
#' @param TableName \code{string} - The table's name, used for command line messaging
#' @param PrimaryKey \code{character vector} - Name of feature(s) that serve(s) as table's primary key
#' @param RootSubjectKey \code{character vector} - Names of features that identify Root subjects in current table, functioning as a foreign key (usually primary key of data set Root subjects)
#' @param SeedSubjectKey \code{string} - The name of the feature identifying Seed subjects in table
#' @param PrimaryKeyIgnoredInRedundancyCheck \code{logical} - Indicating whether primary key feature has no semantic meaning and can be ignored when determining redundancy - Default: \code{TRUE}
#' @param DataSetRoot \code{data.frame} (Optional) - Identifying all data set Root subjects (e.g. pairs of PatientIDs and DiagnosisIDs). The \code{data.frame}'s feature names must contain foreign key features in depending tables.
#' @param UnlinkedRecords.Detect \code{logical}
#' @param UnlinkedRecords.Remove \code{logical}
#' @param EmptyStrings.Detect \code{logical} - Whether empty strings ('') and strings containing only white space should be detected - Default: \code{TRUE}
#' @param EmptyStrings.Substitute \code{logical} - Whether empty strings ('') and strings containing only white space should be removed and replaced by \code{NA} - Default: \code{TRUE}
#' @param EmptyStrings.Substitution \code{string} - The string which should be used to substitute empty strings. If 'NA' is passed, the empty strings are removed and set \code{NA}. Default: 'NA'
#' @param DuplicateRecords.Detect \code{logical} - Whether duplicate records should be detected - Default: \code{TRUE}
#' @param DuplicateRecords.Remove \code{logical} - Whether duplicate records should be removed - Default: \code{TRUE}
#' @param FeatureRequirements \code{data.frame}
#' @param FeatureAvailabilityViolations.Detect \code{logical}
#' @param FeatureAvailabilityViolations.Remove \code{logical}
#' @param PrintMessages \code{logical} - Whether to print report messages during function proceedings
#'
#' @return A \code{list} containing
#'            \itemize{ \item Table (\code{data.frame})
#'                      \item NonconformingRecords (\code{data.frame})
#'                      \item Counter (\code{data.frame})
#'                      \item Log (\code{data.frame}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CleanTable <- function(Table,
                       TableName = NA_character_,
                       PrimaryKey,
                       RootSubjectKey,
                       SeedSubjectKey,
                       PrimaryKeyIgnoredInRedundancyCheck = TRUE,
                       DataSetRoot = NULL,
                       UnlinkedRecords.Detect = TRUE,
                       UnlinkedRecords.Remove = TRUE,
                       EmptyStrings.Detect = TRUE,
                       EmptyStrings.Substitute = TRUE,
                       EmptyStrings.Substitution = "NA",
                       DuplicateRecords.Detect = TRUE,
                       DuplicateRecords.Remove = TRUE,
                       FeatureRequirements = NULL,
                       FeatureAvailabilityViolations.Detect = TRUE,
                       FeatureAvailabilityViolations.Remove = TRUE,
                       PrintMessages = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSetRoot
  # TableName <- ".DataSetRoot"
  # tablename <- ".DataSetRoot"
  # PrimaryKey <- RootPrimaryKey
  # RootSubjectKey <- RootPrimaryKey
  # SeedSubjectKey <- SeedPrimaryKey
  # DataSetRoot <- NULL
  # FeatureRequirements = Settings$FeatureRequirements %>% filter(Table %in% RootTableNames)
  # #---
  # Table <- DataSet$Department
  # TableName <- "Department"
  # tablename <- "Department"
  # PrimaryKey <- PrimaryKeys[[tablename]]
  # RootSubjectKey <- RootSubjectKeys[[tablename]]
  # SeedSubjectKey <- "CaseID"
  # DataSetRoot <- DataSetRoot
  # FeatureRequirements <- Settings$FeatureRequirements %>% filter(Table == tablename)
  # #---
  # PrimaryKeyIgnoredInRedundancyCheck <- TRUE
  # UnlinkedRecords.Detect <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(UnlinkedRecords.Detect)
  # UnlinkedRecords.Remove <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(UnlinkedRecords.Remove)
  # EmptyStrings.Detect <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Detect)
  # EmptyStrings.Substitute <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Substitute)
  # EmptyStrings.Substitution <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Substitution)
  # DuplicateRecords.Detect <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(DuplicateRecords.Detect)
  # DuplicateRecords.Remove <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(DuplicateRecords.Remove)
  # FeatureAvailabilityViolations.Detect <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(FeatureAvailabilityViolations.Detect)
  # FeatureAvailabilityViolations.Remove <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(FeatureAvailabilityViolations.Remove)
  # PrintMessages <- TRUE

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.string(TableName),
              is.character(PrimaryKey),
              is.character(RootSubjectKey),
              is.character(SeedSubjectKey),
              is.flag(PrimaryKeyIgnoredInRedundancyCheck),
              is.flag(UnlinkedRecords.Detect),
              is.flag(UnlinkedRecords.Remove),
              is.flag(EmptyStrings.Detect),
              is.flag(EmptyStrings.Substitute),
              is.string(EmptyStrings.Substitution),
              is.flag(DuplicateRecords.Detect),
              is.flag(DuplicateRecords.Remove),
              is.flag(FeatureAvailabilityViolations.Detect),
              is.flag(FeatureAvailabilityViolations.Remove),
              is.flag(PrintMessages))
  stopifnot("ERROR: 'PrimaryKey' must contain column names of 'Table'." = (PrimaryKey %in% names(Table)))
  stopifnot("ERROR: 'RootSubjectKey' must contain column names of 'Table'!" = (all(RootSubjectKey %in% names(Table))))
  stopifnot("ERROR: 'SeedSubjectKey' must be a column name of 'Table'!" = (SeedSubjectKey %in% names(Table)))
  if (!is.null(DataSetRoot)) { assert_that(is.data.frame(DataSetRoot))
                               stopifnot("ERROR: 'DataSetRoot' must no be empty!" = (length(DataSetRoot) > 0 && nrow(DataSetRoot) > 0))
                               stopifnot("ERROR: 'RootSubjectKey' must contain column names of 'DataSetRoot'!" = (all(RootSubjectKey %in% names(DataSetRoot)))) }
  if (!is.null(FeatureRequirements)) { assert_that(is.data.frame(FeatureRequirements)) }

#-------------------------------------------------------------------------------

  # Ensure validity of settings regarding substitution of empty strings or removal of records
  if (UnlinkedRecords.Remove == TRUE) { UnlinkedRecords.Detect <- TRUE }
  if (EmptyStrings.Substitute == TRUE) { EmptyStrings.Detect <- TRUE }
  if (DuplicateRecords.Remove == TRUE) { DuplicateRecords.Detect <- TRUE }
  if (FeatureAvailabilityViolations.Remove == TRUE) { FeatureAvailabilityViolations.Detect <- TRUE }

  # Create auxiliary ID feature to ensure correct and unique identification of table records throughout function where necessary
  if (!(".AuxID" %in% names(Table)))
  {
      Table <- Table %>% ungroup() %>% mutate(.AuxID = row_number())

  } else {    # In the (unlikely) case of preexistence of a feature named '.AuxID' stop and print error message

      stop("ERROR: The passed table contains a feature called '.AuxID' which interferes with function protocol. Please rename this feature and try again.", call. = FALSE)
  }

#-------------------------------------------------------------------------------


# 1) DETECT and REMOVE records that are not linked with data set Root subjects via foreign key
#-------------------------------------------------------------------------------

  Detector.UnlinkedRecords <- NULL
  Counter.UnlinkedRecords <- NULL

  Log.UnlinkedRecords <- Log.New(Table = TableName,
                                 ProcessTopic = "Unlinked table records",
                                 ProcessExecution = "Omitted",
                                 Message = "Omitted detection and removal.",
                                 MessageClass = "Info")

  if (UnlinkedRecords.Detect == TRUE && !is.null(DataSetRoot))
  {
      # Make sure only relevant foreign key features are selected from DataSetRoot (This step is necessary to account for possibility of different RootSubjectKeys across data set tables)
      DataSetRoot <- DataSetRoot %>%
                          select(all_of(RootSubjectKey)) %>%
                          distinct()

      # DETECTOR: Get all records in 'Table' that do not have a match in 'DataSetRoot'
      Detector.UnlinkedRecords <- Table %>%
                                      anti_join(DataSetRoot, by = join_by(!!!syms(RootSubjectKey))) %>%
                                      mutate(.Nonconformance = "Unlinked",
                                             .HasBeenRemoved = FALSE)

      # Create COUNTER entry from Detector
      Counter.UnlinkedRecords <- Detector.UnlinkedRecords %>%
                                      summarize(Table = TableName,
                                                ProcessTopic = "Unlinked table records",
                                                CountLevel = "Topic",
                                                CountRecords.Detected = n(),
                                                CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey))),
                                                Message = paste0("Detected ", CountRecords.Detected, " unlinked records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                                MessageClass = "Info") %>%
                                      Counter.Make()

      # EXECUTE REMOVAL of unlinked table records
      if (UnlinkedRecords.Remove == TRUE && nrow(Detector.UnlinkedRecords) > 0)
      {
          Table <- DataSetRoot %>%
                        inner_join(Table, by = join_by(!!!syms(RootSubjectKey)))      # This effectively filters out records that are not linked to any data set root subject

          # Mark records in DETECTOR as removed
          Detector.UnlinkedRecords <- Detector.UnlinkedRecords %>%
                                          mutate(.HasBeenRemoved = TRUE)

          # Modify COUNTER after executed removal of unlinked records
          Counter.UnlinkedRecords <- Counter.UnlinkedRecords %>%
                                          mutate(CountRecords.Removed = CountRecords.Detected,
                                                 Message = paste0("Removed ", CountRecords.Removed, " unlinked records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                                 MessageClass = "Success",
                                                 Timestamp = Sys.time())
      }

      # Create LOG entry
      Log.UnlinkedRecords <- Counter.UnlinkedRecords %>%
                                  Log.Make() %>%
                                  mutate(ProcessExecution = "Executed")
  }

  # Print report message
  if (PrintMessages == TRUE) { Log.Print(Log.UnlinkedRecords) }



# 2) DETECT and SUBSTITUTE empty strings in character features
#-------------------------------------------------------------------------------

  Log.EmptyStrings <- Log.New(Table = TableName,
                              ProcessTopic = "Empty Strings",
                              ProcessExecution = "Omitted",
                              Message = "Omitted detection and substitution.",
                              MessageClass = "Info")

  if (EmptyStrings.Detect == TRUE)
  {
      # DETECTOR: Count records that contain empty strings and count times empty strings occur
      Detector.EmptyStrings <- Table %>%
                                  filter(if_any(where(is.character),
                                                ~ str_trim(.x) == "")) %>%       # This detects all strings that are empty ('') or contain only white space
                                  mutate(.CountEmptyStrings = rowSums(across(where(is.character),
                                                                             ~ str_trim(.x) == ""),
                                                                      na.rm = TRUE)) %>%
                                  select(all_of(RootSubjectKey),
                                         .CountEmptyStrings)

      # Create Log SUMMARY entry on DETECTION of empty strings
      Log.EmptyStrings <- Detector.EmptyStrings %>%
                              summarize(Table = TableName,
                                        ProcessTopic = "Empty strings",
                                        ProcessExecution = "Executed",
                                        CountRecords.Detected = n(),
                                        CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                        CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey))),
                                        Message = paste0("Detected a total of ", sum(.CountEmptyStrings, na.rm = TRUE), " empty strings in ", CountRecords.Detected, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                        MessageClass = "Info")

      if (EmptyStrings.Substitute == TRUE && nrow(Detector.EmptyStrings) > 0)      # Substitute only if any empty strings were found
      {
          # EXECUTE SUBSTITUTION of empty strings with NA values
          .Substitution <- ifelse(EmptyStrings.Substitution == "NA",
                                  NA_character_,
                                  EmptyStrings.Substitution)

          Table <- Table %>%
                        mutate(across(where(is.character),
                                      ~ case_when(str_trim(.x) == "" ~ .Substitution,
                                                  .default = .x)))

          # Modify LOG entry after executed substitution
          Log.EmptyStrings <- Log.EmptyStrings %>%
                                  mutate(ProcessExecution = "Executed",
                                         Message = paste0("Detected and substituted a total of ", sum(Detector.EmptyStrings$.CountEmptyStrings, na.rm = TRUE), " empty strings in ", CountRecords.Detected, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                         MessageClass = "Success",
                                         Timestamp = Sys.time())
      }

      # Finalize LOG entry
      Log.EmptyStrings <- Log.EmptyStrings %>%
                              Log.Make()
  }

  # Print report message
  if (PrintMessages == TRUE) { Log.Print(Log.EmptyStrings) }



# 3) DETECT and REMOVE duplicate records
#-------------------------------------------------------------------------------

  Detector.DuplicateRecords <- NULL
  Counter.DuplicateRecords <- NULL

  Log.DuplicateRecords <- Log.New(Table = TableName,
                                  ProcessTopic = "Duplicate records",
                                  ProcessExecution = "Omitted",
                                  Message = "Omitted detection and removal.",
                                  MessageClass = "Info")

  if (DuplicateRecords.Detect == TRUE)
  {
      # DETECTOR: Identify duplicate records and count them
      Detector.DuplicateRecords <- Table %>%
                                      mutate(.IsDuplicate = ifelse(PrimaryKeyIgnoredInRedundancyCheck == TRUE,      # duplicated() marks all duplicate records EXCEPT the first occurrence of duplicate records
                                                                   duplicated(across(-all_of(PrimaryKey))),      # marks rows that are a duplicate in all feature but the 'PrimaryKey' feature
                                                                   duplicated(.)),
                                             .Nonconformance = "Duplicate",
                                             .HasBeenRemoved = FALSE) %>%
                                      filter(.IsDuplicate == TRUE)

      # Create COUNTER entry from Detector
      Counter.DuplicateRecords <- Detector.DuplicateRecords %>%
                                      summarize(Table = TableName,
                                                ProcessTopic = "Duplicate records",
                                                CountLevel = "Topic",
                                                CountRecords.Detected = n(),
                                                CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey))),
                                                Message = paste0("Detected ", CountRecords.Detected, " duplicate records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                                MessageClass = "Info") %>%
                                      Counter.Make()

      Counter.DuplicateRecords.Details <- NULL

      # Create COUNTER DETAILS on DETECTION of duplicate records
      if (Counter.DuplicateRecords$CountRecords.Detected > 0)
      {
          Counter.DuplicateRecords.Details <- Detector.DuplicateRecords %>%
                                                  group_by(pick(all_of(RootSubjectKey))) %>%
                                                      summarize(.Group.CountDuplicateRecords = n(),
                                                                .CountDuplicateRecords = n()) %>%
                                                  ungroup() %>%
                                                  group_by(.Group.CountDuplicateRecords) %>%
                                                      summarize(CountRecords.Detected = sum(.CountDuplicateRecords),
                                                                CountRootSubjects.Affected = n(),
                                                                CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey)))) %>%
                                                  ungroup() %>%
                                                  mutate(Table = TableName,
                                                         ProcessTopic = "Duplicate records",
                                                         ProcessTopic.Subgroup = paste0(.Group.CountDuplicateRecords, " duplicate records"),
                                                         CountLevel = "Subgroup",
                                                         Message = paste0("Detected a total of ", CountRecords.Detected, " duplicate records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects> with ", ProcessTopic.Subgroup, "."),
                                                         MessageClass = "Details.Info") %>%
                                                  select(-.CountDuplicateRecords) %>%
                                                  Counter.Make()
      }

      # EXECUTE REMOVAL of duplicate records (only first occurrence is kept)
      if (DuplicateRecords.Remove == TRUE && nrow(Detector.DuplicateRecords) > 0)
      {
          if (PrimaryKeyIgnoredInRedundancyCheck == TRUE)
          {
              Table <- Table %>%
                          distinct(across(-all_of(PrimaryKey)), .keep_all = TRUE)

          } else {

              Table <- Table %>%
                          distinct()
          }

          # Mark records in DETECTOR as removed
          Detector.DuplicateRecords <- Detector.DuplicateRecords %>%
                                          mutate(.HasBeenRemoved = TRUE)

          # Modify COUNTER after executed removal of duplicate records
          Counter.DuplicateRecords <- Counter.UnlinkedRecords %>%
                                          mutate(CountRecords.Removed = CountRecords.Detected,
                                                 Message = paste0("Removed ", CountRecords.Removed, " duplicate records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                                 MessageClass = "Success",
                                                 Timestamp = Sys.time())

          # Modify COUNTER DETAILS after executed removal of duplicate records
          Counter.DuplicateRecords.Details <- Counter.DuplicateRecords.Details %>%
                                                  mutate(CountRecords.Removed = CountRecords.Detected,
                                                         Message = paste0("Removed a total of ", CountRecords.Detected, " duplicate records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects> with ", ProcessTopic.Subgroup, "."),
                                                         MessageClass = "Details.Success",
                                                         Timestamp = Sys.time())
      }

      # Row-bind Counter summary and details
      if (length(Counter.DuplicateRecords.Details) > 0 && nrow(Counter.DuplicateRecords.Details) > 0)
      {
          Counter.DuplicateRecords <- Counter.DuplicateRecords %>%
                                          bind_rows(Counter.DuplicateRecords.Details)
      }

      # Create LOG entry
      Log.DuplicateRecords <- Counter.DuplicateRecords %>%
                                  Log.Make() %>%
                                  mutate(ProcessExecution = "Executed")
  }

  # # Count Root / Seed subjects and records in current table version
  # CurrentCount <- Table %>%
  #                     summarize(RootSubjects = n_distinct(pick(all_of(RootSubjectKey))),
  #                               Records = n())
  #
  # # Add current root subject and record counts to Report
  # Report.DuplicateRecords <- Report.DuplicateRecords %>%
  #                                 mutate(CountRootSubjects.Current = CurrentCount$RootSubjects,
  #                                        CountRecords.Current = CurrentCount$Records)

  # Print report message
  if (PrintMessages == TRUE) { Log.Print(Log.DuplicateRecords) }


# 4) DETECT and REMOVE records that violate feature availability requirements
#-------------------------------------------------------------------------------

  Detector.FeatureAvailabilityViolations.Strict <- NULL
  Detector.FeatureAvailabilityViolations.TransFeature <- NULL

  Log.FeatureAvailabilityViolations <- Log.New(Table = TableName,
                                               ProcessTopic = "Feature availability violations",
                                               ProcessExecution = "Inapplicable",
                                               Message = "Found no requirements.",
                                               MessageClass = "Info")

  if (length(FeatureRequirements) > 0 && nrow(FeatureRequirements) > 0)
  {
      # Get table's set of (strictly) required features ...
      RequiredFeatures <- FeatureRequirements %>%
                              filter(Availability == "Required") %>%
                              pull(Feature) %>%
                              unique()

      # ... and also create a list with feature names as list element names and vectors of negligible values as elements
      NegligibleValues <- FeatureRequirements %>%
                              filter(Availability == "Required",
                                     !is.na(NegligibleValues)) %>%
                              select(Feature, NegligibleValues) %>%
                              distinct(Feature, .keep_all = TRUE) %>%
                              tibble::deframe() %>%
                              as.list() %>%
                              imap(function(FeatureNegligibleValues, featurename)
                                   {
                                      str_split(FeatureNegligibleValues, ",\\s")[[1]]
                                   })

      # Create auxiliary copy of original Table
      TableAuxCopy <- Table

      # In 'TableAuxCopy': Substitute values from 'NegligibleValues' with NA
      if (length(NegligibleValues) > 0)
      {
          for (featurename in names(NegligibleValues))
          {
              if (featurename %in% names(TableAuxCopy))
              {
                  CurrentNegligibleValues <- NegligibleValues[[featurename]]
                  TableAuxCopy[[featurename]] <- if_else(TableAuxCopy[[featurename]] %in% CurrentNegligibleValues,
                                                         NA,
                                                         TableAuxCopy[[featurename]])
              }
          }
      }

      if (FeatureAvailabilityViolations.Detect == FALSE)
      {
          Log.FeatureAvailabilityViolations <- Log.New(Table = TableName,
                                                       ProcessTopic = "Feature availability violations",
                                                       ProcessExecution = "Omitted",
                                                       Message = "Omitted detection and removal.",
                                                       MessageClass = "Info")
      } else {

          # Initiate Sub-Report objects
          Counter.FeatureAvailabilityViolations.Strict <- NULL
          Counter.FeatureAvailabilityViolations.Strict.Details <- NULL
          Counter.FeatureAvailabilityViolations.TransFeature <- NULL
          Counter.FeatureAvailabilityViolations.TransFeature.Details <- NULL

          # 4.1) First, handle strict feature availability requirements
          #-------------------------------------------------------------------------

          if (length(RequiredFeatures) > 0)
          {
              # DETECTOR: Which records have missing values in required features?
              Detector.FeatureAvailabilityViolations.Strict <- TableAuxCopy %>%      # Important! Using 'TableAuxCopy' here, not original table
                                                                    filter(if_any(all_of(RequiredFeatures),
                                                                                  ~ is.na(.x))) %>%
                                                                    mutate(.MissingRequiredFeatures = pmap_chr(select(., all_of(RequiredFeatures)),
                                                                                                               ~ paste(RequiredFeatures[is.na(c(...))], collapse = " & ")),      # This saves names of missing required features in a list-column of character vectors
                                                                           .Nonconformance = "Missing required features",
                                                                           .HasBeenRemoved = FALSE)

              # Create COUNTER SUMMARY entry from Detector
              Counter.FeatureAvailabilityViolations.Strict <- Detector.FeatureAvailabilityViolations.Strict %>%
                                                                  summarize(Table = TableName,
                                                                            ProcessTopic = "Feature availability violations",
                                                                            CountLevel = "Topic",
                                                                            CountRecords.Detected = n(),
                                                                            CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                                            CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey))),
                                                                            Message = paste0("Detected ", CountRecords.Detected, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                                                            MessageClass = "Info") %>%
                                                                  Counter.Make()

              # Create COUNTER DETAILS on DETECTION of records with feature availability violations
              if (nrow(Detector.FeatureAvailabilityViolations.Strict) > 0)
              {
                  Counter.FeatureAvailabilityViolations.Strict.Details <- Detector.FeatureAvailabilityViolations.Strict %>%
                                                                              group_by(.MissingRequiredFeatures) %>%
                                                                                  summarize(CountRecords.Detected = n(),
                                                                                            CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                                                            CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey)))) %>%
                                                                              ungroup() %>%
                                                                              mutate(Table = TableName,
                                                                                     ProcessTopic = "Feature availability violations",
                                                                                     ProcessTopic.Subgroup = paste0("Feature availability violations: ", .MissingRequiredFeatures),
                                                                                     CountLevel = "Subgroup",
                                                                                     Message = paste0("Detected ", CountRecords.Detected, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects> with missing values in the following required features: ", .MissingRequiredFeatures),
                                                                                     MessageClass = "Details.Info")
              }

              # EXECUTE REMOVAL of records that have missing values in any of strictly required features
              if (FeatureAvailabilityViolations.Remove == TRUE && nrow(Detector.FeatureAvailabilityViolations.Strict) > 0)
              {
                  # Filter out records from original table that are present in Detector object
                  Table <- Table %>%
                              filter(!(.AuxID %in% Detector.FeatureAvailabilityViolations.Strict$.AuxID))

                  # Also filter out records from 'TableAuxCopy' so they do not get 're-detected' in further proceedings
                  TableAuxCopy <- TableAuxCopy %>%
                                      filter(!(.AuxID %in% Detector.FeatureAvailabilityViolations.Strict$.AuxID))

                  # Mark records in DETECTOR as removed
                  Detector.FeatureAvailabilityViolations.Strict <- Detector.FeatureAvailabilityViolations.Strict %>%
                                                                      mutate(.HasBeenRemoved = TRUE)

                  # Modify COUNTER after executed removal of records with feature availability violations
                  Counter.FeatureAvailabilityViolations.Strict <- Counter.FeatureAvailabilityViolations.Strict %>%
                                                                      mutate(CountRecords.Removed = CountRecords.Detected,
                                                                             Message = paste0("Removed ", CountRecords.Removed, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                                                             MessageClass = "Success",
                                                                             Timestamp = Sys.time())

                  # Modify COUNTER DETAILS after executed removal of records with feature availability violations
                  Counter.FeatureAvailabilityViolations.Strict.Details <- Counter.FeatureAvailabilityViolations.Strict.Details %>%
                                                                              mutate(CountRecords.Removed = CountRecords.Detected,
                                                                                     Message = paste0("Removed ", CountRecords.Removed, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects> with missing values in the following required features: ", .MissingRequiredFeatures),
                                                                                     MessageClass = "Details.Success",
                                                                                     Timestamp = Sys.time())
              }

              # COUNTER DETAILS: Remove special grouping column not needed anymore and turn data.frame into Counter entry
              if (length(Counter.FeatureAvailabilityViolations.Strict.Details) > 0)
              {
                  Counter.FeatureAvailabilityViolations.Strict.Details <- Counter.FeatureAvailabilityViolations.Strict.Details %>%
                                                                              select(-.MissingRequiredFeatures) %>%
                                                                              Counter.Make()
              }
          }


          # 4.2) Second, handle trans-feature availability requirements
          #-------------------------------------------------------------------------

          # Get table's set of trans-feature availability requirements (stated as pseudo-code)
          TransFeatureRequirements <- FeatureRequirements %>%
                                          filter(!is.na(Availability),
                                                 Availability != "NA",
                                                 Availability != "Required") %>%
                                          distinct(Availability) %>%
                                          pull()

          if (length(TransFeatureRequirements) > 0)
          {
              # Set names for requirement vector. These names will serve as column names later on (induced by a mutate statement).
              names(TransFeatureRequirements) <- paste0(".TransFeatureRequirement.", 1:length(TransFeatureRequirements))

              # Compile list of non-evaluated expressions for subsequent mutate statement
              TransFeatureRequirements.Expr <- TransFeatureRequirements %>%
                                                    CompileTransFeatureRequirements() %>%      # ... use CompileTransFeatureRequirements() to turn the pseudo-code into evaluable strings...
                                                    lapply(., rlang::parse_expr) %>%      # ... then transform them into a list of expressions
                                                    setNames(names(TransFeatureRequirements))      # The keys of the list will be the column names of auxiliary features used to determine whether records violate requirements (s. proceedings)

              # DETECTOR: Track violations of trans-feature availability requirements
              Detector.FeatureAvailabilityViolations.TransFeature <- TableAuxCopy %>%      # Important! Using 'TableAuxCopy' here, not original table
                                                                          mutate(!!!TransFeatureRequirements.Expr) %>%      # Use list of expressions created earlier to apply trans-feature rules to all records ...
                                                                          filter(if_any(all_of(names(TransFeatureRequirements)), ~ .x == FALSE)) %>%      # ... and filter out any records that violate those rules
                                                                          mutate(.ViolatedTransFeatureRequirements = pmap_chr(select(., all_of(names(TransFeatureRequirements))),
                                                                                                                              ~ paste(TransFeatureRequirements[names(TransFeatureRequirements)[c(...) == FALSE]], collapse = " & ")),      # This returns a list-column with character vectors containing trans-feature rules as pseudo-code
                                                                                 .Nonconformance = "Violating trans-feature requirements",
                                                                                 .HasBeenRemoved = FALSE)

              # Create COUNTER SUMMARY entry from Detector
              Counter.FeatureAvailabilityViolations.TransFeature <- Detector.FeatureAvailabilityViolations.TransFeature %>%
                                                                        summarize(Table = TableName,
                                                                                  ProcessTopic = "Trans-feature availability violations",
                                                                                  CountLevel = "Topic",
                                                                                  CountRecords.Detected = n(),
                                                                                  CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                                                  CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey))),
                                                                                  Message = paste0("Detected ", CountRecords.Detected, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                                                                  MessageClass = "Info") %>%
                                                                        Counter.Make()

              # Create COUNTER DETAILS on DETECTION of records that violate trans-feature availability requirements
              if (nrow(Detector.FeatureAvailabilityViolations.TransFeature) > 0)
              {
                  Counter.FeatureAvailabilityViolations.TransFeature.Details <- Detector.FeatureAvailabilityViolations.TransFeature %>%
                                                                                    group_by(.ViolatedTransFeatureRequirements) %>%
                                                                                        summarize(CountRecords.Detected = n(),
                                                                                                  CountRootSubjects.Affected = n_distinct(pick(all_of(RootSubjectKey))),
                                                                                                  CountSeedSubjects.Affected = n_distinct(pick(all_of(SeedSubjectKey)))) %>%
                                                                                    ungroup() %>%
                                                                                    mutate(Table = TableName,
                                                                                           ProcessTopic = "Trans-feature availability violations",
                                                                                           ProcessTopic.Subgroup = paste0("Trans-feature availability violations: ", .ViolatedTransFeatureRequirements),
                                                                                           CountLevel = "Subgroup",
                                                                                           Message = paste0("Detected ", CountRecords.Detected, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects> and violating the following trans-feature availability requirements: ", .ViolatedTransFeatureRequirements),
                                                                                           MessageClass = "Details.Info")
              }

              # EXECUTE REMOVAL of records that violate trans-feature availability requirements
              if (FeatureAvailabilityViolations.Remove == TRUE && nrow(Detector.FeatureAvailabilityViolations.TransFeature) > 0)
              {
                  # Filter out records from original table that are present in Detector object
                  Table <- Table %>%
                              filter(!(.AuxID %in% Detector.FeatureAvailabilityViolations.TransFeature$.AuxID))

                  # Mark records in DETECTOR as removed
                  Detector.FeatureAvailabilityViolations.TransFeature <- Detector.FeatureAvailabilityViolations.TransFeature %>%
                                                                            mutate(.HasBeenRemoved = TRUE)

                  # Modify COUNTER SUMMARY after executed removal of records with trans-feature availability violations
                  Counter.FeatureAvailabilityViolations.TransFeature <- Counter.FeatureAvailabilityViolations.TransFeature %>%
                                                                            mutate(CountRecords.Removed = CountRecords.Detected,
                                                                                   Message = paste0("Removed ", CountRecords.Removed, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects>."),
                                                                                   MessageClass = "Success",
                                                                                   Timestamp = Sys.time())

                  # Modify COUNTER DETAILS after executed removal of records with trans-feature availability violations
                  Counter.FeatureAvailabilityViolations.TransFeature.Details <- Counter.FeatureAvailabilityViolations.TransFeature.Details %>%
                                                                                    mutate(CountRecords.Removed = CountRecords.Detected,
                                                                                           Message = paste0("Removed ", CountRecords.Removed, " records belonging to ", CountRootSubjects.Affected, " <Root subjects> / ", CountSeedSubjects.Affected, " <Seed subjects> and violating the following trans-feature availability requirements: ", .ViolatedTransFeatureRequirements),
                                                                                           MessageClass = "Details.Success",
                                                                                           Timestamp = Sys.time())
              }

              # COUNTER DETAILS: Remove special grouping column not needed anymore and turn data.frame into Counter entry
              if (length(Counter.FeatureAvailabilityViolations.TransFeature.Details) > 0)
              {
                  Counter.FeatureAvailabilityViolations.TransFeature.Details <- Counter.FeatureAvailabilityViolations.TransFeature.Details %>%
                                                                                    select(-.ViolatedTransFeatureRequirements) %>%
                                                                                    Counter.Make()
              }
          }

          # Compile consolidated 'Counter.FeatureAvailabilityViolations'
          Counter.FeatureAvailabilityViolations <- bind_rows(Counter.FeatureAvailabilityViolations.Strict,
                                                             Counter.FeatureAvailabilityViolations.Strict.Details,
                                                             Counter.FeatureAvailabilityViolations.TransFeature,
                                                             Counter.FeatureAvailabilityViolations.TransFeature.Details)
      }

      # Create LOG entry
      Log.FeatureAvailabilityViolations <- Counter.FeatureAvailabilityViolations %>%
                                                Log.Make() %>%
                                                mutate(ProcessExecution = "Executed")
  }

  # # Count root subjects and records in current table version
  # CurrentCount <- Table %>%
  #                     summarize(RootSubjects = n_distinct(pick(all_of(RootSubjectKey))),
  #                               Records = n())
  #
  # # Add current root subject and record counts to Report
  # Report.FeatureAvailabilityViolations <- Report.FeatureAvailabilityViolations %>%
  #                                             mutate(CountRootSubjects.Current = CurrentCount$RootSubjects,
  #                                                    CountRecords.Current = CurrentCount$Records)

  # Print report messages
  if (PrintMessages == TRUE) { Log.Print(Log.FeatureAvailabilityViolations) }

  # Remove auxiliary feature '.AuxID'
  Table <- Table %>%
              select(-.AuxID)


# Consolidate reporting objects
#-------------------------------------------------------------------------------
  Counter <- bind_rows(Counter.UnlinkedRecords,
                       Counter.DuplicateRecords,
                       Counter.FeatureAvailabilityViolations)

  Log <- bind_rows(Log.UnlinkedRecords,
                   Log.EmptyStrings,
                   Log.DuplicateRecords,
                   Log.FeatureAvailabilityViolations)


# Row-bind all Detector data.frames into one that contains all nonconforming table records
#-------------------------------------------------------------------------------
  NonconformingRecords <- Detector.UnlinkedRecords %>%
                              bind_rows(Detector.DuplicateRecords) %>%
                              bind_rows(Detector.FeatureAvailabilityViolations.Strict) %>%
                              bind_rows(Detector.FeatureAvailabilityViolations.TransFeature) %>%
                              arrange(.HasBeenRemoved) %>%      # This is important, if a non-conforming record was only detected in one Detector and marked as removed in another one. Put the removed one first.
                              distinct(.AuxID, .keep_all = TRUE) %>%      # This is important, because some non-conforming records could be present in multiple Detectors, if they were only detected and not removed
                              { if (".AuxID" %in% names(.)) { select(., -.AuxID) } else {.} }


#-------------------------------------------------------------------------------
  return(list(Table = Table,
              NonconformingRecords = NonconformingRecords,
              Counter = Counter,
              Log = Log))
}
