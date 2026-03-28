
#' CleanTable
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Perform exclusion of invalid table records.
#'
#' @param Table \code{data.frame} or \code{tibble} - The table object to be cleaned
#' @param PrimaryKey \code{character vector} - Name of feature(s) that serve(s) as table's primary key
#' @param ForeignKey \code{character vector} - Names of features that serve as table's foreign key (usually primary key of data set 'root subjects')
#' @param PrimaryKeyIgnoredInRedundancyCheck \code{logical} - Indicating whether primary key feature has no semantic meaning and can be ignored when determining redundancy - Default: \code{TRUE}
#' @param DataSetRoot \code{data.frame} (Optional) - Identifying all data set root subjects (e.g. pairs of PatientIDs and DiagnosisIDs). The \code{data.frame}'s feature names must contain foreign key features in depending tables.
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
#'
#' @return \code{tibble} - Clean table
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CleanTable <- function(Table,
                       PrimaryKey,
                       ForeignKey = NULL,
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
                       FeatureAvailabilityViolations.Remove = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSet$Staging
  # tablename <- "Staging"
  # PrimaryKey <- PrimaryKeys[[tablename]]
  # ForeignKey <- ForeignKeys[[tablename]]
  # PrimaryKeyIgnoredInRedundancyCheck <- TRUE
  # DataSetRoot <- DataSetRoot
  # UnlinkedRecords.Detect <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(UnlinkedRecords.Detect)
  # UnlinkedRecords.Remove <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(UnlinkedRecords.Remove)
  # EmptyStrings.Detect <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Detect)
  # EmptyStrings.Substitute <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Substitute)
  # EmptyStrings.Substitution <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Substitution)
  # DuplicateRecords.Detect <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(DuplicateRecords.Detect)
  # DuplicateRecords.Remove <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(DuplicateRecords.Remove)
  # FeatureRequirements <- Settings$FeatureRequirements %>% filter(Table == tablename)
  # FeatureAvailabilityViolations.Detect <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(FeatureAvailabilityViolations.Detect)
  # FeatureAvailabilityViolations.Remove <- Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(FeatureAvailabilityViolations.Remove)

  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.character(PrimaryKey),
              is.flag(PrimaryKeyIgnoredInRedundancyCheck),
              is.flag(UnlinkedRecords.Detect),
              is.flag(UnlinkedRecords.Remove),
              is.flag(EmptyStrings.Detect),
              is.flag(EmptyStrings.Substitute),
              is.string(EmptyStrings.Substitution),
              is.flag(DuplicateRecords.Detect),
              is.flag(DuplicateRecords.Remove),
              is.flag(FeatureAvailabilityViolations.Detect),
              is.flag(FeatureAvailabilityViolations.Remove))
  stopifnot("ERROR: 'PrimaryKey' must contain column names of 'Table'." = (PrimaryKey %in% names(Table)))
  if (!is.null(DataSetRoot)) { assert_that(is.data.frame(DataSetRoot))
                               stopifnot("ERROR: 'DataSetRoot' must no be empty!" = (length(DataSetRoot) > 0 && nrow(DataSetRoot) > 0))
                               stopifnot("ERROR: 'ForeignKey' must contain column names of 'DataSetRoot'!" = (all(ForeignKey %in% names(DataSetRoot)))) }
  if (length(ForeignKey) > 0) { assert_that(is.character(ForeignKey))
                                stopifnot("ERROR: 'ForeignKey' must contain column names of 'Table'!" = (all(ForeignKey %in% names(Table)))) }
  if (!is.null(FeatureRequirements)) { assert_that(is.data.frame(FeatureRequirements)) }

#-------------------------------------------------------------------------------

  # If no 'ForeignKey' is passed (as usually the case in the 'Seed' table), set the variable equal to 'PrimaryKey' (this ensures syntactic validity throughout function proceedings)
  if (length(ForeignKey) == 0) { ForeignKey <- PrimaryKey }

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


# 1) DETECT and REMOVE records that are not linked with data set root subjects via foreign key
#-------------------------------------------------------------------------------

  Tracker.UnlinkedRecords <- NULL

  Report.UnlinkedRecords <- tibble(ProcessTopic = "Unlinked table records",
                                   ProcessExecution = "Omitted",
                                   ReportType = "Message",
                                   DetailsGroup = NA_character_,
                                   CountRootSubjects.Affected = NA_integer_,
                                   CountRecords.Affected = NA_integer_,
                                   CountRecords.Removed = NA_integer_,
                                   Message = "Unlinked table records: Omitted detection and removal.",
                                   MessageClass = "Info",
                                   Timestamp = Sys.time())

  if (UnlinkedRecords.Detect == TRUE && !is.null(DataSetRoot))
  {
      # Make sure only relevant foreign key features are selected from DataSetRoot (This step is necessary to account for possibility of different ForeignKeys across data set tables)
      DataSetRoot <- DataSetRoot %>%
                          select(all_of(ForeignKey)) %>%
                          distinct()

      # For TRACKING purposes: Get all records in 'Table' that do not have a match in 'DataSetRoot'
      Tracker.UnlinkedRecords <- Table %>%
                                    anti_join(DataSetRoot, by = join_by(!!!syms(ForeignKey))) %>%
                                    mutate(.Nonconformance = "Unlinked",
                                           .HasBeenRemoved = FALSE)

      # Create summarizing report from Tracker object
      Report.UnlinkedRecords <- Tracker.UnlinkedRecords %>%
                                    summarize(ProcessTopic = "Unlinked table records",
                                              ProcessExecution = "Detection",
                                              ReportType = "Summary",
                                              DetailsGroup = NA_character_,
                                              CountRootSubjects.Affected = n_distinct(across(all_of(ForeignKey))),
                                              CountRecords.Affected = n(),
                                              Message = paste0("Unlinked table records: Detected ", CountRecords.Affected, " unlinked records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                              MessageClass = "Info",
                                              Timestamp = Sys.time())

      # EXECUTE REMOVAL of unlinked table records
      if (UnlinkedRecords.Remove == TRUE && nrow(Tracker.UnlinkedRecords) > 0)
      {
          Table <- DataSetRoot %>%
                        inner_join(Table, by = join_by(!!!syms(ForeignKey)))      # This effectively filters out records that are not linked to any data set root subject

          # Mark records in TRACKER as removed
          Tracker.UnlinkedRecords <- Tracker.UnlinkedRecords %>%
                                          mutate(.HasBeenRemoved = TRUE)

          # Modify REPORT SUMMARY after executed removal of unlinked records
          Report.UnlinkedRecords <- Report.UnlinkedRecords %>%
                                          mutate(ProcessExecution = "Removal",
                                                 CountRecords.Removed = CountRecords.Affected,
                                                 Message = paste0("Unlinked table records: Detected and removed ", CountRecords.Removed, " unlinked records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                                 MessageClass = "Success",
                                                 Timestamp = Sys.time())
      }
  }


# 2) DETECT and SUBSTITUTE empty strings in character features
#-------------------------------------------------------------------------------

  Report.EmptyStrings <- tibble(ProcessTopic = "Empty Strings",
                                ProcessExecution = "Omitted",
                                ReportType = "Message",
                                DetailsGroup = NA_character_,
                                CountRootSubjects.Affected = NA_integer_,
                                CountRecords.Affected = NA_integer_,
                                Message = "Empty Strings: Omitted detection and substitution.",
                                MessageClass = "Info",
                                Timestamp = Sys.time())

  if (EmptyStrings.Detect == TRUE)
  {
      # For TRACKING purposes: Count records that contain empty strings and count times empty strings occur
      Tracker.EmptyStrings <- Table %>%
                                  filter(if_any(where(is.character),
                                                ~ str_trim(.x) == "")) %>%      # This detects all strings that are empty ('') or contain only white space
                                  mutate(.CountEmptyStrings = rowSums(across(where(is.character),
                                                                             ~ .x == ""))) %>%
                                  select(all_of(ForeignKey),
                                         .CountEmptyStrings)

      # Create REPORT SUMMARY on DETECTION of empty strings
      Report.EmptyStrings <- Tracker.EmptyStrings %>%
                                  summarize(ProcessTopic = "Empty strings",
                                            ProcessExecution = "Detection",
                                            ReportType = "Summary",
                                            DetailsGroup = NA_character_,
                                            CountRootSubjects.Affected = n_distinct(across(all_of(ForeignKey))),
                                            CountRecords.Affected = n(),
                                            Message = paste0("Empty Strings: Detected a total of ", sum(.CountEmptyStrings, na.rm = TRUE), " empty strings in ", CountRecords.Affected, " records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                            MessageClass = "Info",
                                            Timestamp = Sys.time())

      if (EmptyStrings.Substitute == TRUE && nrow(Tracker.EmptyStrings) > 0)      # Substitute only if any empty strings were found
      {
          # EXECUTE SUBSTITUTION of empty strings with NA values
          .Substitution <- ifelse(EmptyStrings.Substitution == "NA",
                                  NA_character_,
                                  EmptyStrings.Substitution)

          Table <- Table %>%
                        mutate(across(where(is.character),
                                      ~ case_when(str_trim(.x) == "" ~ .Substitution,
                                                  .default = .x)))

          # Modify REPORT after executed substitution
          Report.EmptyStrings <- Report.EmptyStrings %>%
                                      mutate(ProcessExecution = "Substitution",
                                             Message = paste0("Empty Strings: Detected and substituted a total of ", sum(Tracker.EmptyStrings$.CountEmptyStrings, na.rm = TRUE), " empty strings in ", CountRecords.Affected, " records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                             MessageClass = "Success",
                                             Timestamp = Sys.time())
      }
  }


# 3) DETECT and REMOVE duplicate records
#-------------------------------------------------------------------------------

  Tracker.DuplicateRecords <- NULL

  Report.DuplicateRecords <- tibble(ProcessTopic = "Duplicate records",
                                    ProcessExecution = "Omitted",
                                    ReportType = "Message",
                                    DetailsGroup = NA_character_,
                                    CountRootSubjects.Affected = NA_integer_,
                                    CountRecords.Affected = NA_integer_,
                                    CountRecords.Removed = NA_integer_,
                                    Message = "Duplicated records: Omitted detection and removal.",
                                    MessageClass = "Info",
                                    Timestamp = Sys.time())

  if (DuplicateRecords.Detect == TRUE)
  {
      # For TRACKING purposes: Identify duplicate records and count them
      Tracker.DuplicateRecords <- Table %>%
                                      mutate(.IsDuplicate = ifelse(PrimaryKeyIgnoredInRedundancyCheck == TRUE,      # duplicated() marks all duplicate records EXCEPT the first occurrence of duplicate records
                                                                   duplicated(across(-all_of(PrimaryKey))),      # marks rows that are a duplicate in all feature but the 'PrimaryKey' feature
                                                                   duplicated(.)),
                                             .Nonconformance = "Duplicate",
                                             .HasBeenRemoved = FALSE) %>%
                                      filter(.IsDuplicate == TRUE)

      # Create REPORT SUMMARY on DETECTION of duplicate records
      Report.DuplicateRecords <- Tracker.DuplicateRecords %>%
                                      summarize(ProcessTopic = "Duplicate records",
                                                ProcessExecution = "Detection",
                                                ReportType = "Summary",
                                                DetailsGroup = NA_character_,
                                                CountRootSubjects.Affected = n_distinct(across(all_of(ForeignKey))),
                                                CountRecords.Affected = n(),
                                                Message = paste0("Duplicate records: Detected ", CountRecords.Affected, " duplicate records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                                MessageClass = "Info",
                                                Timestamp = Sys.time())

      ReportDetails.DuplicateRecords <- NULL

      # Create REPORT DETAILS on DETECTION of duplicate records
      if (Report.DuplicateRecords$CountRecords.Affected > 0)
      {
          ReportDetails.DuplicateRecords <- Tracker.DuplicateRecords %>%
                                                group_by(across(all_of(ForeignKey))) %>%
                                                    summarize(.Group.CountDuplicateRecords = n(),
                                                              .CountDuplicateRecords = n()) %>%
                                                ungroup() %>%
                                                group_by(.Group.CountDuplicateRecords) %>%
                                                    summarize(CountRootSubjects.Affected = n(),
                                                              CountRecords.Affected = sum(.CountDuplicateRecords)) %>%
                                                ungroup() %>%
                                                mutate(ProcessTopic = "Duplicate records",
                                                       ProcessExecution = "Detection",
                                                       ReportType = "Details",
                                                       DetailsGroup = paste0(.Group.CountDuplicateRecords, " duplicate records"),
                                                       Message = paste0("Duplicate records: Detected a total of ", CountRecords.Affected, " duplicate records belonging to ", CountRootSubjects.Affected, " root subjects with ", DetailsGroup, "."),
                                                       MessageClass = "Info",
                                                       Timestamp = Sys.time()) %>%
                                                select(-.CountDuplicateRecords)
      }

      # EXECUTE REMOVAL of duplicate records (only first occurrence is kept)
      if (DuplicateRecords.Remove == TRUE && nrow(Tracker.DuplicateRecords) > 0)
      {
          if (PrimaryKeyIgnoredInRedundancyCheck == TRUE)
          {
              Table <- Table %>%
                          distinct(across(-all_of(PrimaryKey)), .keep_all = TRUE)

          } else {

              Table <- Table %>%
                          distinct()
          }

          # Mark records in TRACKER as removed
          Tracker.DuplicateRecords <- Tracker.DuplicateRecords %>%
                                          mutate(.HasBeenRemoved = TRUE)

          # Modify REPORT SUMMARY after executed removal of duplicate records
          Report.DuplicateRecords <- Report.DuplicateRecords %>%
                                          mutate(ProcessExecution = "Removal",
                                                 CountRecords.Removed = CountRecords.Affected,
                                                 Message = paste0("Duplicate records: Detected and removed ", CountRecords.Removed, " duplicate records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                                 MessageClass = "Success",
                                                 Timestamp = Sys.time())

          # Modify REPORT DETAILS after executed removal of duplicate records
          ReportDetails.DuplicateRecords <- ReportDetails.DuplicateRecords %>%
                                                mutate(ProcessExecution = "Removal",
                                                       CountRecords.Removed = CountRecords.Affected,
                                                       Message = paste0("Duplicate records: Detected and removed a total of ", CountRecords.Removed, " duplicate records belonging to ", CountRootSubjects.Affected, " root subjects with ", DetailsGroup, "."),
                                                       MessageClass = "Success",
                                                       Timestamp = Sys.time())
      }

      # Row-bind Report summary and Report details
      if (length(ReportDetails.DuplicateRecords) > 0 && nrow(ReportDetails.DuplicateRecords))
      {
          Report.DuplicateRecords <- Report.DuplicateRecords %>%
                                          bind_rows(ReportDetails.DuplicateRecords)
      }
  }


# 4) DETECT and REMOVE records that violate feature availability requirements
#-------------------------------------------------------------------------------

  Tracker.FeatureAvailabilityViolations.Strict <- NULL
  Tracker.FeatureAvailabilityViolations.TransFeature <- NULL

  if (length(FeatureRequirements) == 0 || nrow(FeatureRequirements) == 0)
  {
      Report.FeatureAvailabilityViolations <- tibble(ProcessTopic = "Feature availability violations",
                                                     ProcessExecution = "Inapplicable",
                                                     ReportType = "Message",
                                                     Message = "Feature availability violations: Found no requirements.",
                                                     MessageClass = "Info",
                                                     Timestamp = Sys.time())
  } else {

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


      if (FeatureAvailabilityViolations.Detect == TRUE)
      {
          # Initiate Sub-Report objects
          Report.FeatureAvailabilityViolations.Strict <- NULL
          ReportDetails.FeatureAvailabilityViolations.Strict <- NULL
          Report.FeatureAvailabilityViolations.TransFeature <- NULL
          ReportDetails.FeatureAvailabilityViolations.TransFeature <- NULL

          # 4.1) First, handle strict feature availability requirements
          #-------------------------------------------------------------------------

          if (length(RequiredFeatures) > 0)
          {
              # For TRACKING purposes: Track which records have missing values in required features
              Tracker.FeatureAvailabilityViolations.Strict <- TableAuxCopy %>%      # Important! Using 'TableAuxCopy' here, not original table
                                                                  filter(if_any(all_of(RequiredFeatures),
                                                                                ~ is.na(.x))) %>%
                                                                  mutate(.MissingRequiredFeatures = pmap_chr(select(., all_of(RequiredFeatures)),
                                                                                                             ~ paste(RequiredFeatures[is.na(c(...))], collapse = " / ")),      # This saves names of missing required features in a list-column of character vectors
                                                                         .Nonconformance = "Missing required features",
                                                                         .HasBeenRemoved = FALSE)

              # Create REPORT SUMMARY on DETECTION of records with feature availability violations
              Report.FeatureAvailabilityViolations.Strict <- Tracker.FeatureAvailabilityViolations.Strict %>%
                                                                  summarize(ProcessTopic = "Feature availability violations",
                                                                            ProcessExecution = "Detection",
                                                                            ReportType = "Summary",
                                                                            DetailsGroup = NA,
                                                                            CountRootSubjects.Affected = n_distinct(across(all_of(ForeignKey))),
                                                                            CountRecords.Affected = n()) %>%
                                                                  mutate(Message = paste0("Feature availability violations: Detected ", CountRecords.Affected, " records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                                                         MessageClass = "Info",
                                                                         Timestamp = Sys.time())

              # Create REPORT DETAILS on DETECTION of records with feature availability violations
              if (nrow(Tracker.FeatureAvailabilityViolations.Strict) > 0)
              {
                  ReportDetails.FeatureAvailabilityViolations.Strict <- Tracker.FeatureAvailabilityViolations.Strict %>%
                                                                            group_by(.MissingRequiredFeatures) %>%
                                                                                summarize(CountRootSubjects.Affected = n_distinct(across(all_of(ForeignKey))),
                                                                                          CountRecords.Affected = n()) %>%
                                                                            ungroup() %>%
                                                                            mutate(ProcessTopic = "Feature availability violations",
                                                                                   ProcessExecution = "Detection",
                                                                                   ReportType = "Details",
                                                                                   DetailsGroup = paste0("Feature availability violations: ", .MissingRequiredFeatures),
                                                                                   Message = paste0("Feature availability violations: Detected ", CountRecords.Affected, " records belonging to ", CountRootSubjects.Affected, " root subjects with missing values in the following required features: ", .MissingRequiredFeatures),
                                                                                   MessageClass = "Info",
                                                                                   Timestamp = Sys.time())
              }

              # EXECUTE REMOVAL of records that have missing values in any of strictly required features
              if (FeatureAvailabilityViolations.Remove == TRUE && nrow(Tracker.FeatureAvailabilityViolations.Strict) > 0)
              {
                  # Filter out records from original table that are present in Tracker object
                  Table <- Table %>%
                              filter(!(.AuxID %in% Tracker.FeatureAvailabilityViolations.Strict$.AuxID))

                  # Mark records in TRACKER as removed
                  Tracker.FeatureAvailabilityViolations.Strict <- Tracker.FeatureAvailabilityViolations.Strict %>%
                                                                      mutate(.HasBeenRemoved = TRUE)

                  # Modify REPORT SUMMARY after executed removal of records with feature availability violations
                  Report.FeatureAvailabilityViolations.Strict <- Report.FeatureAvailabilityViolations.Strict %>%
                                                                      mutate(ProcessExecution = "Removal",
                                                                             CountRecords.Removed = CountRecords.Affected,
                                                                             Message = paste0("Feature availability violations: Detected and removed ", CountRecords.Removed, " records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                                                             MessageClass = "Success",
                                                                             Timestamp = Sys.time())

                  # Modify REPORT DETAILS after executed removal of records with feature availability violations
                  ReportDetails.FeatureAvailabilityViolations.Strict <- ReportDetails.FeatureAvailabilityViolations.Strict %>%
                                                                            mutate(ProcessExecution = "Removal",
                                                                                   CountRecords.Removed = CountRecords.Affected,
                                                                                   Message = paste0("Feature availability violations: Detected and removed ", CountRecords.Removed, " records belonging to ", CountRootSubjects.Affected, " root subjects with missing values in the following required features: ", .MissingRequiredFeatures),
                                                                                   MessageClass = "Success",
                                                                                   Timestamp = Sys.time())
              }

              # Remove special grouping column not needed anymore
              if (length(ReportDetails.FeatureAvailabilityViolations.Strict) > 0)
              {
                  ReportDetails.FeatureAvailabilityViolations.Strict <- ReportDetails.FeatureAvailabilityViolations.Strict %>%
                                                                            select(-.MissingRequiredFeatures)
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

              # For TRACKING purposes: Track violations of trans-feature availability requirements
              Tracker.FeatureAvailabilityViolations.TransFeature <- TableAuxCopy %>%      # Important! Using 'TableAuxCopy' here, not original table
                                                                        mutate(!!!TransFeatureRequirements.Expr) %>%      # Use list of expressions created earlier to apply trans-feature rules to all records ...
                                                                        filter(if_any(all_of(names(TransFeatureRequirements)), ~ .x == FALSE)) %>%      # ... and filter out any records that violate those rules
                                                                        mutate(.ViolatedTransFeatureRequirements = pmap_chr(select(., all_of(names(TransFeatureRequirements))),
                                                                                                                            ~ paste(TransFeatureRequirements[names(TransFeatureRequirements)[c(...) == FALSE]], collapse = " / ")),      # This returns a list-column with character vectors containing trans-feature rules as pseudo-code
                                                                               .Nonconformance = "Violating trans-feature requirements",
                                                                               .HasBeenRemoved = FALSE)

              # Create REPORT SUMMARY on DETECTION of records that violate trans-feature availability requirements
              Report.FeatureAvailabilityViolations.TransFeature <- Tracker.FeatureAvailabilityViolations.TransFeature %>%
                                                                        summarize(ProcessTopic = "Feature availability violations",
                                                                                  ProcessExecution = "Detection",
                                                                                  ReportType = "Summary",
                                                                                  DetailsGroup = NA_character_,
                                                                                  CountRootSubjects.Affected = n_distinct(across(all_of(ForeignKey))),
                                                                                  CountRecords.Affected = n()) %>%
                                                                        mutate(Message = paste0("Trans-feature availability violations: Detected ", CountRecords.Affected, " records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                                                               MessageClass = "Info",
                                                                               Timestamp = Sys.time())

              # Create REPORT DETAILS on DETECTION of records that violate trans-feature availability requirements
              if (nrow(Tracker.FeatureAvailabilityViolations.TransFeature) > 0)
              {
                  ReportDetails.FeatureAvailabilityViolations.TransFeature <- Tracker.FeatureAvailabilityViolations.TransFeature %>%
                                                                                  group_by(.ViolatedTransFeatureRequirements) %>%
                                                                                      summarize(CountRootSubjects.Affected = n_distinct(across(all_of(ForeignKey))),
                                                                                                CountRecords.Affected = n()) %>%
                                                                                  ungroup() %>%
                                                                                  mutate(ProcessTopic = "Feature availability violations",
                                                                                         ProcessExecution = "Detection",
                                                                                         ReportType = "Details",
                                                                                         DetailsGroup = paste0("Trans-feature availability violations: ", .ViolatedTransFeatureRequirements),
                                                                                         Message = paste0("Trans-feature availability violations: Detected ", CountRecords.Affected, " records belonging to ", CountRootSubjects.Affected, " root subjects and violating the following trans-feature availability requirements: ", .ViolatedTransFeatureRequirements),
                                                                                         MessageClass = "Info",
                                                                                         Timestamp = Sys.time())
              }

              # EXECUTE REMOVAL of records that violate trans-feature availability requirements
              if (FeatureAvailabilityViolations.Remove == TRUE && nrow(Tracker.FeatureAvailabilityViolations.TransFeature) > 0)
              {
                  # Filter out records from original table that are present in Tracker object
                  Table <- Table %>%
                              filter(!(.AuxID %in% Tracker.FeatureAvailabilityViolations.TransFeature$.AuxID))

                  # Mark records in TRACKER as removed
                  Tracker.FeatureAvailabilityViolations.TransFeature <- Tracker.FeatureAvailabilityViolations.TransFeature %>%
                                                                            mutate(.HasBeenRemoved = TRUE)

                  # Modify REPORT SUMMARY after executed removal of records with trans-feature availability violations
                  Report.FeatureAvailabilityViolations.TransFeature <- Report.FeatureAvailabilityViolations.TransFeature %>%
                                                                            mutate(ProcessExecution = "Removal",
                                                                                   CountRecords.Removed = CountRecords.Affected,
                                                                                   Message = paste0("Trans-feature availability violations: Detected and removed ", CountRecords.Removed, " records belonging to ", CountRootSubjects.Affected, " root subjects."),
                                                                                   MessageClass = "Success",
                                                                                   Timestamp = Sys.time())

                  # Modify REPORT DETAILS after executed removal of records with trans-feature availability violations
                  ReportDetails.FeatureAvailabilityViolations.TransFeature <- ReportDetails.FeatureAvailabilityViolations.TransFeature %>%
                                                                                  mutate(ProcessExecution = "Removal",
                                                                                         CountRecords.Removed = CountRecords.Affected,
                                                                                         Message = paste0("Trans-feature availability violations: Detected and removed ", CountRecords.Removed, " records belonging to ", CountRootSubjects.Affected, " root subjects and violating the following trans-feature availability requirements: ", .ViolatedTransFeatureRequirements),
                                                                                         MessageClass = "Success",
                                                                                         Timestamp = Sys.time())
              }

              # Remove special grouping column not needed anymore
              if (length(ReportDetails.FeatureAvailabilityViolations.TransFeature) > 0)
              {
                  ReportDetails.FeatureAvailabilityViolations.TransFeature <- ReportDetails.FeatureAvailabilityViolations.TransFeature %>%
                                                                                  select(-.ViolatedTransFeatureRequirements)
              }
          }

          # Compile 'Report.FeatureAvailabilityViolations'
          if (length(RequiredFeatures) == 0 & length(TransFeatureRequirements) == 0)
          {
              Report.FeatureAvailabilityViolations <- Report.FeatureAvailabilityViolations %>%
                                                          mutate(Message = "Feature availability violations: No requirements found.",
                                                                 MessageClass = "Info")
          } else {

              Report.FeatureAvailabilityViolations <- bind_rows(Report.FeatureAvailabilityViolations.Strict,
                                                                ReportDetails.FeatureAvailabilityViolations.Strict,
                                                                Report.FeatureAvailabilityViolations.TransFeature,
                                                                ReportDetails.FeatureAvailabilityViolations.TransFeature)
          }
      }
  }

  # Remove auxiliary feature '.AuxID'
  Table <- Table %>%
              select(-.AuxID)


# Consolidate reporting objects
#-------------------------------------------------------------------------------
  Report <-  bind_rows(Report.UnlinkedRecords,
                       Report.EmptyStrings,
                       Report.DuplicateRecords,
                       Report.FeatureAvailabilityViolations)


# Row-bind all tracker data.frames into one that contains all nonconforming table records
#-------------------------------------------------------------------------------
  NonconformingRecords <- Tracker.UnlinkedRecords %>%
                              bind_rows(Tracker.DuplicateRecords) %>%
                              bind_rows(Tracker.FeatureAvailabilityViolations.Strict) %>%
                              bind_rows(Tracker.FeatureAvailabilityViolations.TransFeature) %>%
                              { if(".AuxID" %in% names(.)) { select(., -.AuxID) } else {.} }


#-------------------------------------------------------------------------------
  return(list(Table = Table,
              Report = Report,
              NonconformingRecords = NonconformingRecords))
}
