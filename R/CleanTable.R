
#' CleanTable
#'
#' Perform exclusion of invalid table entries.
#'
#' @param Table \code{data.frame} or \code{tibble} - The table object to be cleaned
#' @param TableNameLookup \code{string} - The table name(s) to be looked up in 'FeatureObligations$RuleSet'. Can be a single string or a character vector.
#' @param PrimaryKey \code{character vector} - Name of features that serve as primary key for table
#' @param ForeignKey \code{character vector} - Names of features that serve as foreign key for table (usually primary key of data set 'root subjects')
#' @param PrimaryKeyIgnoredInRedundancyCheck \code{logical} - Indicating whether primary key feature has no semantic meaning and can be ignored when determining redundancy - Default: \code{TRUE}
#' @param RemoveEmptyStrings \code{logical} - Whether empty strings ('') and strings containing only white space should be removed and replaced by \code{NA} - Default: \code{TRUE}
#' @param RemoveDuplicateEntries \code{logical} - Whether duplicate entries should be removed
#' @param FeatureObligations \code{list}
#'            \itemize{\item \code{RuleSet} - \code{data.frame}
#'                     \item \code{RuleSet.Profile} - \code{character} - Profile name defining strict and trans-feature rules for obligatory feature content. Profile name must be stated in \code{FeatureObligations$RuleSet}}
#' @param ObeyFeatureObligations \code{logical}
#' @param ObeyTransFeatureObligations \code{logical}
#'
#' @return \code{tibble} - Clean table
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CleanTable <- function(Table,
                       TableNameLookup,
                       PrimaryKey,
                       ForeignKey = NULL,
                       PrimaryKeyIgnoredInRedundancyCheck = TRUE,
                       RemoveEmptyStrings = TRUE,
                       RemoveDuplicateEntries = TRUE,
                       FeatureObligations = NULL,
                       ObeyFeatureObligations = TRUE,
                       ObeyTransFeatureObligations = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- DataSetRoot
  # TableNameLookup <- c("Diagnosis", "Patient")
  # PrimaryKey <- c("DiagnosisID", "PatientID")
  # ForeignKey <- c("DiagnosisID", "PatientID")
  # PrimaryKeyIgnoredInRedundancyCheck <- FALSE
  # RemoveEmptyStrings <- TRUE
  # RemoveDuplicateEntries <- TRUE
  # FeatureObligations <- Settings$FeatureObligations
  # ObeyFeatureObligations <- TRUE
  # ObeyTransFeatureObligations <- TRUE


  # --- Argument Validation ---
  assert_that(is.data.frame(Table),
              is.character(TableNameLookup),
              is.character(PrimaryKey),
              is.flag(PrimaryKeyIgnoredInRedundancyCheck),
              is.flag(RemoveEmptyStrings),
              is.flag(RemoveDuplicateEntries),
              is.flag(ObeyFeatureObligations),
              is.flag(ObeyTransFeatureObligations))
  stopifnot("ERROR: 'PrimaryKey' must contain column names of 'Table'." = (PrimaryKey %in% names(Table)))
  if (length(ForeignKey) > 0) { assert_that(is.character(ForeignKey))
                                stopifnot("ERROR: 'ForeignKey' must contain column names of 'Table'." = (all(ForeignKey %in% names(Table)))) }
  if (!is.null(FeatureObligations)) { assert_that(is.list(FeatureObligations))
                                      stopifnot("ERROR: Value of argument 'FeatureObligations$RuleSet.Profile' must be a column name of the data.frame passed in argument 'FeatureObligations$RuleSet'." = (FeatureObligations$RuleSet.Profile %in% names(FeatureObligations$RuleSet))) }

#-------------------------------------------------------------------------------

  # If no 'ForeignKey' is passed, set the variable equal to 'PrimaryKey' (this ensures syntactic validity throughout function proceedings)
  if (length(ForeignKey) == 0) { ForeignKey <- PrimaryKey }

  # --- Initialize Tracker and Reporting objects ---
  Tracker.EmptyStrings <- NULL
  Tracker.ObligatoryFeatures <- NULL
  Tracker.DuplicateEntries <- NULL
  Tracker.TransFeatureObligations <- NULL

  # Create report tibbles containing default column values
  Report.EmptyStrings <- tibble(ProcessTopic = "Empty Strings",
                                ProcessExecuted = FALSE,
                                ReportType = "Message",
                                DetailsGroup = NA_character_,
                                CountRootSubjects = NA_integer_,
                                CountEntries = NA_integer_,
                                Message = "Empty Strings: Omitted detection and removal.")

  Report.DuplicateEntries <- tibble(ProcessTopic = "Duplicate entries",
                                    ProcessExecuted = FALSE,
                                    ReportType = "Message",
                                    DetailsGroup = NA_character_,
                                    CountRootSubjects = NA_integer_,
                                    CountEntries = NA_integer_,
                                    Message = "Duplicated entries: Omitted detection and removal.")

  Report.ObligatoryFeatures <- tibble(ProcessTopic = "Missing obligatory features",
                                      ProcessExecuted = FALSE,
                                      ReportType = "Message",
                                      DetailsGroup = NA_character_,
                                      CountRootSubjects = NA_integer_,
                                      CountEntries = NA_integer_,
                                      Message = "Missing obligatory features: Omitted detection and removal of ineligible entries.")

  Report.TransFeatureObligations <- tibble(ProcessTopic = "Trans-feature obligations",
                                           ProcessExecuted = FALSE,
                                           ReportType = "Message",
                                           DetailsGroup = NA_character_,
                                           CountRootSubjects = NA_integer_,
                                           CountEntries = NA_integer_,
                                           Message = "Trans-feature obligations: Omitted detection and removal of ineligible entries.")


# 1) Remove empty strings in character features (replace them with NAs)
#-------------------------------------------------------------------------------
  if (RemoveEmptyStrings == TRUE)
  {
      # For TRACKING purposes: Count entries that contain empty strings and count times empty strings occur
      Tracker.EmptyStrings <- Table %>%
                                  filter(if_any(where(is.character),
                                                ~ str_trim(.x) == "")) %>%      # This detects all strings that are empty ('') or contain only white space
                                  mutate(.CountEmptyStrings = rowSums(across(where(is.character),
                                                                             ~ .x == ""))) %>%
                                  select(all_of(ForeignKey),
                                         .CountEmptyStrings)

      # PERFORM replacement of empty strings with NA values
      Table <- Table %>%
                    mutate(across(where(is.character),
                                  ~ case_when(str_trim(.x) == "" ~ NA_character_,      # This detects all strings that are empty ('') or contain only white space
                                              .default = .x)))

      # Create REPORT on substitution of empty strings
      Report.EmptyStrings <- Tracker.EmptyStrings %>%
                                  summarize(ProcessTopic = "Empty strings",
                                            ProcessExecuted = TRUE,
                                            ReportType = "Summary",
                                            DetailsGroup = NA_character_,
                                            CountRootSubjects = n_distinct(across(all_of(ForeignKey))),
                                            CountEntries = n(),
                                            Message = paste0("Empty Strings: Found a total of ", sum(.CountEmptyStrings, na.rm = TRUE), " empty strings in ", CountEntries, " entries belonging to ", CountRootSubjects, " root subjects."))
  }


# 2) Remove duplicate entries (optional)
#-------------------------------------------------------------------------------

  if (RemoveDuplicateEntries == TRUE)
  {
      # For TRACKING purposes: Identify duplicate entries and count them
      Tracker.DuplicateEntries <- Table %>%
                                      mutate(.IsDuplicate = ifelse(PrimaryKeyIgnoredInRedundancyCheck == TRUE,      # duplicated() marks all duplicate entries EXCEPT the first occurrence of duplicate entries
                                                                   duplicated(across(-all_of(PrimaryKey))),      # marks rows that are a duplicate in all feature but the 'PrimaryKey' feature
                                                                   duplicated(.))) %>%
                                      filter(.IsDuplicate == TRUE) %>%
                                      select(all_of(ForeignKey),
                                             .IsDuplicate)

      # PERFORM removal of duplicate entries (only first occurrence is kept)
      if (PrimaryKeyIgnoredInRedundancyCheck == TRUE)
      {
          Table <- Table %>%
                      distinct(across(-all_of(PrimaryKey)), .keep_all = TRUE)

      } else {

          Table <- Table %>%
                      distinct()
      }

      # Create REPORT on removal of duplicate entries
      Report.DuplicateEntries <- Tracker.DuplicateEntries %>%
                                      summarize(ProcessTopic = "Duplicate entries",
                                                ProcessExecuted = TRUE,
                                                ReportType = "Summary",
                                                DetailsGroup = NA_character_,
                                                CountRootSubjects = n_distinct(across(all_of(ForeignKey))),
                                                CountEntries = n(),
                                                Message = paste0("Duplicate entries: Removed ", CountEntries, " duplicate entries belonging to ", CountRootSubjects, " root subjects."))

      if (Report.DuplicateEntries$CountEntries > 0)
      {
          Details.DuplicateEntries <- Tracker.DuplicateEntries %>%
                                          group_by(across(all_of(ForeignKey))) %>%
                                              summarize(.Group.CountDuplicateEntries = n(),
                                                        .CountDuplicateEntries = n()) %>%
                                          ungroup() %>%
                                          group_by(.Group.CountDuplicateEntries) %>%
                                              summarize(CountRootSubjects = n(),
                                                        CountEntries = sum(.CountDuplicateEntries)) %>%
                                          ungroup() %>%
                                          mutate(ProcessTopic = "Duplicate entries",
                                                 ProcessExecuted = TRUE,
                                                 ReportType = "Details",
                                                 DetailsGroup = paste0(.Group.CountDuplicateEntries, " duplicate entries"),
                                                 Message = paste0("Duplicate entries: ", CountRootSubjects, " root subjects had ", .CountDuplicateEntries, " duplicate entries.")) %>%
                                          select(-.CountDuplicateEntries)

          Report.DuplicateEntries <- Report.DuplicateEntries %>%
                                          bind_rows(Details.DuplicateEntries)
      }
  }


# 3) Remove entries that have missing values in strictly obligatory features
#-------------------------------------------------------------------------------

  if (ObeyFeatureObligations == TRUE)
  {
      ObligatoryFeatures <- NULL

      if (!is.null(FeatureObligations$RuleSet))
      {
          # Get table's set of (strictly) obligatory features from meta data passed to function
          ObligatoryFeatures <- FeatureObligations$RuleSet %>%
                                    rename(Rule = all_of(FeatureObligations$RuleSet.Profile)) %>%      # Renaming feature based on passed argument
                                    filter(Table %in% TableNameLookup, Rule == "Obligatory") %>%
                                    pull(Feature)
      }

      if (length(ObligatoryFeatures > 0))
      {
          # For TRACKING purposes: Track which entries have missing values in obligatory features
          Tracker.ObligatoryFeatures <- Table %>%
                                            filter(if_any(all_of(ObligatoryFeatures),
                                                          ~ is.na(.x))) %>%
                                            mutate(.MissingObligatoryFeatures = pmap_chr(select(., all_of(ObligatoryFeatures)),
                                                                                         ~ paste(ObligatoryFeatures[is.na(c(...))], collapse = " / "))) %>%      # This saves names of missing obligatory features in a list-column of character vectors
                                            select(all_of(ForeignKey),
                                                   .MissingObligatoryFeatures)

          # PERFORM removal of entries that have missing values in any of strictly obligatory features
          Table <- Table %>%
                      filter(if_all(all_of(ObligatoryFeatures),
                                    ~ !is.na(.x)))

          # Create REPORT on entries that were removed because of missing obligatory features
          Report.ObligatoryFeatures <- Tracker.ObligatoryFeatures %>%
                                            summarize(ProcessTopic = "Missing obligatory features",
                                                      ProcessExecuted = TRUE,
                                                      ReportType = "Summary",
                                                      DetailsGroup = NA,
                                                      CountRootSubjects = n_distinct(across(all_of(ForeignKey))),
                                                      CountEntries = n()) %>%
                                            mutate(Message = paste0("Missing obligatory features: Removed ", CountEntries, " entries belonging to ", CountRootSubjects, " root subjects."))

          if (Report.ObligatoryFeatures$CountEntries > 0)
          {
              Details.ObligatoryFeatures <- Tracker.ObligatoryFeatures %>%
                                                group_by(.MissingObligatoryFeatures) %>%
                                                    summarize(CountRootSubjects = n_distinct(across(all_of(ForeignKey))),
                                                              CountEntries = n()) %>%
                                                ungroup() %>%
                                                mutate(ProcessTopic = "Missing obligatory features",
                                                       ProcessExecuted = TRUE,
                                                       ReportType = "Details",
                                                       DetailsGroup = paste0("Missing obligatory features: ", .MissingObligatoryFeatures),
                                                       Message = paste0("Missing obligatory features: ", CountEntries, " entries belonging to ", CountRootSubjects, " root subjects had missing values in the following obligatory features: ", .MissingObligatoryFeatures)) %>%
                                                select(-.MissingObligatoryFeatures)

              Report.ObligatoryFeatures <- Report.ObligatoryFeatures %>%
                                                bind_rows(Details.ObligatoryFeatures)
          }

      } else {

          Report.ObligatoryFeatures$Message <- "Missing obligatory features: No obligation rules found."
      }
  }


# 4) Remove entries that breach special trans-feature obligations
#-------------------------------------------------------------------------------

  if (ObeyTransFeatureObligations == TRUE)
  {
      TransFeatureObligations <- NULL

      if (!is.null(FeatureObligations$RuleSet))
      {
          # Get current table's set of trans-feature obligation rules (stated as pseudo-code), if there are any defined in meta data passed to function
          TransFeatureObligations <- FeatureObligations$RuleSet %>%
                                          rename(Rule = all_of(FeatureObligations$RuleSet.Profile)) %>%
                                          filter(Table %in% TableNameLookup, !is.na(Rule), Rule != "NA", Rule != "Obligatory") %>%
                                          distinct(Rule) %>%
                                          pull()
      }

      # If there are any of these rules for the current table, filter out entries that breach them
      if (length(TransFeatureObligations) > 0)
      {
          # Set names for rule vector. These names will serve as column names later on (induced by a mutate statement).
          names(TransFeatureObligations) <- paste0("TransFeatureObligation_", 1:length(TransFeatureObligations))

          # Compile list of evaluable expressions for subsequent mutate statement
          TransFeatureObligations.Expr <- TransFeatureObligations %>%
                                              CompileTransFeatureObligations() %>%      # ... use CompileTransFeatureObligations() to turn the pseudo-code into evaluable strings...
                                              lapply(., rlang::parse_expr) %>%      # ... then transform them into a list of expressions
                                              setNames(names(TransFeatureObligations))      # The keys of the list will be the column names of auxiliary features used to determine whether entries are consistent with rules (s. proceedings)

          # For TRACKING purposes: Track breaching of trans-feature rules
          Tracker.TransFeatureObligations <- Table %>%
                                                mutate(!!!TransFeatureObligations.Expr) %>%      # Use list of expressions created earlier to apply trans-feature rules to all entries...
                                                filter(if_any(all_of(names(TransFeatureObligations)), ~ .x == FALSE)) %>%
                                                mutate(.BreachedTransFeatureObligations = pmap_chr(select(., all_of(names(TransFeatureObligations))),
                                                                                           ~ paste(TransFeatureObligations[names(TransFeatureObligations)[c(...) == FALSE]], collapse = " / "))) %>%      # This returns a list-column with character vectors containing trans-feature rules as pseudo-code
                                                select(all_of(ForeignKey),
                                                       .BreachedTransFeatureObligations)

          # PERFORM exclusion of entries that are not compliant with trans-feature rules
          Table <- Table %>%
                      mutate(!!!TransFeatureObligations.Expr) %>%      # Use list of expressions created earlier to apply trans-feature rules to all entries...
                      filter(if_all(all_of(names(TransFeatureObligations)), ~ .x == TRUE)) %>%      # ... and filter out any entries that are not consistent with those rules
                      select(-all_of(names(TransFeatureObligations)))      # Remove auxiliary columns

          # Create REPORT on entries removed because of non-compliance with trans-feature rules
          if (length(Tracker.TransFeatureObligations) > 0)
          {
              Report.TransFeatureObligations <- Tracker.TransFeatureObligations %>%
                                                    summarize(ProcessTopic = "Trans-feature obligations",
                                                              ProcessExecuted = TRUE,
                                                              ReportType = "Summary",
                                                              DetailsGroup = NA_character_,
                                                              CountRootSubjects = n_distinct(across(all_of(ForeignKey))),
                                                              CountEntries = n()) %>%
                                                    mutate(Message = paste0("Breached trans-feature obligations: Removed ", CountEntries, " entries belonging to ", CountRootSubjects, " root subjects."))

              if (Report.TransFeatureObligations$CountEntries > 0)
              {
                  Details.TransFeatureObligations <- Tracker.TransFeatureObligations %>%
                                                          group_by(.BreachedTransFeatureObligations) %>%
                                                              summarize(CountRootSubjects = n_distinct(across(all_of(ForeignKey))),
                                                                        CountEntries = n()) %>%
                                                          ungroup() %>%
                                                          mutate(ProcessTopic = "Trans-feature obligations",
                                                                 ProcessExecuted = TRUE,
                                                                 ReportType = "Details",
                                                                 DetailsGroup = paste0("Breached trans-feature obligations: ", .BreachedTransFeatureObligations),
                                                                 Message = paste0("Breached trans-feature obligations: ", CountEntries, " entries belonging to ", CountRootSubjects, " root subjects breached the following trans-feature obligations: ", .BreachedTransFeatureObligations)) %>%
                                                          select(-.BreachedTransFeatureObligations)

                  Report.TransFeatureObligations <- Report.TransFeatureObligations %>%
                                                        bind_rows(Details.TransFeatureObligations)
              }
          }

      } else {

          Report.TransFeatureObligations$Message <- "Trans-feature obligations: No obligation rules found."
      }
  }


#-------------------------------------------------------------------------------

  Report <-  bind_rows(Report.EmptyStrings,
                       Report.DuplicateEntries,
                       Report.ObligatoryFeatures,
                       Report.TransFeatureObligations)


# Create data.frame containing all root subjects affected by entry removals
#-------------------------------------------------------------------------------
  AffectedRootSubjects <- Tracker.DuplicateEntries %>%
                              bind_rows(Tracker.ObligatoryFeatures) %>%
                              bind_rows(Tracker.TransFeatureObligations)%>%
                              distinct(across(all_of(ForeignKey))) %>%
                              mutate(AffectedByDataRemoval = TRUE)


#-------------------------------------------------------------------------------
  return(list(Table = Table,
              Report = Report,
              AffectedRootSubjects = AffectedRootSubjects))
}
