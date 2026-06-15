
#' CurateDataDS
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Transforms Raw Data Set (RDS) into Curated Data Set (CDS) while tracking data set transformation.
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{character} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#' @param Module.S \code{string} - Identifying a registered FREDA module. Meta data and processing settings are obtained from the corresponding installed package.
#' @param Profile.CurationProcess.S \code{string} - "Default"
#' @param Profile.DataRemediation.S \code{string} - "Default"
#' @param Profile.TransformativeExpressions.S \code{string} - "Default"
#' @param Profile.Dictionary.S \code{string} - "Default"
#' @param Profile.FuzzyStringMatching.S \code{string} - "Default"
#' @param Profile.FeatureRequirements.S \code{string} - "Default"
#' @param Profile.FeatureTracking.S \code{string} - "Default"
#' @param Profile.PrimaryTableCleaning.S \code{string} - "Default"
#' @param Profile.RecordSubsumption.S \code{string} - "Default"
#' @param Profile.SecondaryTableCleaning.S \code{string} - "Default"
#' @param Profile.TableNormalization.S \code{string} - "Default"
#'
#' @return A \code{list} containing the following objects:
#'         \itemize{\item DataSet \code{list}
#'                  \item NonconformingRecords \code{list}
#'                  \item AffectedRootSubjects \code{list}
#'                      \itemize{ \item HadNonconformingRecords
#'                                \item HadRemovedRecords }
#'                  \item Report \code{list}
#'                      \itemize{ \item Settings
#'                                \item Process
#'                                \item Log
#'                                \item Counter
#'                                    \itemize{ \item Summary
#'                                                  \itemize{ \item DataSetLevel
#'                                                            \item TableLevel }
#'                                              \item Extensive
#'                                                  \itemize{ \item StageLevel
#'                                                            \item Details }}
#'                                \item DataHarmonization
#'                                    \itemize{ \item Reports
#'                                                  \itemize{ \item DataSetLevel
#'                                                            \item TableLevel
#'                                                            \item FeatureLevel
#'                                                            \item ValueLevel
#'                                                            \item ValueSets }
#'                                              \item Monitors
#'                                                  \itemize{ \item TableLevel
#'                                                            \item FeatureLevel
#'                                                            \item ValueLevel }}}
#'                  \item Messages \code{list}}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CurateDataDS <- function(RawDataSetName.S = "RawDataSet",
                         Module.S,
                         Profile.CurationProcess.S = "Default",
                         Profile.DataRemediation.S = "Default",
                         Profile.Dictionary.S = "Default",
                         Profile.FeatureRequirements.S = "Default",
                         Profile.FeatureTracking.S = "Default",
                         Profile.FuzzyStringMatching.S = "Default",
                         Profile.PrimaryTableCleaning.S = "Default",
                         Profile.RecordSubsumption.S = "Default",
                         Profile.SecondaryTableCleaning.S = "Default",
                         Profile.TableNormalization.S = "Default",
                         Profile.TransformativeExpressions.S = "Default")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{

#===============================================================================
# - OVERVIEW -
#===============================================================================
#
#   SETUP
#
#   MODULE A)  STRUCTURAL HARMONIZATION
#     1)  Transform table names
#     2)  Add empty tables in data set if they are missing in raw data
#     3)  Rename features
#     4)  In tables with missing features, add empty features accordingly
#
#   MODULE B)  DATA SET CHECKS
#
#   MODULE C)  PRIMARY TABLE CLEANING
#     1)  Remove records that are not linked to related tables
#     2)  Remove duplicate records
#     3)  Remove records in RDS missing required features (defined in meta data or passed as optional argument)
#
#   MODULE D)  TABLE NORMALIZATION
#     1)  Separate or merge features horizontally or vertically (e.g. 'Split and expand' etc.)
#
#   MODULE E)  DATA HARMONIZATION
#     2) Definition of features to monitor during Transformation
#     3) Tracking of raw feature values
#     4) Data Remediation
#     5) Tracking of remediated feature values
#     6) Data recoding
#     7) Tracking of recoded feature values
#     8) Finalize data remediation (Substitution/Removal of ineligible values)
#     9) Data formatting
#     10) Tracking of finalized feature values
#     11) Compilation of monitor objects for reporting
#     12) Perform data harmonization (remediation, recoding, formatting) on non-conforming records
#
#   MODULE F)  SECONDARY TABLE CLEANING
#     1)  Remove duplicate records
#     2)  Remove records in CDS missing required features (defined in meta data or passed as optional argument)
#
#   MODULE G)  RECORD SUBSUMPTION
#     1)  Process data set root tables first, since other tables hold primary key.
#         Any Root subject keys that are removed due to redundancy need to be replaced in dependent tables.
#     2)  Proceed with all other tables
#
#   RETURN list containing
#     - Curated Data Set (list)
#     - Curation Report (list)
#     - Curation Messages (list)
#
#===============================================================================


  # --- For Testing Purposes ---
  # RawDataSetName.S <- "RawDataSet"
  # Module.S <- "CCP"
  # Profile.CurationProcess.S <- "Default"
  # Profile.DataRemediation.S <- "Default"
  # Profile.Dictionary.S <- "Default"
  # Profile.FeatureRequirements.S <- "Default"
  # Profile.FeatureTracking.S <- "Default"
  # Profile.FuzzyStringMatching.S <- "Default"
  # Profile.PrimaryTableCleaning.S <- "Default"
  # Profile.RecordSubsumption.S <- "Default"
  # Profile.SecondaryTableCleaning.S <- "Default"
  # Profile.TableNormalization.S <- "Default"
  # Profile.TransformativeExpressions.S <- "Default"

  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName.S),
              is.string(Module.S),
              is.string(Profile.CurationProcess.S),
              is.string(Profile.DataRemediation.S),
              is.string(Profile.Dictionary.S),
              is.string(Profile.FeatureRequirements.S),
              is.string(Profile.FeatureTracking.S),
              is.string(Profile.FuzzyStringMatching.S),
              is.string(Profile.PrimaryTableCleaning.S),
              is.string(Profile.RecordSubsumption.S),
              is.string(Profile.SecondaryTableCleaning.S),
              is.string(Profile.TableNormalization.S),
              is.string(Profile.TransformativeExpressions.S))

  # Check if 'Module.S' refers to a registered FREDA module
  errormessage <- paste0("ERROR: Value in 'Module.S' is not referring to a registered FREDA module. Possible values are: ", paste0("'", names(dsFreda::Meta.Modules), "'", collapse = ", "), ".")
  if (!(Module.S %in% names(dsFreda::Meta.Modules))) { stop(errormessage) }


#-------------------------------------------------------------------------------
# - OBTAIN MODULE DATA -
#-------------------------------------------------------------------------------
#   Get module META DATA and SETTINGS from corresponding package registered in 'Meta.Modules'
#-------------------------------------------------------------------------------

  # Get correct package name from 'Meta.Modules'
  ModulePackageName <- dsFreda::Meta.Modules[[Module.S]]

  # Get module-specific data set META DATA
  MetaData <- list(Tables = eval(parse(text = paste0(ModulePackageName, "::Meta.Tables"))),
                   Features = eval(parse(text = paste0(ModulePackageName, "::Meta.Features"))),
                   Values = eval(parse(text = paste0(ModulePackageName, "::Meta.Values"))))

  # Map SETTINGS objects to their corresponding profile arguments
  SettingsMapping <- tribble(~ObjectName, ~ProfileArgument,
                             "Set.CurationProcess", "Profile.CurationProcess.S",
                             "Set.DataRemediation", "Profile.DataRemediation.S",
                             "Set.Dictionary", "Profile.Dictionary.S",
                             "Set.FeatureRequirements", "Profile.FeatureRequirements.S",
                             "Set.FeatureTracking", "Profile.FeatureTracking.S",
                             "Set.FuzzyStringMatching", "Profile.FuzzyStringMatching.S",
                             "Set.PrimaryTableCleaning", "Profile.PrimaryTableCleaning.S",
                             "Set.RecordSubsumption", "Profile.RecordSubsumption.S",
                             "Set.SecondaryTableCleaning", "Profile.SecondaryTableCleaning.S",
                             "Proc.TableNormalization", "Profile.TableNormalization.S",
                             "Set.TransformativeExpressions", "Profile.TransformativeExpressions.S")

  # ASSIGN SETTINGS objects and check if passed profile names are eligible
  for (i in 1:nrow(SettingsMapping))
  {
      # Assign Settings objects in function environment
      assign(SettingsMapping$ObjectName[i], eval(parse(text = paste0(ModulePackageName, "::", SettingsMapping$ObjectName[i]))))

      SettingsObjectIsEmpty <- eval(parse(text = paste0("nrow(", SettingsMapping$ObjectName[i], ") == 0")))

      if (SettingsObjectIsEmpty == FALSE)
      {
          PassedProfileName <- eval(parse(text = SettingsMapping$ProfileArgument[i]))
          RegisteredProfiles <- eval(parse(text = paste0("unique(", SettingsMapping$ObjectName[i], "$Profile)")))

          # Check if passed profile name for current settings object is present/registered in settings data.frame
          errormessage <- paste0("Value of argument '", SettingsMapping$ProfileArgument[i], "' does not occur in '", paste0(ModulePackageName, "::", SettingsMapping$ObjectName[i]), "'! Please pass one of the following registered profile names: ", paste0("'", RegisteredProfiles, "'", collapse = ", "), ".")
          if (!(PassedProfileName %in% RegisteredProfiles)) { stop(errormessage) }
      }
  }

  # Filter data.frames in SETTINGS for profile preferences
  Settings <- list(CurationProcess = Set.CurationProcess %>% filter(Profile == Profile.CurationProcess.S),
                   DataRemediation = list(Process = Set.DataRemediation %>% filter(Profile == Profile.DataRemediation.S),
                                          TransformativeExpressions = Set.TransformativeExpressions %>% filter(Profile == Profile.TransformativeExpressions.S),
                                          Dictionary = Set.Dictionary %>% filter(Profile == Profile.Dictionary.S),
                                          FuzzyStringMatching = Set.FuzzyStringMatching %>% filter(Profile == Profile.FuzzyStringMatching.S)),
                   FeatureRequirements = Set.FeatureRequirements %>% filter(Profile == Profile.FeatureRequirements.S),
                   FeatureTracking = Set.FeatureTracking %>% filter(Profile == Profile.FeatureTracking.S),
                   PrimaryTableCleaning = Set.PrimaryTableCleaning %>% filter(Profile == Profile.PrimaryTableCleaning.S),
                   RecordSubsumption = Set.RecordSubsumption %>% filter(Profile == Profile.RecordSubsumption.S),
                   SecondaryTableCleaning = Set.SecondaryTableCleaning %>% filter(Profile == Profile.SecondaryTableCleaning.S),
                   TableNormalization = Proc.TableNormalization %>% filter(Profile == Profile.TableNormalization.S))

  # Set global options
  options(dplyr.summarise.inform = FALSE)      # Suppress summarize info messages
  options(cli.progress_clear = FALSE)      # Whether to clear progress bars from console after finished process
  options(cli.progress_show_after = 0)      # Time after process start after which progress bars should be displayed


#-------------------------------------------------------------------------------
# - Get local data set object -
#-------------------------------------------------------------------------------

  # Parse expression and evaluate
  DataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())


#-------------------------------------------------------------------------------
# - Initiate reporting objects -
#-------------------------------------------------------------------------------

  # Initiate PROCESS report with some initial entries
  Report.Process <- list()
  Report.Process$Curation.Start <- Sys.time()
  Report.Process$Curation.CompletionCheck <- "red"

  # Initiate main LOG report
  Report.Log <- Log.New(ProcessingStage = "General",
                        Message = "Starting Data Curation...",
                        MessageClass = "Special",
                        PrintMessage = TRUE)

#-------------------------------------------------------------------------------


  # Use tryCatch to catch warnings and errors
  # Note: Warnings and errors must be defined and thrown explicitly for this to work. Unspecified errors will not be caught directly but will also not lead to harsh stops.
  #tryCatch({


#===============================================================================
# SETUP:  Processing of data set meta data
#===============================================================================
# Assuming a hierarchical data model, the following terminology is used to describe dependencies in the data set:
#   - 'Root subjects' are the main entities that are described by the data set. There can be more then one class of 'Root subject' in a data set (e.g. 'Patient' or 'Patient + Diagnosis')
#   - 'Seed table' refers to exactly one table that is highest in the hierarchy when it comes to describing the 'Root subjects' (e.g. 'Patient'). It is part of every 'Root'.
#   - 'Root table' refers to tables that contain further data on 'Root subjects' (e.g. 'Diagnosis').
#   - 'Branch table' refers to tables that are lowest in the hierarchy and are directly descended from at least one 'Root' feature
#-------------------------------------------------------------------------------

  # Extracting all table names from meta data
  TableNames <- MetaData$Tables$TableName.Curated

  SeedTableName <- MetaData$Tables %>%
                        filter(Role == "Seed") %>%
                        pull(TableName.Curated)

  stopifnot("ERROR in meta data: There must be exactly one table with role 'Seed'!" = (length(SeedTableName) == 1))

  RootTableNames <- MetaData$Tables %>%
                        filter(Role == "Seed" | Role == "Root") %>%
                        pull(TableName.Curated)

  SeedPrimaryKey <- MetaData$Features %>%
                        filter(TableName.Curated == SeedTableName,
                               IsPrimaryKey == TRUE) %>%
                        pull(FeatureName.Curated)

  RootPrimaryKey <- MetaData$Features %>%
                        filter(TableName.Curated %in% RootTableNames,
                               IsPrimaryKey) %>%
                        pull(FeatureName.Curated)

  BranchTableNames <- MetaData$Tables %>%
                          filter(Role == "Branch") %>%
                          pull(TableName.Curated)

  PrimaryKeys <- TableNames %>%
                      map(function(tablename)
                          {
                              MetaData$Features %>%
                                  filter(TableName.Curated == tablename,
                                         IsPrimaryKey == TRUE) %>%
                                  pull(FeatureName.Curated)
                          }) %>%
                      set_names(TableNames)

  RootSubjectKeys <- TableNames %>%
                          map(function(tablename)
                              {
                                  MetaData$Features %>%
                                      filter(TableName.Curated == tablename,
                                             FeatureName.Curated %in% RootPrimaryKey) %>%
                                      pull(FeatureName.Curated)
                              }) %>%
                          set_names(TableNames)



#===============================================================================
# MODULE A)  Structural Harmonization
#===============================================================================
#   - Add empty tables in data set if they are missing in raw data
#   - Recode feature names according to meta data
#   - Add empty features in case of missing feature names
#   - Remove unknown features
#-------------------------------------------------------------------------------

  # Log report: New processing stage
  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "Structural Harmonization",
                                    Message = "Structural Harmonization",
                                    MessageClass = "Topic",
                                    MessagePriority = 3,
                                    PrintMessage = TRUE))


# If tables are missing, create corresponding empty tables for easier management throughout following processing
#-------------------------------------------------------------------------------

  MissingTableNames <- TableNames[!(TableNames %in% names(DataSet))]

  # Create empty data frames for missing tables
  if (length(MissingTableNames) > 0)
  {
      for (tablename in MissingTableNames)
      {
          DataSet[[tablename]] <- data.frame()

          # Inform log report about missing tables
          Report.Log <- Report.Log %>%
                            Log.Add(Log.New(Table = tablename,
                                            ProcessTopic = "Missing tables",
                                            Message = paste0("Table is missing and will be created as empty data.frame with default features."),
                                            PrintMessage = TRUE))
      }
  }

  # Reestablish original order of tables in 'DataSet' list
  DataSet <- DataSet[TableNames]


# Rename features from harmonized raw feature names to curated feature names
#-------------------------------------------------------------------------------

  # Looping through tables to rename features
  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          # Create named vector to look up matching feature names in meta data ('OldName' = 'NewName')
                          Lookup <- MetaData$Features %>% filter(TableName.Curated == tablename) %>% pull(FeatureName.Raw)
                          names(Lookup) <- MetaData$Features %>% filter(TableName.Curated == tablename) %>% pull(FeatureName.Curated)

                          if (length(Table) > 0)
                          {
                              # Rename feature names according to look-up vector
                              return(Table %>% rename(any_of(Lookup)))      # Returns a tibble

                          } else {

                              # Create empty data.frame with pre-defined column names
                              EmptyTable <- data.frame(matrix(nrow = 0,
                                                              ncol = length(names(Lookup)))) %>%
                                                setNames(names(Lookup)) %>%
                                                mutate(across(everything(), ~ as.character(.x)))

                              return(EmptyTable)
                          }
                       })


# Create list 'NonconformingRecords' containing empty data.frames with the same structure as the DataSet tables
#-------------------------------------------------------------------------------

  NonconformingRecords <- DataSet %>%
                              imap(function(Table, tablename)
                                   {
                                      Table %>% slice(0)
                                   }) %>%
                              c(list(.DataSetRoot = tibble()))


# Add empty features in case of missing feature names and remove unknown existing features
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          # Determine missing and unknown features
                          RequiredFeatureNames <- filter(MetaData$Features, TableName.Curated == tablename)$FeatureName.Curated
                          PresentFeatureNames <- names(Table)
                          MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]
                          UnknownFeatures <- names(Table)[!(names(Table) %in% RequiredFeatureNames)]

                          # If a table misses features, add empty columns accordingly
                          if (length(MissingFeatures) > 0)
                          {
                              Table <- Table %>%
                                            mutate(!!!set_names(rep(list(NA_character_), length(MissingFeatures)), MissingFeatures))

                              # Inform log report about missing features
                              Report.Log <- Report.Log %>%
                                                Log.Add(Log.New(Table = tablename,
                                                                ProcessTopic = "Missing features",
                                                                Message = paste0("Added empty vector for missing features ", paste0("'", MissingFeatures, "'", collapse = ", "), ".")))
                          }

                          # The following effectively removes all unknown features
                          Table <- Table %>%
                                        select(all_of(RequiredFeatureNames))

                          # Print and save messages for removed features
                          if (length(UnknownFeatures) > 0)
                          {
                              # Inform log report about unknown features
                              Report.Log <- Report.Log %>%
                                                Log.Add(Log.New(Table = tablename,
                                                                ProcessTopic = "Unknown features",
                                                                Message = paste0("Removed unknown features ", paste0("'", UnknownFeatures, "'", collapse = ", "), "!"),
                                                                MessageClass = "Warning",
                                                                PrintMessage = TRUE))
                          }

                          return(Table)
                       })


#===============================================================================
# MODULE B)  Data Set Checks
#===============================================================================

  # Log report: New processing stage
  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "Data Set Checks",
                                    Message = "Data Set Checks",
                                    MessageClass = "Topic",
                                    MessagePriority = 3,
                                    PrintMessage = TRUE))

# Check if primary key features of data set tables contain only unique values
#-------------------------------------------------------------------------------

  Check.PrimaryKeyUniqueness <- DataSet %>%
                                    imap(function(Table, tablename)
                                         {
                                            PrimaryKeyVector <- Table[[PrimaryKeys[[tablename]]]]
                                            CheckUniqueness <- length(PrimaryKeyVector) == length(unique(PrimaryKeyVector))
                                            CountRecords.Affected <- 0L
                                            if (CheckUniqueness == FALSE) { CountRecords.Affected <- sum(duplicated(PrimaryKeyVector), na.rm = TRUE) }

                                            Log.New(ProcessingStage = "Data Set Checks",
                                                    Table = tablename,
                                                    ProcessTopic = "Primary key uniqueness",
                                                    ProcessExecution = "Executed",
                                                    Message = ifelse(CheckUniqueness == TRUE,
                                                                     paste0("Check: Primary key values are unique."),
                                                                     paste0("Warning: Primary key values are not unique (", CountRecords.Affected, " duplicates)!")),
                                                    MessageClass = ifelse(CheckUniqueness == TRUE,
                                                                          "Success",
                                                                          "Warning"),
                                                    MessagePriority = ifelse(CheckUniqueness == TRUE,
                                                                             2,
                                                                             3),
                                                    PrintMessage = TRUE)
                                         }) %>%
                                      list_rbind()

  # Inform log report about primary key uniqueness check
  Report.Log <- Report.Log %>%
                    Log.Add(Check.PrimaryKeyUniqueness)



#===============================================================================
# INITIATE COUNTER: Initial record count and creation of reporting object
#===============================================================================

  # Print message to mark reporting of record counts
  PrintSoloMessage(c(Topic = "Initial record counts"))

  # Initiate 'Report.Counter' as data.frame holding table record and root subject counts throughout processing
  Report.Counter <- DataSet %>%
                          imap(function(Table, tablename)
                               {
                                  Table %>%
                                      summarize(CountRecords.Prior = n(),
                                                CountRootSubjects.Prior = n_distinct(pick(RootSubjectKeys[[tablename]])),
                                                CountSeedSubjects.Prior = n_distinct(pick(all_of(SeedPrimaryKey))),
                                                CountRecords.Post = CountRecords.Prior,
                                                CountRootSubjects.Post = CountRootSubjects.Prior,
                                                CountSeedSubjects.Post = CountSeedSubjects.Prior)
                               }) %>%
                          list_rbind(names_to = "Table") %>%
                          mutate(ProcessingStage = "Initial",
                                 ProcessTopic = "COUNT",
                                 CountLevel = "Stage",
                                 Message = paste0(CountRecords.Prior, " records belonging to ", CountRootSubjects.Prior, " <Root subjects> / ", CountSeedSubjects.Prior, " <Seed subjects>."),
                                 MessageClass = "Info",
                                 MessagePriority = 2) %>%
                          Counter.Make()

  # Add initial record and root subject counts to 'Report.Log'
  Report.Log <- Report.Log %>%
                    Log.Add(Log.Make(Report.Counter),
                            PrintMessage = TRUE)



#===============================================================================
# MODULE C)  Primary Table Cleaning
#===============================================================================
#   1) Creating auxiliary data.frame 'DataSetRoot' by linking all tables with Role 'Seed' and 'Root'
#   2) Table cleaning with detection/removal of:
#       - Records that are not linked to any data set root subject
#       - Duplicate records
#       - Records with missing required features (determined in meta data / passed through Settings)
#       - Records that are not consistent with special trans-feature requirement rules (defined in meta data / passed through Settings)
#-------------------------------------------------------------------------------

  # Log: New processing stage
  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "Primary Table Cleaning",
                                    Message = "Primary Table Cleaning",
                                    MessageClass = "Topic",
                                    MessagePriority = 3,
                                    PrintMessage = TRUE))

#===============================================================================
#   MODULE C1)  Create and clean 'DataSetRoot'
#===============================================================================
#     - Create auxiliary data.frame containing all eligible 'Root subjects' by merging 'Seed' table with all 'Root' tables (in case 'Root' does not only consist of 'Seed')
#     - Filter out any record that has missing values in features marked as required in meta data (thereby also removing 'rogue'/unlinked patient or diagnosis records because this way every patient needs to have at least one related diagnosis and vice versa)
#     - Note: Setting 'PrimaryKeyIgnoredInRedundancyCheck' on FALSE is crucial (e.g. because different DiagnosisIDs of same patient and diagnosis can e.g. be related to different Histologies).
#-------------------------------------------------------------------------------

  # The reduce()-call performs pair-wise left-joining of 'Seed' table with all 'Root' tables
  DataSetRoot <- reduce(.x = DataSet[RootTableNames[RootTableNames != SeedTableName]],      # All 'Root' tables that are not 'Seed'
                        .f = \(TableA, TableB) left_join(TableA, TableB, by = SeedPrimaryKey),
                        .init = DataSet[[SeedTableName]])

  # Add initial DataSetRoot COUNTER to Counter report
  Report.Counter <- Report.Counter %>%
                        Counter.Add(Counter.New(ProcessingStage = "Initial",
                                                Table = ".DataSetRoot",
                                                ProcessTopic = "COUNT",
                                                CountLevel = "Stage",
                                                CountRecords.Prior = nrow(DataSetRoot),
                                                CountRootSubjects.Prior = n_distinct(DataSetRoot[RootPrimaryKey]),
                                                CountSeedSubjects.Prior = n_distinct(DataSetRoot[SeedPrimaryKey]),
                                                CountRecords.Post = nrow(DataSetRoot),
                                                CountRootSubjects.Post = n_distinct(DataSetRoot[RootPrimaryKey]),
                                                CountSeedSubjects.Post = n_distinct(DataSetRoot[SeedPrimaryKey]),
                                                Message = "DataSetRoot: Initial record and subject count",
                                                MessageClass = "Info",
                                                MessagePriority = 2,
                                                PrintMessage = FALSE))

  # Clean DataSetRoot
  if ((Settings$CurationProcess %>% filter(Table == ".DataSetRoot") %>% pull(PrimaryTableCleaning)) == TRUE)
  {
      # dsFreda::CleanTable() returns a list object with the elements 'Table', 'NonconformingRecords', 'Counter' and 'Log'
      PrimaryTableCleaning.DataSetRoot <- dsFreda::CleanTable(Table = DataSetRoot,
                                                              TableName = ".DataSetRoot",
                                                              PrimaryKey = RootPrimaryKey,
                                                              RootSubjectKey = RootPrimaryKey,
                                                              SeedSubjectKey = SeedPrimaryKey,
                                                              PrimaryKeyIgnoredInRedundancyCheck = FALSE,      # This setting is important because it considers the semantic meaning of root primary keys (e.g. two different DiagnosisIDs of the same patient should stay truly distinct, because they can be related to different diagnostic procedures)
                                                              EmptyStrings.Detect = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(EmptyStrings.Detect),
                                                              EmptyStrings.Substitute = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(EmptyStrings.Substitute),
                                                              EmptyStrings.Substitution = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(EmptyStrings.Substitution),
                                                              DuplicateRecords.Detect = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(DuplicateRecords.Detect),
                                                              DuplicateRecords.Remove = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(DuplicateRecords.Remove),
                                                              FeatureRequirements = Settings$FeatureRequirements %>% filter(Table %in% RootTableNames),
                                                              FeatureAvailabilityViolations.Detect = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(FeatureAvailabilityViolations.Detect),
                                                              FeatureAvailabilityViolations.Remove = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(FeatureAvailabilityViolations.Remove),
                                                              PrintMessages = TRUE)

      # Assess new .DataSetRoot record and root/seed subject counts
      StageCounter.DataSetRoot <-  dsFreda::TrackCounts(DataSet.Prior = list(.DataSetRoot = DataSetRoot),
                                                        TransformationReturn = list(.DataSetRoot = PrimaryTableCleaning.DataSetRoot),
                                                        RootSubjectKeys = list(.DataSetRoot = RootPrimaryKey),
                                                        SeedSubjectKey = SeedPrimaryKey,
                                                        PrintMessages = FALSE) %>%
                                        mutate(ProcessingStage = "PRE Primary Table Cleaning",
                                               ProcessTopic = "COUNT",
                                               CountLevel = "Monitor")

      # Update COUNTER report
      Report.Counter <- Report.Counter %>%
                            bind_rows(PrimaryTableCleaning.DataSetRoot$Counter %>% mutate(ProcessingStage = "Secondary Table Cleaning"),
                                      StageCounter.DataSetRoot)

      # Update LOG report
      Report.Log <- Report.Log %>%
                        bind_rows(PrimaryTableCleaning.DataSetRoot$Log %>% mutate(ProcessingStage = "Secondary Table Cleaning"))

      # Reassign DataSetRoot
      DataSetRoot <- PrimaryTableCleaning.DataSetRoot$Table

      # Save data.frame of non-conforming records in predefined list element
      NonconformingRecords[[".DataSetRoot"]] <- NonconformingRecords[[".DataSetRoot"]] %>%
                                                    bind_rows(PrimaryTableCleaning.DataSetRoot$NonconformingRecords)
  }

  # Select only primary key features
  DataSetRoot <- DataSetRoot %>%
                      select(all_of(RootPrimaryKey)) %>%
                      distinct()


#===============================================================================
#   MODULE C2)  Table Cleaning
#===============================================================================
#     - Looping through all tables in 'DataSet' to perform table cleaning
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  # ProgressBar <- cli::cli_progress_bar(format = "Primary table cleaning: Detecting non-conforming table records...",
  #                                      total = length(DataSet))
#-------------------------------------------------------------------------------

  PrimaryTableCleaning <- DataSet %>%
                              imap(function(Table, tablename)
                                   {
                                      # Check in settings if primary table cleaning should be executed for current table
                                      if ((Settings$CurationProcess %>% filter(Table == tablename) %>% pull(PrimaryTableCleaning)) == FALSE)
                                      {
                                          CurrentTableCleaning <- list(Table = Table,
                                                                       NonconformingRecords = NULL,
                                                                       Counter = NULL,
                                                                       Log = Log.New(ProcessingStage = "Primary Table Cleaning",
                                                                                     Table = tablename,
                                                                                     ProcessTopic = "Table cleaning",
                                                                                     ProcessExecution = "Omitted",
                                                                                     Message = "Omitted.",
                                                                                     MessageClass = "Info",
                                                                                     PrintMessage = TRUE))

                                      # Check if current table is missing or empty
                                      } else if (length(Table) == 0 || nrow(Table) == 0) {

                                          CurrentTableCleaning <- list(Table = Table,
                                                                       NonconformingRecords = NULL,
                                                                       Counter = NULL,
                                                                       Log = Log.New(ProcessingStage = "Primary Table Cleaning",
                                                                                     Table = tablename,
                                                                                     ProcessTopic = "Table cleaning",
                                                                                     ProcessExecution = "Inapplicable",
                                                                                     Message = "Table is missing or empty.",
                                                                                     MessageClass = "Info",
                                                                                     PrintMessage = TRUE))

                                      # Main Case
                                      } else {

                                          # For Root tables 'PrimaryKeyIgnoredInRedundancyCheck' must be set on FALSE (for explanation see section on DataSetRoot above)
                                          PrimaryKeyIgnoredInRedundancyCheck <- TRUE
                                          if (tablename %in% RootTableNames) { PrimaryKeyIgnoredInRedundancyCheck <- FALSE }

                                          # Perform table cleaning (this returns a list object with the elements 'Table', 'NonconformingRecords', 'Counter' and 'Log')
                                          CurrentTableCleaning <- dsFreda::CleanTable(Table = Table,
                                                                                      TableName = tablename,
                                                                                      PrimaryKey = PrimaryKeys[[tablename]],
                                                                                      RootSubjectKey = RootSubjectKeys[[tablename]],
                                                                                      SeedSubjectKey = SeedPrimaryKey,
                                                                                      PrimaryKeyIgnoredInRedundancyCheck = PrimaryKeyIgnoredInRedundancyCheck,
                                                                                      DataSetRoot = DataSetRoot,
                                                                                      UnlinkedRecords.Detect = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(UnlinkedRecords.Detect),
                                                                                      UnlinkedRecords.Remove = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(UnlinkedRecords.Remove),
                                                                                      EmptyStrings.Detect = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Detect),
                                                                                      EmptyStrings.Substitute = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Substitute),
                                                                                      EmptyStrings.Substitution = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Substitution),
                                                                                      DuplicateRecords.Detect = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(DuplicateRecords.Detect),
                                                                                      DuplicateRecords.Remove = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(DuplicateRecords.Remove),
                                                                                      FeatureRequirements = Settings$FeatureRequirements %>% filter(Table == tablename),
                                                                                      FeatureAvailabilityViolations.Detect = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(FeatureAvailabilityViolations.Detect),
                                                                                      FeatureAvailabilityViolations.Remove = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(FeatureAvailabilityViolations.Remove),
                                                                                      PrintMessages = TRUE)

                                          # Add processing stage to COUNTER and LOG
                                          CurrentTableCleaning$Counter <- CurrentTableCleaning$Counter %>%
                                                                              mutate(ProcessingStage = "Primary Table Cleaning")

                                          CurrentTableCleaning$Log <- CurrentTableCleaning$Log %>%
                                                                              mutate(ProcessingStage = "Primary Table Cleaning")
                                      }

                                      return(CurrentTableCleaning)
                                  })
                                  # .progress = list(name = "Primary table cleaning",
                                  #                  type = "iterator"))

  # Update COUNTER report
  Report.Counter <- Report.Counter %>%
                        bind_rows(PrimaryTableCleaning %>%
                                      map(\(CurrentTableCleaning) CurrentTableCleaning$Counter) %>%
                                      list_rbind())

  # Update LOG report
  Report.Log <- Report.Log %>%
                    bind_rows(PrimaryTableCleaning %>%
                                  map(\(CurrentTableCleaning) CurrentTableCleaning$Log) %>%
                                  list_rbind())

  # Print message to mark reporting of record count changes
  PrintSoloMessage(c(Topic = "Record counts after Primary Table Cleaning"))

  # Assess new table record and root subject counts
  StageCounter <- DataSet %>%
                      dsFreda::TrackCounts(TransformationReturn = PrimaryTableCleaning,
                                           RootSubjectKeys = RootSubjectKeys,
                                           SeedSubjectKey = SeedPrimaryKey,
                                           PrintMessages = TRUE) %>%
                      mutate(ProcessingStage = "Primary Table Cleaning",
                             ProcessTopic = "COUNT",
                             CountLevel = "Stage")

  # Update COUNTER report
  Report.Counter <- Report.Counter %>%
                          bind_rows(StageCounter)

  # Reassign DataSet
  DataSet <- PrimaryTableCleaning %>%
                  map(\(CurrentTableCleaning) CurrentTableCleaning$Table)

  # Save non-conforming records
  NonconformingRecords <- NonconformingRecords %>%
                              imap(\(CurrentNonconformingRecords, tablename) bind_rows(CurrentNonconformingRecords, PrimaryTableCleaning[[tablename]]$NonconformingRecords))

#-------------------------------------------------------------------------------
# For COUNTER: Recreate 'DataSetRoot' after Primary Table Cleaning and count records / subjects
#-------------------------------------------------------------------------------

  # Recreate 'DataSetRoot' by pair-wise left-joining of 'Seed' table with all 'Root' tables
  DataSetRoot <- reduce(.x = DataSet[RootTableNames[RootTableNames != SeedTableName]],
                        .f = \(TableA, TableB) left_join(TableA, TableB, by = SeedPrimaryKey),
                        .init = DataSet[[SeedTableName]])

  # Add DataSetRoot COUNTER to Counter report
  Report.Counter <- Report.Counter %>%
                          Counter.Add(Counter.New(ProcessingStage = "Primary Table Cleaning",
                                                  Table = ".DataSetRoot",
                                                  ProcessTopic = "COUNT",
                                                  CountLevel = "Stage",
                                                  CountRecords.Post = nrow(DataSetRoot),
                                                  CountRootSubjects.Post = n_distinct(DataSetRoot[RootPrimaryKey]),
                                                  CountSeedSubjects.Post = n_distinct(DataSetRoot[SeedPrimaryKey]),
                                                  Message = "DataSetRoot Stage count",
                                                  MessageClass = "Info",
                                                  MessagePriority = 3,
                                                  PrintMessage = FALSE))



#===============================================================================
# MODULE D)  Table Normalization
#===============================================================================
#   - Perform procedures like 'split and expand' where necessary (as determined by settings / meta data)
#-------------------------------------------------------------------------------

  # Log: New processing stage
  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "Table Normalization",
                                    Message = "Table Normalization",
                                    MessageClass = "Topic",
                                    MessagePriority = 3,
                                    PrintMessage = TRUE))

  TableNormalization <- DataSet %>%
                            imap(function(Table, tablename)
                                 {
                                    NormalizationRules <- Settings$TableNormalization %>% filter(Table == tablename)

                                    # Check if there are any normalization rules for current table
                                    if (length(NormalizationRules) == 0 || nrow(NormalizationRules) == 0)
                                    {
                                        CurrentTableNormalization <- list(Table = Table,
                                                                          Counter = NULL,
                                                                          Log = Log.New(ProcessingStage = "Table Normalization",
                                                                                        Table = tablename,
                                                                                        ProcessTopic = "Table Normalization",
                                                                                        ProcessExecution = "Inapplicable",
                                                                                        Message = "No procedures provided.",
                                                                                        MessageClass = "Info",
                                                                                        PrintMessage = TRUE))

                                    # Check in settings if normalization procedures should be omitted for current table
                                    } else if ((Settings$CurationProcess %>% filter(Table == tablename) %>% pull(TableNormalization)) == FALSE) {

                                        CurrentTableNormalization <- list(Table = Table,
                                                                          Counter = NULL,
                                                                          Log = Log.New(ProcessingStage = "Table Normalization",
                                                                                        Table = tablename,
                                                                                        ProcessTopic = "Table Normalization",
                                                                                        ProcessExecution = "Omitted",
                                                                                        Message = "Omitted.",
                                                                                        MessageClass = "Info",
                                                                                        PrintMessage = TRUE))

                                    # Check if current table is missing or empty
                                    } else if (length(Table) == 0 || nrow(Table) == 0) {

                                        CurrentTableNormalization <- list(Table = Table,
                                                                          Counter = NULL,
                                                                          Log = Log.New(ProcessingStage = "Table Normalization",
                                                                                        Table = tablename,
                                                                                        ProcessTopic = "Table Normalization",
                                                                                        ProcessExecution = "Omitted",
                                                                                        Message = "Table is missing or empty.",
                                                                                        MessageClass = "Info",
                                                                                        PrintMessage = TRUE))
                                    # Main Case
                                    } else {

                                        # Perform table normalization operations (this returns a list object with the elements 'Table', 'Counter' and 'Log')
                                        CurrentTableNormalization <- Table %>%
                                                                        dsFreda::NormalizeTable(TableName = tablename,
                                                                                                PrimaryKey = PrimaryKeys[[tablename]],
                                                                                                RootSubjectKey = RootSubjectKeys[[tablename]],
                                                                                                SeedSubjectKey = SeedPrimaryKey,
                                                                                                RuleSet = NormalizationRules,
                                                                                                PrintMessages = TRUE)

                                        # Add processing stage to COUNTER and LOG
                                        if (length(CurrentTableNormalization$Counter) > 0)
                                        {
                                            CurrentTableNormalization$Counter <- CurrentTableNormalization$Counter %>%
                                                                                      mutate(ProcessingStage = "Table Normalization")
                                        }

                                        CurrentTableNormalization$Log <- CurrentTableNormalization$Log %>%
                                                                              mutate(ProcessingStage = "Table Normalization")
                                    }

                                    return(CurrentTableNormalization)
                                 })
                                 # .progress = list(name = "Table normalization",
                                 #                  type = "iterator"))

  # Update COUNTER report
  Report.Counter <- Report.Counter %>%
                        bind_rows(TableNormalization %>%
                                      map(\(CurrentTableNormalization) CurrentTableNormalization$Counter) %>%
                                      list_rbind())

  # Update LOG report
  Report.Log <- Report.Log %>%
                    bind_rows(TableNormalization %>%
                                  map(\(CurrentTableNormalization) CurrentTableNormalization$Log) %>%
                                  list_rbind())

  # Print message to mark reporting of record count changes
  PrintSoloMessage(c(Topic = "Record counts after Table Normalization"))

  # Assess new table record and root/seed subject counts
  StageCounter <- DataSet %>%
                      dsFreda::TrackCounts(TransformationReturn = TableNormalization,
                                           RootSubjectKeys = RootSubjectKeys,
                                           SeedSubjectKey = SeedPrimaryKey,
                                           PrintMessages = TRUE) %>%
                      mutate(ProcessingStage = "Table Normalization",
                             ProcessTopic = "COUNT",
                             CountLevel = "Stage")

  # Update COUNTER report
  Report.Counter <- Report.Counter %>%
                        bind_rows(StageCounter)

  # Reassign DataSet
  DataSet <- TableNormalization %>%
                  map(\(CurrentTableNormalization) CurrentTableNormalization$Table)

#-------------------------------------------------------------------------------
# For COUNTER: Recreate 'DataSetRoot' after Table Normalization and count Records and Root / Seed subjects
#-------------------------------------------------------------------------------

  # Recreate 'DataSetRoot' by pair-wise left-joining of 'Seed' table with all 'Root' tables
  DataSetRoot <- reduce(.x = DataSet[RootTableNames[RootTableNames != SeedTableName]],
                        .f = \(TableA, TableB) left_join(TableA, TableB, by = SeedPrimaryKey),
                        .init = DataSet[[SeedTableName]])

  # Add DataSetRoot COUNTER to Counter report
  Report.Counter <- Report.Counter %>%
                        Counter.Add(Counter.New(ProcessingStage = "Table Normalization",
                                                Table = ".DataSetRoot",
                                                ProcessTopic = "COUNT",
                                                CountLevel = "Stage",
                                                CountRecords.Post = nrow(DataSetRoot),
                                                CountRootSubjects.Post = n_distinct(DataSetRoot[RootPrimaryKey]),
                                                CountSeedSubjects.Post = n_distinct(DataSetRoot[SeedPrimaryKey]),
                                                Message = "DataSetRoot Stage count",
                                                MessageClass = "Info",
                                                MessagePriority = 3,
                                                PrintMessage = FALSE))



#===============================================================================
# MODULE E)  Data Harmonization
#===============================================================================
#   The data harmonization process consists of the following steps:
#     - Data remediation
#     - Data recoding
#     - Data formatting
#     - Data remediation finalization
#-------------------------------------------------------------------------------
#   2) Definition of features to monitor during Transformation
#   3) Tracking of raw feature values
#   4) Data remediation
#   5) Tracking of remediated feature values
#   6) Data recoding and formatting
#   7) Tracking of recoded / formatted feature values
#   8) Finalize data remediation (Removal of ineligible/unremediated values)
#   9) Tracking of finalized feature values
#   10) Compilation of monitor objects for reporting
#-------------------------------------------------------------------------------

  # Log report: New processing stage
  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "Data Harmonization",
                                    Message = "Data Harmonization",
                                    MessageClass = "Topic",
                                    MessagePriority = 3,
                                    PrintMessage = TRUE))

#===============================================================================
#   MODULE E 1)  Conversion of all non-numeric and non-logical features into type character prior to definitive formatting
#===============================================================================

  # DataSet <- DataSet %>%
  #                 imap(function(Table, tablename)
  #                      {
  #                         Table <- Table %>%
  #                                     mutate(across(!(where(is.numeric) | where(is.logical)), ~ as.character(.x)))
  #                      })
  #
  # NonconformingRecords <- NonconformingRecords %>%
  #                             imap(function(Table, tablename)
  #                                  {
  #                                     Table <- Table %>%
  #                                                 mutate(across(!(where(is.numeric) | where(is.logical)), ~ as.character(.x)))
  #                                  })


#===============================================================================
#   MODULE E 2)  Definition of tracked features and their sets of eligible values
#===============================================================================
#     - Create meta data on eligible value sets of features to be tracked during Curation process
#     - Element object syntax: List of vectors
#         - Vector names = Name of feature to be monitored during Curation (Transformation)
#         - Vector values = Set of eligible values obtained from meta data / passed rule set
#     - If a feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

  TrackingMetaData <- names(DataSet) %>%
                            map(function(tablename)
                                {
                                    FeaturesToTrack <- Settings$FeatureTracking %>%
                                                              filter(Table == tablename,
                                                                     IsTracked == TRUE) %>%
                                                              pull(Feature)

                                    EligibleValues <- FeaturesToTrack %>%
                                                              map(function(featurename)
                                                                  {
                                                                      Values <- MetaData$Values %>%
                                                                                        filter(Table == tablename,
                                                                                               FeatureName.Curated == featurename) %>%
                                                                                        select(Value.Raw,
                                                                                               Value.Curated)

                                                                      if (nrow(Values) == 0) { return(NULL) } else { return(Values) }

                                                                  }) %>%
                                                              set_names(FeaturesToTrack)

                                    return(EligibleValues)
                                }) %>%
                            set_names(names(DataSet))



#===============================================================================
#   MODULE E 3)  Track feature values of raw data
#===============================================================================
#     - Get unique raw values and their frequencies for monitoring
#     - Copy values of monitored features and mark them with TrackID that has correspondent in actually processed data.frames
#-------------------------------------------------------------------------------

  # Internal guard condition: Make sure two lists have exactly the same element names (aligned in order)
  stopifnot("Internal ERROR: Names of 'TrackingMetaData' and 'DataSet' must be exactly aligned." = (all(names(TrackingMetaData) == names(DataSet))))

  # Initiate list of data.frames containing transformation tracks
  # First step: Store copied raw values of monitored features and mark them with 'TrackID' to track them along transformation process
  TransformationTracks <- map2(.x = DataSet,
                               .y = TrackingMetaData,
                               .f = function(Table, TableTrackingMetaData)
                                    {
                                        if (length(TableTrackingMetaData) == 0)
                                        {
                                            return(data.frame())

                                        } else {

                                            Table %>%
                                                 select(names(TableTrackingMetaData)) %>%
                                                 rename_with(.fn = ~ str_c(., "___Raw"),   # Three underscores for later use in pivot_longer()
                                                             .cols = everything()) %>%
                                                 mutate(.TrackID = row_number(), .before = 1)   # Create TrackID to enable correct mapping and further processing
                                       }
                                    })

  ValueCounts.Raw <- map2(.x = DataSet,
                          .y = TrackingMetaData,
                          .f = function(Table, TableTrackingMetaData)
                               {
                                  Table %>%
                                      dsFreda::TrackValueCounts(FeatureNames = names(TableTrackingMetaData),
                                                                TransformationStage = "Raw") %>%
                                      select(Feature,
                                             Value,
                                             Frequency) %>%
                                      rename(Value.Raw = Value,
                                             Count.Raw = Frequency)
                               })



#===============================================================================
#   MODULE E 4)  Data Remediation
#===============================================================================
#     Step-wise approach incorporating the following methods (feature-specific selection and order defined by passed settings)
#       - Transformative expressions
#       - Dictionary look-up
#       - Fuzzy String Matching
#-------------------------------------------------------------------------------

  DataRemediation <- DataSet %>%
                          imap(function(Table, tablename)
                               {
                                  # 'TrackID' needs to be appended regardless of further proceedings
                                  Table <- Table %>% mutate(.TrackID = row_number())      # Enables tracking of transformation (see above)

                                  # Initiate table-specific log report
                                  TableReport <- NULL

                                  # Get table-specific settings on data remediation process
                                  RemediationProcess <- Settings$DataRemediation$Process %>%
                                                            filter(Table == tablename,
                                                                   RunRemediation == TRUE) %>%
                                                            arrange(RemediationOrder)      # Defines the order in which features within a table are being remediated (this can be relevant in transformative espressions that contain inter-feature dependencies)

                                  # Check if there are any features in current table that should be remediated
                                  if (length(RemediationProcess) == 0 || nrow(RemediationProcess) == 0)
                                  {
                                      TableReport <- Log.New(Table = tablename,
                                                             ProcessTopic = "Data Remediation",
                                                             ProcessExecution = "Inapplicable",
                                                             Message = "No procedures provided.",
                                                             MessageClass = "Info",
                                                             PrintMessage = TRUE)

                                  # Check in settings if normalization procedures should be executed for current table
                                  } else if ((Settings$CurationProcess %>% filter(Table == tablename) %>% pull(DataRemediation)) == FALSE) {

                                      TableReport <- Log.New(Table = tablename,
                                                             ProcessTopic = "Data Remediation",
                                                             ProcessExecution = "Omitted",
                                                             Message = "Omitted",
                                                             MessageClass = "Info",
                                                             PrintMessage = TRUE)

                                  # Check if current table is missing or empty
                                  } else if (length(Table) == 0 || nrow(Table) == 0) {

                                      TableReport <- Log.New(Table = tablename,
                                                             ProcessTopic = "Data Remediation",
                                                             ProcessExecution = "Inapplicable",
                                                             Message = "Table is missing or empty.",
                                                             MessageClass = "Info",
                                                             PrintMessage = TRUE)

                                  # Main Case
                                  } else {

                                      TableReport <- Log.New(Table = tablename,
                                                             ProcessTopic = "Data Remediation",
                                                             ProcessExecution = "Executed",
                                                             Message = "Started data remediation",
                                                             MessageClass = "Info",
                                                             PrintMessage = TRUE)

                                      # Loop through table features that are supposed to be remediated
                                      for (featurename in RemediationProcess$Feature)
                                      {
                                          # Get set of eligible values (raw stage) for current feature
                                          EligibleValueSet.Raw <- MetaData$Values %>%
                                                                      filter(Table == tablename,
                                                                             FeatureName.Curated == featurename) %>%
                                                                      pull(Value.Raw)   # Eligible Values PRIOR to recoding

                                          # Count ineligible values in current feature prior to data remediation
                                          CountIneligibleValues = sum(!is.na(Table[[featurename]]) & (Table[[featurename]] %notin% EligibleValueSet.Raw))

                                          # Check if there are no ineligible feature values needing remediation
                                          if (is.na(CountIneligibleValues) | CountIneligibleValues == 0)
                                          {
                                              # Create feature-specific LOG report
                                              FeatureReport <- Log.New(Table = tablename,
                                                                       ProcessTopic = "Data Remediation",
                                                                       ProcessTopic.Subgroup = featurename,
                                                                       ProcessExecution = "Inapplicable",
                                                                       Message = paste0("Feature '", featurename, "' had no ineligible values to remediate."),
                                                                       MessageClass = "Details.Info",
                                                                       PrintMessage = TRUE)
                                          } else {

                                              # Get data remediation methods for current feature
                                              Methods <- Settings$DataRemediation$Process %>%
                                                              filter(Table == tablename,
                                                                     Feature == featurename) %>%
                                                              as.list()

                                              TransformativeExpressions <- Settings$DataRemediation$TransformativeExpressions %>%
                                                                                filter(Table == tablename,
                                                                                       Feature == featurename)

                                              Dictionary <- Settings$DataRemediation$Dictionary %>%
                                                                filter(Table == tablename,
                                                                       Feature == featurename) %>%
                                                                pull(var = NewValue,
                                                                     name = LookupValue)

                                              FuzzyStringMatching <- Settings$DataRemediation$FuzzyStringMatching %>%
                                                                          filter(Table == tablename,
                                                                                 Feature == featurename) %>%
                                                                          as.list()

                                              # EXECUTE data remediation for current feature
                                              Table[[featurename]] <- dsFreda::RemediateData(Feature = Table[[featurename]],
                                                                                             FeatureName = featurename,
                                                                                             ContextDataFrame = Table,
                                                                                             Methods = Methods,
                                                                                             EligibleValueSet = EligibleValueSet.Raw,
                                                                                             TransformativeExpressions = TransformativeExpressions,
                                                                                             Dictionary = Dictionary,
                                                                                             FuzzyStringMatching = FuzzyStringMatching)

                                              # Create Feature LOG report
                                              FeatureReport <- Log.New(Table = tablename,
                                                                       ProcessTopic = "Data Remediation",
                                                                       ProcessTopic.Subgroup = featurename,
                                                                       ProcessExecution = "Executed",
                                                                       Message = paste0("Feature '", featurename, "': Performed remediation measures on ", CountIneligibleValues ," ineligible values."),
                                                                       MessageClass = "Details.Success",
                                                                       PrintMessage = TRUE)
                                          }

                                          # Add FeatureReport to TableReport
                                          TableReport <- TableReport %>%
                                                              bind_rows(FeatureReport)
                                      }
                                  }

                                  # Complement table-specific log report
                                  TableReport <- TableReport %>%
                                                      mutate(ProcessingStage = "Data Harmonization")

                                  #---------------------------------------------
                                  return(list(Table = Table,
                                              Report = TableReport))
                               })
                               # .progress = list(name = "Transforming ineligible values",
                               #                     type = "iterator"))

  # Reassign DataSet
  DataSet <- DataRemediation %>%
                  map(\(CurrentDataRemediation) CurrentDataRemediation$Table)

  # Extract LOG reports and bind them to main log report
  Report.Log <- Report.Log %>%
                    Log.Add(DataRemediation %>%
                                map(\(CurrentDataRemediation) CurrentDataRemediation$Report) %>%
                                list_rbind())


#===============================================================================
#   MODULE E 5)  Track feature values after Remediation
#===============================================================================

# Map raw values to their remediated state to get transformation tracks
#===============================================================================

  # Internal guard condition: Make sure lists have exactly the same element names (in aligned order)
  stopifnot("Internal ERROR: Names of 'TransformationTracks', 'TrackingMetaData' and 'DataSet' must be exactly aligned." = (all(names(TransformationTracks) == names(DataSet)) && (all(names(TrackingMetaData) == names(DataSet)))))

  TransformationTracks <- pmap(.l = list(TransformationTracks,
                                         DataSet,
                                         TrackingMetaData),
                               .f = function(CurrentTransformationTracks, RemediatedTable, TableTrackingMetaData)
                                    {
                                        if (length(TableTrackingMetaData) == 0)
                                        {
                                            return(data.frame())

                                        } else {

                                            RemediatedValues <- RemediatedTable %>%
                                                                     select(c(".TrackID", names(TableTrackingMetaData))) %>%
                                                                     rename_with(.fn = ~ str_c(., "___Remediated"),   # Three underscores for later use in pivot_longer()
                                                                                 .cols = all_of(names(TableTrackingMetaData)))

                                            CurrentTransformationTracks %>%
                                                left_join(RemediatedValues, by = join_by(.TrackID)) %>%
                                                distinct(pick(contains("___Raw")), .keep_all = TRUE)
                                       }
                                   })


# Get counts of all distinct values in remediated tables
#===============================================================================

  # Internal guard condition: Make sure two lists have exactly the same element names (aligned in order)
  stopifnot("Internal ERROR: Names of 'TrackingMetaData' and 'DataSet' must be exactly aligned." = (names(TrackingMetaData) == names(DataSet)))

  ValueCounts.Remediated <- map2(.x = DataSet,
                                 .y = TrackingMetaData,
                                 .f = function(Table, TableTrackingMetaData)
                                      {
                                          Table %>%
                                              dsFreda::TrackValueCounts(FeatureNames = names(TableTrackingMetaData),
                                                                        TransformationStage = "Remediated") %>%
                                              select(Feature,
                                                     Value,
                                                     Frequency) %>%
                                              rename(Value.Remediated = Value,
                                                     Count.Remediated = Frequency)
                                     })



#===============================================================================
#   MODULE E 6)  Data recoding
#===============================================================================
#     - Recoding data using dsFreda::RecodeData() based on specifications in MetaData$Values
#     - RecodeData() uses a dictionary in the form of a named vector to perform recoding on a target vector
#-------------------------------------------------------------------------------

  DataRecoding <- DataSet %>%
                      imap(function(Table, tablename)
                           {
                              if (length(Table) == 0 || nrow(Table) == 0)
                              {
                                  Report.Recoding <- Log.New(Table = tablename,
                                                             ProcessTopic = "Data recoding",
                                                             ProcessExecution = "Inapplicable",
                                                             Message = "Table is missing or empty.",
                                                             MessageClass = "Info",
                                                             PrintMessage = FALSE)

                              } else {

                                  FeaturesWithValueSets <- MetaData$Values %>%
                                                                filter(Table == tablename) %>%
                                                                pull(FeatureName.Curated) %>%
                                                                unique()

                                  if (length(FeaturesWithValueSets) == 0)
                                  {
                                      Report.Recoding <- Log.New(Table = tablename,
                                                                 ProcessTopic = "Data recoding",
                                                                 ProcessExecution = "Inapplicable",
                                                                 Message = "Table has no recodable features.",
                                                                 MessageClass = "Info",
                                                                 PrintMessage = FALSE)

                                  } else {

                                      RecodingDictionaries <- MetaData$Values %>%
                                                                  filter(Table == tablename) %>%
                                                                  split(.$FeatureName.Curated) %>%      # 'split' is a base function and needs '.$' to address 'FeatureName.Curated'
                                                                  map(\(Values) with(Values, set_names(Value.Curated, Value.Raw)))

                                      for (featurename in FeaturesWithValueSets)
                                      {
                                          Table[[featurename]] <- dsFreda::RecodeData(TargetVector = Table[[featurename]],
                                                                                      Dictionary = RecodingDictionaries[[featurename]])
                                      }

                                      Report.Recoding <- Log.New(Table = tablename,
                                                                 ProcessTopic = "Data recoding",
                                                                 ProcessExecution = "Executed",
                                                                 Message = paste0("Recoded values of features ", paste0("'", FeaturesWithValueSets, "'", collapse = ", "), "."),
                                                                 MessageClass = "Success",
                                                                 PrintMessage = TRUE)
                                  }
                              }

                              # Complement report
                              Report.Recoding <- Report.Recoding %>%
                                                      mutate(ProcessingStage = "Data Harmonization")

                              return(list(Table = Table,
                                          Report = Report.Recoding))
                           })
                           # .progress = list(name = "Data recoding",
                           #                  type = "iterator"))

  # Reassign DataSet
  DataSet <- DataRecoding %>%
                  map(\(CurrentDataRecoding) CurrentDataRecoding$Table)

  # Extract Report data.frames and bind them to main log report
  Report.Log <- Report.Log %>%
                    Log.Add(DataRecoding %>%
                                map(\(CurrentDataRecoding) CurrentDataRecoding$Report) %>%
                                list_rbind())


#===============================================================================
#   MODULE E 7)  Track feature values after Recoding
#===============================================================================

# Map raw values to their recoded state to get transformation tracks
#===============================================================================

  # Internal guard condition: Make sure lists have exactly the same element names (in aligned order)
  stopifnot("Internal ERROR: Names of 'TransformationTracks', 'TrackingMetaData' and 'DataSet' must be exactly aligned." = (all(names(TransformationTracks) == names(DataSet)) && (all(names(TrackingMetaData) == names(DataSet)))))

  TransformationTracks <- pmap(.l = list(TransformationTracks,
                                         DataSet,
                                         TrackingMetaData),
                               .f = function(TableTransformationTracks, RecodedTable, TableTrackingMetaData)
                                    {
                                        if (length(TableTrackingMetaData) == 0)
                                        {
                                            return(data.frame())

                                        } else {

                                            RecodedValues <- RecodedTable %>%
                                                                 select(c(".TrackID", names(TableTrackingMetaData))) %>%
                                                                 rename_with(.fn = ~ str_c(., "___Recoded"),   # Three underscores for later use in pivot_longer()
                                                                             .cols = all_of(names(TableTrackingMetaData)))

                                            TableTransformationTracks %>%
                                                left_join(RecodedValues,
                                                          by = join_by(.TrackID)) %>%
                                                distinct(pick(contains("___Raw")), .keep_all = TRUE)
                                        }
                                   })


# Get counts of all distinct values in recoded data sets
#===============================================================================

  # Internal guard condition: Make sure two lists have exactly the same element names (aligned in order)
  stopifnot("Internal ERROR: Names of 'TrackingMetaData' and 'DataSet' must be exactly aligned." = (all(names(TrackingMetaData) == names(DataSet))))

  ValueCounts.Recoded <- map2(.x = DataSet,
                              .y = TrackingMetaData,
                              .f = function(Table, TableTrackingMetaData)
                                   {
                                      Table %>%
                                         dsFreda::TrackValueCounts(FeatureNames = names(TableTrackingMetaData),
                                                                   TransformationStage = "Recoded") %>%
                                         select(Feature,
                                                Value,
                                                Frequency) %>%
                                         rename(Value.Recoded = Value,
                                                Count.Recoded = Frequency)
                                   })



#===============================================================================
#   MODULE E 8)  Finalize data remediation using dsFreda::FinalizeDataRemediation()
#===============================================================================
#     - (Optional / Default) Exclusion of ineligible data (data that could not be transformed)
#     - (Optional) Conversion to ordered factor
#     - (Optional) Assignment of factor labels   <-- Conversion to factor is put off for now, 02/2024
#     - All predefined information stored in MetaData$Values
#-------------------------------------------------------------------------------

  DataRemediation.Finalization <- DataSet %>%
                                      imap(function(Table, tablename)
                                           {
                                              # Initiate table log report object
                                              Report.Finalization <- Log.New(ProcessingStage = "Data Harmonization",
                                                                             Table = tablename,
                                                                             ProcessTopic = "Finalization",
                                                                             ProcessExecution = "Executed",
                                                                             Message = "Started finalization of data harmonization.",
                                                                             MessageClass = "Info",
                                                                             PrintMessage = TRUE)

                                              # Check if remediation finalization is intended and applicable for current table
                                              if (Settings$CurationProcess %>% filter(Table == tablename) %>% pull(DataRemediation) == TRUE &&
                                                  length(Table) > 0 && nrow(Table) > 0)
                                              {
                                                  # Get features that are supposed to be remediated from settings
                                                  RemediationProcess <- Settings$DataRemediation$Process %>%
                                                                              filter(Table == tablename,
                                                                                     RunRemediation == TRUE)

                                                  if (length(RemediationProcess) > 0 && nrow(RemediationProcess) > 0)
                                                  {
                                                      for (featurename in RemediationProcess$Feature)
                                                      {
                                                          # Initiate feature log report object
                                                          FeatureReport.Log <- tibble()

                                                          # Get eligible value set for current feature as data.frame including data on factoring
                                                          EligibleValueSet <- MetaData$Values %>%
                                                                                  filter(Table == tablename,
                                                                                         FeatureName.Curated == featurename)

                                                          if (nrow(EligibleValueSet) > 0)
                                                          {
                                                              UnremediatedValues.Substitute <- RemediationProcess %>% filter(Feature == featurename) %>% pull(UnremediatedValues.Substitute)
                                                              UnremediatedValues.Substitution <- RemediationProcess %>% filter(Feature == featurename) %>% pull(UnremediatedValues.Substitution)

                                                              Table[[featurename]] <- dsFreda::FinalizeDataRemediation(TargetVector = Table[[featurename]],
                                                                                                                       EligibleValueSet = EligibleValueSet,
                                                                                                                       UnremediatedValues.Substitute = UnremediatedValues.Substitute,
                                                                                                                       UnremediatedValues.Substitution = UnremediatedValues.Substitution)

                                                              FeatureReport.Log <- Log.New(ProcessingStage = "Data Harmonization",
                                                                                           Table = tablename,
                                                                                           ProcessTopic = "Finalization",
                                                                                           ProcessTopic.Subgroup = featurename,
                                                                                           ProcessExecution = "Executed",
                                                                                           Message = ifelse(UnremediatedValues.Substitute == TRUE,
                                                                                                            paste0("Remaining ineligible values of feature '", featurename , "' were substituted for '", UnremediatedValues.Substitution, "'."),
                                                                                                            paste0("Remaining ineligible values of feature '", featurename , "' were left unsubstituted.")),
                                                                                           MessageClass = ifelse(UnremediatedValues.Substitute == TRUE,
                                                                                                                 "Details.Success",
                                                                                                                 "Details.Info"),
                                                                                           PrintMessage = TRUE)
                                                          }

                                                          # Add feature-specific report to table LOG REPORT
                                                          Report.Finalization <- Report.Finalization %>%
                                                                                      bind_rows(FeatureReport.Log)
                                                      }
                                                  }
                                              }
                                              #-------------------------------
                                              return(list(Table = Table,
                                                          Report = Report.Finalization))
                                           })
                                           # .progress = list(name = "Finalizing data remediation",
                                           #                  type = "iterator"))

  # Reassign DataSet
  DataSet <- DataRemediation.Finalization %>%
                  map(\(CurrentDataRemediation.Finalization) CurrentDataRemediation.Finalization$Table)

  # Extract Report data.frames and bind them to main log report
  Report.Log <- Report.Log %>%
                    Log.Add(DataRemediation.Finalization %>%
                                map(\(CurrentDataRemediation.Finalization) CurrentDataRemediation.Finalization$Report) %>%
                                list_rbind())


#===============================================================================
#   MODULE E 9)  Data formatting
#===============================================================================
#     - Format / Re-type data using dsFreda::FormatData() based on specifications in MetaData$Features
#-------------------------------------------------------------------------------

  DataFormatting <- DataSet %>%
                        imap(function(Table, tablename)
                             {
                                if (length(Table) == 0 || nrow(Table) == 0)
                                {
                                    Report.Formatting <- Log.New(Table = tablename,
                                                                 ProcessTopic = "Data formatting",
                                                                 ProcessExecution = "Inapplicable",
                                                                 Message = "Table is missing or empty.",
                                                                 MessageClass = "Info",
                                                                 PrintMessage = FALSE)

                                } else {

                                    FeatureTypes <- MetaData$Features %>%
                                                        filter(TableName.Curated == tablename) %>%
                                                        select(FeatureName.Curated,
                                                               Type) %>%
                                                        rename(Feature = "FeatureName.Curated")

                                    if (nrow(FeatureTypes) > 0)
                                    {
                                        for (i in 1:nrow(FeatureTypes))
                                        {
                                            Table <- Table %>%
                                                          mutate(across(all_of(FeatureTypes$Feature[i]),
                                                                        ~ dsFreda::FormatData(.x, FeatureTypes$Type[i])))
                                        }

                                        Report.Formatting <- Log.New(Table = tablename,
                                                                     ProcessTopic = "Data formatting",
                                                                     ProcessExecution = "Executed",
                                                                     Message = paste0("Formatted table features according to provided meta data."),
                                                                     MessageClass = "Success",
                                                                     PrintMessage = TRUE)
                                    }
                                }

                                # Complement report
                                Report.Formatting <- Report.Formatting %>%
                                                          mutate(ProcessingStage = "Data Harmonization")

                                return(list(Table = Table,
                                            Report = Report.Formatting))
                             })
                             # .progress = list(name = "Data formatting",
                             #                  type = "iterator"))

  # Reassign DataSet
  DataSet <- DataFormatting %>%
                  map(\(CurrentDataFormatting) CurrentDataFormatting$Table)

  # Extract Report data.frames and bind them to main log report
  Report.Log <- Report.Log %>%
                    Log.Add(DataFormatting %>%
                                map(\(CurrentDataFormatting) CurrentDataFormatting$Report) %>%
                                list_rbind())


#===============================================================================
#   MODULE E 10)  Track feature values after finalized Remediation
#===============================================================================

# Map raw values to their finalized state to get transformation tracks
#===============================================================================

  # Internal guard condition: Make sure lists have exactly the same element names (in aligned order)
  stopifnot("Internal ERROR: Names of 'TransformationTracks', 'TrackingMetaData' and 'DataSet' must be exactly aligned." = (all(names(TransformationTracks) == names(DataSet)) && (all(names(TrackingMetaData) == names(DataSet)))))

  TransformationTracks <- pmap(.l = list(TransformationTracks,
                                         DataSet,
                                         TrackingMetaData),
                               .f = function(TableTransformationTracks, FinalizedTable, TableTrackingMetaData)
                                    {
                                        if (length(TableTrackingMetaData) == 0)
                                        {
                                            return(data.frame())

                                        } else {

                                            FinalizedValues <- FinalizedTable %>%
                                                                   select(c(".TrackID", names(TableTrackingMetaData))) %>%
                                                                   rename_with(.fn = ~ str_c(., "___Final"),   # Three underscores for later use in pivot_longer()
                                                                               .cols = all_of(names(TableTrackingMetaData)))

                                            TableTransformationTracks %>%
                                                left_join(FinalizedValues,
                                                          by = join_by(.TrackID)) %>%
                                                distinct(pick(contains("___Raw")), .keep_all = TRUE)
                                        }
                                    })


# Get counts of all distinct values in finalized data sets
#===============================================================================

  # Internal guard condition: Make sure two lists have exactly the same element names (aligned in order)
  stopifnot("Internal ERROR: Names of 'TrackingMetaData' and 'DataSet' must be exactly aligned." = (all(names(TrackingMetaData) == names(DataSet))))

  ValueCounts.Final <- map2(.x = DataSet,
                            .y = TrackingMetaData,
                            .f = function(Table, TableTrackingMetaData)
                                 {
                                     Table %>%
                                        dsFreda::TrackValueCounts(FeatureNames = names(TableTrackingMetaData),
                                                                  TransformationStage = "Final") %>%
                                        select(Feature,
                                               Value,
                                               Frequency) %>%
                                        rename(Value.Final = Value,
                                               Count.Final = Frequency)
                                 })



#===============================================================================
#   MODULE E 11)  Merge monitor objects into coherent summaries
#===============================================================================

# Summarize Transformation Tracks
#===============================================================================
  TransformationTracks.Summaries <- pmap(.l = list(TransformationTracks,
                                                   TrackingMetaData,
                                                   names(TransformationTracks)),
                                         .f = function(TableTransformationTracks,
                                                       TableTrackingMetaData,
                                                       tablename)
                                              {
                                                  if (length(TableTransformationTracks) == 0 || nrow(TableTransformationTracks) == 0)
                                                  {
                                                      return(data.frame())

                                                  } else {

                                                      # Get info about previously performed substitution of ineligible values from remediation settings
                                                      UnremediatedValues.SubstitutionSettings <- Settings$DataRemediation$Process %>%
                                                                                                      filter(Table == tablename,
                                                                                                             RunRemediation == TRUE) %>%
                                                                                                      select(Feature,
                                                                                                             UnremediatedValues.Substitute,
                                                                                                             UnremediatedValues.Substitution)

                                                      Summary <- TableTransformationTracks %>%
                                                                     mutate(across(everything(), ~ as.character(.x))) %>%      # Turn all columns into character (necessary for pivot_longer() to work correctly)
                                                                     tidyr::pivot_longer(cols = c(everything(), -.TrackID),
                                                                                         names_to = c("Feature", "Stage"),
                                                                                         names_sep = "(___)",      # Separate by '___'-string (two underscores)
                                                                                         values_to = "Value") %>%
                                                                     tidyr::pivot_wider(names_from = Stage,
                                                                                        values_from = Value) %>%
                                                                     select(-.TrackID) %>%
                                                                     distinct() %>%
                                                                     rename(Value.Raw = Raw,
                                                                            Value.Remediated = Remediated,
                                                                            Value.Recoded = Recoded,
                                                                            Value.Final = Final) %>%
                                                                     left_join(UnremediatedValues.SubstitutionSettings, by = join_by(Feature)) %>%
                                                                     rowwise() %>%
                                                                     mutate(IsOccurring = TRUE,
                                                                            IsEligible.Raw = ifelse(is.na(Value.Raw) | is.null(TableTrackingMetaData[[Feature]]),      # If value is NA or if there is no set of eligible values, set variable NA...
                                                                                                    NA,
                                                                                                    Value.Raw %in% TableTrackingMetaData[[Feature]]$Value.Raw),      # ... else check if specific row value is in set of eligible values
                                                                            IsEligible.Remediated = ifelse(is.na(Value.Remediated) | is.null(TableTrackingMetaData[[Feature]]),
                                                                                                           NA,
                                                                                                           Value.Remediated %in% TableTrackingMetaData[[Feature]]$Value.Raw),
                                                                            IsEligible.Recoded = ifelse(is.na(Value.Recoded) | is.null(TableTrackingMetaData[[Feature]]),
                                                                                                        NA,
                                                                                                        Value.Recoded %in% TableTrackingMetaData[[Feature]]$Value.Curated),
                                                                            IsEligible.Final = ifelse(is.na(Value.Final) | is.null(TableTrackingMetaData[[Feature]]),
                                                                                                      NA,
                                                                                                      ifelse(UnremediatedValues.Substitute == TRUE & Value.Final == UnremediatedValues.Substitution,
                                                                                                             FALSE,
                                                                                                             TRUE))) %>%
                                                                     ungroup() %>%
                                                                     select(-UnremediatedValues.Substitute,
                                                                            -UnremediatedValues.Substitution)

                                                      # Add set of all eligible values regardless of occurrence to summary
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      for (i in 1:length(TableTrackingMetaData))   # Loop through all monitored features of a table
                                                      {
                                                         AllEligibleValues <- tibble(Feature = names(TableTrackingMetaData)[i],
                                                                                     Value.Raw = TableTrackingMetaData[[i]]$Value.Raw,
                                                                                     IsOccurring = FALSE,
                                                                                     IsEligible.Raw = TRUE)

                                                         Summary <- bind_rows(Summary,
                                                                              AllEligibleValues)
                                                      }

                                                      # Filter out eligible values marked as not occurring if they actually occur
                                                      # Result: All eligible values are included in summary, regardless of occurrence
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      Summary <- Summary %>%
                                                                    group_by(Feature, Value.Raw) %>%
                                                                        arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                                                        slice_head() %>%
                                                                    ungroup() %>%
                                                                    arrange(Feature,
                                                                            desc(IsOccurring),
                                                                            desc(IsEligible.Raw),
                                                                            desc(IsEligible.Remediated),
                                                                            Value.Raw)

                                                      return(Summary)
                                                 }
                                             })


# Create detailed transformation monitors and reports on VALUE-LEVEL
#===============================================================================
#   - Joining of info from transformation tracks and value counts
#-------------------------------------------------------------------------------

  # Create data harmonization MONITOR on VALUE-LEVEL
  Monitor.DataHarmonization.ValueLevel <- pmap(.l = list(TransformationTracks.Summaries,
                                                         ValueCounts.Raw,
                                                         ValueCounts.Remediated,
                                                         ValueCounts.Recoded,
                                                         ValueCounts.Final),
                                               .f = function(TableTransformationTracks.Summary,
                                                             TableValueCounts.Raw,
                                                             TableValueCounts.Remediated,
                                                             TableValueCounts.Recoded,
                                                             TableValueCounts.Final)
                                                    {
                                                       if (nrow(TableTransformationTracks.Summary) == 0)
                                                       {
                                                          return(NULL)

                                                       } else {

                                                           TableTransformationTracks.Summary %>%
                                                               left_join(TableValueCounts.Raw, by = c("Feature", "Value.Raw")) %>%
                                                               left_join(TableValueCounts.Remediated, by = c("Feature", "Value.Remediated")) %>%
                                                               left_join(TableValueCounts.Recoded, by = c("Feature", "Value.Recoded")) %>%
                                                               left_join(TableValueCounts.Final, by = c("Feature", "Value.Final")) %>%
                                                               mutate(Count.Remediated = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                                   .default = Count.Remediated),
                                                                      Count.Recoded = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                                .default = Count.Recoded),
                                                                      Count.Final = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                              .default = Count.Final)) %>%
                                                               arrange(Feature,
                                                                       desc(IsOccurring),
                                                                       desc(IsEligible.Raw),
                                                                       desc(IsEligible.Remediated),
                                                                       Value.Raw)
                                                       }
                                                    })

  # Create data harmonization REPORT on VALUE-LEVEL
  Report.DataHarmonization.ValueLevel <- Monitor.DataHarmonization.ValueLevel %>%
                                              map(function(TableMonitor)
                                                  {
                                                      if (length(TableMonitor) == 0)
                                                      {
                                                          return(data.frame())

                                                      } else {

                                                          return(TableMonitor %>%
                                                                      mutate(NeedsHarmonization = case_when(IsOccurring == TRUE & IsEligible.Raw == FALSE ~ TRUE,
                                                                                                            IsOccurring == TRUE & IsEligible.Raw == TRUE ~ FALSE,
                                                                                                            .default = NA),
                                                                             SuccessfullyHarmonized = case_when(IsOccurring == TRUE & IsEligible.Raw == FALSE & IsEligible.Final == TRUE ~ TRUE,
                                                                                                                IsOccurring == TRUE & IsEligible.Raw == FALSE & IsEligible.Final == FALSE ~ FALSE,
                                                                                                                .default = NA)) %>%
                                                                      select(Feature,
                                                                             Value.Raw,
                                                                             Count.Raw,
                                                                             IsOccurring,
                                                                             IsEligible.Raw,
                                                                             NeedsHarmonization,
                                                                             SuccessfullyHarmonized,
                                                                             Value.Final) %>%
                                                                      rename(c(Count = "Count.Raw",
                                                                               IsEligible = "IsEligible.Raw")))
                                                      }
                                                  })

  # VALUE SETS: Create sets of occurring and eligible values at different data harmonization stages and their frequencies
  Report.DataHarmonization.ValueSets <- Monitor.DataHarmonization.ValueLevel %>%
                                            map(function(TableMonitor)
                                                {
                                                    if (length(TableMonitor) == 0 || nrow(TableMonitor) == 0)
                                                    {
                                                        return(list())

                                                    } else {

                                                        TableMonitor %>%
                                                            pivot_longer(cols = starts_with("Value"),
                                                                         names_to = "Value.HarmonizationStage",
                                                                         values_to = "Value") %>%
                                                            pivot_longer(cols = starts_with("IsEligible"),
                                                                         names_to = "IsEligible.HarmonizationStage",
                                                                         values_to = "IsEligible") %>%
                                                            pivot_longer(cols = starts_with("Count"),
                                                                         names_to = "Count.HarmonizationStage",
                                                                         values_to = "Count") %>%
                                                            mutate(across(contains("HarmonizationStage"), ~ str_remove(.x, "Value.|IsEligible.|Count."))) %>%
                                                            filter(Value.HarmonizationStage == IsEligible.HarmonizationStage & Value.HarmonizationStage == Count.HarmonizationStage) %>%
                                                            select(-IsEligible.HarmonizationStage,
                                                                   -Count.HarmonizationStage) %>%
                                                            rename(HarmonizationStage = "Value.HarmonizationStage") %>%
                                                            distinct() %>%
                                                            arrange(Feature,
                                                                    desc(IsOccurring),
                                                                    desc(IsEligible),
                                                                    desc(Count)) %>%
                                                            relocate(Value, .before = 1) %>%
                                                            split(.$Feature) %>%
                                                                map(\(X) X %>%
                                                                          select(-Feature) %>%
                                                                          split(.$HarmonizationStage) %>%
                                                                              map(\(X) X %>%
                                                                                        select(-HarmonizationStage) %>%
                                                                                        mutate(Proportion = { if (sum(Count, na.rm = TRUE) == 0) { NA } else { Count / sum(Count, na.rm = TRUE) }})))
                                                    }
                                                })


# Create FEATURE-LEVEL overview of value eligibility in different harmonization stages
#-------------------------------------------------------------------------------

  # Create data harmonization MONITOR on FEATURE-LEVEL
  Monitor.DataHarmonization.FeatureLevel <- pmap(.l = list(Monitor.DataHarmonization.ValueLevel,
                                                           TrackingMetaData),
                                                 .f = function(TableMonitor,
                                                               TableTrackingMetaData)
                                                      {
                                                          if (length(TableMonitor) == 0 || nrow(TableMonitor) == 0)
                                                          {
                                                              return(data.frame())

                                                          } else {

                                                              # Filter out features that are not meant to be monitored, e.g. do not have applicable eligibility criteria
                                                              TableMonitor <- TableMonitor %>%
                                                                                  filter(Feature %in% names(TableTrackingMetaData))

                                                              SummaryRaw <- TableMonitor %>%
                                                                                group_by(Feature, IsEligible.Raw) %>%
                                                                                    summarize(Count.Raw = sum(Count.Raw, na.rm = TRUE)) %>%
                                                                                    rename(Eligibility = IsEligible.Raw)

                                                              SummaryRemediated <- TableMonitor %>%
                                                                                        distinct(pick(Feature, Value.Remediated), .keep_all = TRUE) %>%
                                                                                        group_by(Feature, IsEligible.Remediated) %>%
                                                                                            summarize(Count.Remediated = sum(Count.Remediated, na.rm = TRUE)) %>%
                                                                                            rename(Eligibility = IsEligible.Remediated)

                                                              SummaryRecoded <- TableMonitor %>%
                                                                                    distinct(pick(Feature, Value.Recoded), .keep_all = TRUE) %>%
                                                                                    group_by(Feature, IsEligible.Recoded) %>%
                                                                                        summarize(Count.Recoded = sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                                        rename(Eligibility = IsEligible.Recoded)

                                                              SummaryFinal <- TableMonitor %>%
                                                                                  distinct(pick(Feature, Value.Final), .keep_all = TRUE) %>%
                                                                                  group_by(Feature, IsEligible.Final) %>%
                                                                                      summarize(Count.Final = sum(Count.Final, na.rm = TRUE)) %>%
                                                                                      rename(Eligibility = IsEligible.Final)

                                                              Overview <- SummaryRaw %>%
                                                                              full_join(SummaryRemediated, by = join_by(Feature, Eligibility)) %>%
                                                                              full_join(SummaryRecoded, by = join_by(Feature, Eligibility)) %>%
                                                                              full_join(SummaryFinal, by = join_by(Feature, Eligibility)) %>%
                                                                              arrange(Feature, desc(Eligibility)) %>%
                                                                              mutate(Eligibility = case_match(Eligibility,
                                                                                                              TRUE ~ "Eligible",
                                                                                                              FALSE ~ "Ineligible",
                                                                                                              NA ~ "Missing"))

                                                              # Get tibble of all combinations of occurring features and eligibility category
                                                              AllCombinations <- tibble(Feature = rep(unique(Overview$Feature), each = 3),
                                                                                        Eligibility = rep(c("Eligible", "Ineligible", "Missing"), times = length(unique(Overview$Feature))))

                                                              # Finalize overview
                                                              Overview <- Overview %>%
                                                                              right_join(AllCombinations, by = join_by(Feature, Eligibility)) %>%
                                                                              mutate(across(starts_with("Count."), ~ case_when(is.na(.x) ~ 0,      # Turn all NAs into 0 in count columns
                                                                                                                               .default = .x))) %>%
                                                                              group_by(Feature) %>%
                                                                                  mutate(across(starts_with("Count."), ~ { if (sum(.x) == 0) NA else .x / sum(.x) },
                                                                                                .names = "Proportion.{.col}")) %>%      # Create proportional value columns
                                                                                  rename_with(~ str_replace(.x, ".Count.", "."), starts_with("Proportion.")) %>%
                                                                              ungroup() %>%
                                                                              arrange(Feature, factor(Eligibility, levels = c("Eligible", "Ineligible", "Missing")))
                                                          }
                                                      })

  # Create data harmonization REPORT on FEATURE-LEVEL
  Report.DataHarmonization.FeatureLevel <- Monitor.DataHarmonization.FeatureLevel %>%
                                                map(function(TableMonitor)
                                                    {
                                                        if (length(TableMonitor) == 0)
                                                        {
                                                            return(data.frame())

                                                        } else {

                                                            return(TableMonitor %>%
                                                                        group_by(Feature) %>%
                                                                            summarize(CountValues = sum(Count.Raw, na.rm = TRUE),
                                                                                      CountValues.NonMissing = sum(Count.Raw[Eligibility != "Missing"], na.rm = TRUE),
                                                                                      CountValues.Ineligible.Raw = Count.Raw[Eligibility == "Ineligible"],
                                                                                      CountValues.Ineligible.Final = Count.Final[Eligibility == "Ineligible"],
                                                                                      CountValues.Harmonized = CountValues.Ineligible.Raw - CountValues.Ineligible.Final,
                                                                                      ProportionValues.Harmonized = ifelse(is.na(CountValues.Ineligible.Raw) | CountValues.Ineligible.Raw == 0,
                                                                                                                           NA,
                                                                                                                           CountValues.Harmonized / CountValues.Ineligible.Raw)))
                                                        }
                                                    })


# Create TABLE-LEVEL overview of value eligibility in different harmonization stages
#-------------------------------------------------------------------------------

  # Create data harmonization MONITOR on TABLE-LEVEL
  Monitor.DataHarmonization.TableLevel <- Monitor.DataHarmonization.FeatureLevel %>%
                                              map(function(TableMonitor)
                                                  {
                                                      if (length(TableMonitor) == 0 || nrow(TableMonitor) == 0)
                                                      {
                                                          return(data.frame())

                                                      } else {

                                                          TableMonitor %>%
                                                              group_by(Eligibility) %>%
                                                                  summarize(across(starts_with("Count."), ~ sum(.x, na.rm = TRUE))) %>%
                                                              ungroup() %>%
                                                              mutate(across(starts_with("Count."), ~ { if (sum(.x) == 0) NA else .x / sum(.x) },
                                                                            .names = "Proportion.{.col}")) %>%      # Create proportional value columns
                                                              rename_with(~ str_replace(.x, ".Count.", "."), starts_with("Proportion."))
                                                      }
                                                  })

  # Create data harmonization REPORT on TABLE-LEVEL
  Report.DataHarmonization.TableLevel <- Report.DataHarmonization.FeatureLevel %>%
                                              map(function(TableReport)
                                                  {
                                                      if (length(TableReport) == 0)
                                                      {
                                                          return(data.frame())

                                                      } else {

                                                          return(TableReport %>%
                                                                      summarize(CountTrackedFeatures = n(),
                                                                                across(starts_with("CountValues"), ~ sum(.x, na.rm = TRUE))) %>%
                                                                      mutate(ProportionValues.Harmonized = ifelse(is.na(CountValues.Ineligible.Raw) | CountValues.Ineligible.Raw == 0,
                                                                                                                  NA,
                                                                                                                  CountValues.Harmonized / CountValues.Ineligible.Raw)))
                                                      }
                                                  }) %>%
                                              list_rbind(names_to = "Table")


# Create DATA-SET-LEVEL report on Data Harmonization
#-------------------------------------------------------------------------------

  Report.DataHarmonization.DataSetLevel <- Report.DataHarmonization.TableLevel %>%
                                                summarize(CountAffectedTables = n(),
                                                          across(starts_with("Count"), ~ sum(.x, na.rm = TRUE))) %>%
                                                mutate(ProportionValues.Harmonized = ifelse(is.na(CountValues.Ineligible.Raw) | CountValues.Ineligible.Raw == 0,
                                                                                            NA,
                                                                                            CountValues.Harmonized / CountValues.Ineligible.Raw))

# Delete artificial 'TrackID'-column from data.frames (not needed anymore)
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  map(function(Table)
                      {
                          try(Table %>% select(-.TrackID))
                      })



#===============================================================================
#   MODULE E 12)  Perform data harmonization (remediation, recoding, formatting) on data.frames of non-conforming records (without tracking / reporting)
#===============================================================================

  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "Data Harmonization",
                                    ProcessTopic = "Data Harmonization in non-conforming records",
                                    Message = "Starting data harmonization in non-conforming records.",
                                    MessageClass = "Info",
                                    PrintMessage = TRUE,
                                    PrintMessage.Compile = FALSE))

  NonconformingRecords <- NonconformingRecords %>%
                              imap(function(Table, tablename)
                                   {
                                      #-----------------------------------------
                                      # Check if data remediation was performed on source table and if it should be performed on non-conforming records
                                      #-----------------------------------------
                                      if (Settings$CurationProcess %>% filter(Table == tablename) %>% pull(DataRemediation) == TRUE &&
                                          Settings$CurationProcess %>% filter(Table == tablename) %>% pull(DataRemediation.NonconformingRecords) == TRUE &&
                                          length(Table) == 0 && nrow(Table) == 0)
                                      {
                                          # Get table-specific settings on data remediation process
                                          RemediationProcess <- Settings$DataRemediation$Process %>%
                                                                      filter(Table == tablename,
                                                                             RunRemediation == TRUE) %>%
                                                                      arrange(RemediationOrder)      # Defines the order in which features within a table are being remediated (this can be relevant in transformative espressions that contain inter-feature dependencies)

                                          # Check if there are any features in current table that should be remediated
                                          if (length(RemediationProcess) > 0 && nrow(RemediationProcess) > 0)
                                          {
                                              # Loop through table features that are supposed to be remediated
                                              for (featurename in RemediationProcess$Feature)
                                              {
                                                  # Get set of eligible values for current feature
                                                  EligibleValueSet <- MetaData$Values %>%
                                                                          filter(Table == tablename,
                                                                                 FeatureName.Curated == featurename) %>%
                                                                          pull(Value.Raw)   # Eligible Values PRIOR to recoding

                                                  # Count ineligible values in current feature
                                                  CountValues.Ineligible.Prior = sum(!is.na(Table[[featurename]]) & Table[[featurename]] %notin% EligibleValueSet)

                                                  # Check if there are any ineligible feature values needing remediation
                                                  if (!is.na(CountValues.Ineligible.Prior) && CountValues.Ineligible.Prior > 0)
                                                  {

                                                      # Get data remediation methods for current feature
                                                      Methods <- Settings$DataRemediation$Process %>%
                                                                      filter(Table == tablename,
                                                                             Feature == featurename) %>%
                                                                      as.list()

                                                      TransformativeExpressions <- Settings$DataRemediation$TransformativeExpressions %>%
                                                                                        filter(Table == tablename,
                                                                                               Feature == featurename)

                                                      Dictionary <- Settings$DataRemediation$Dictionary %>%
                                                                        filter(Table == tablename,
                                                                               Feature == featurename) %>%
                                                                        pull(var = NewValue,
                                                                             name = LookupValue)

                                                      FuzzyStringMatching <- Settings$DataRemediation$FuzzyStringMatching %>%
                                                                                  filter(Table == tablename,
                                                                                         Feature == featurename) %>%
                                                                                  as.list()

                                                      # EXECUTE data remediation for current feature
                                                      Table[[featurename]] <- dsFreda::RemediateData(Feature = Table[[featurename]],
                                                                                                     FeatureName = featurename,
                                                                                                     ContextDataFrame = Table,
                                                                                                     Methods = Methods,
                                                                                                     EligibleValueSet = EligibleValueSet,
                                                                                                     TransformativeExpressions = TransformativeExpressions,
                                                                                                     Dictionary = Dictionary,
                                                                                                     FuzzyStringMatching = FuzzyStringMatching)
                                                  }
                                              }
                                          }
                                      }

                                      #-----------------------------------------
                                      # In any case: Recode data
                                      #-----------------------------------------

                                      if (length(Table) > 0 && nrow(Table) > 0)
                                      {
                                          # Recode
                                          #-------------------------------------
                                          ValueSets <- tibble()
                                          if (tablename == ".DataSetRoot")
                                          {
                                              ValueSets <- MetaData$Values %>%
                                                                filter(Table %in% RootTableNames)
                                          } else {

                                              ValueSets <- MetaData$Values %>%
                                                                filter(Table == tablename)
                                          }

                                          if (length(ValueSets) > 0 && nrow(ValueSets > 0))
                                          {
                                              RecodingDictionaries <- ValueSets %>%
                                                                          split(.$FeatureName.Curated) %>%      # 'split' is a base function and needs '.$' to address 'FeatureName.Curated'
                                                                          map(\(Values) with(Values, set_names(Value.Curated, Value.Raw)))

                                              FeaturesWithValueSets <- ValueSets %>%
                                                                            pull(FeatureName.Curated) %>%
                                                                            unique()

                                              for (featurename in FeaturesWithValueSets)
                                              {
                                                  Table[[featurename]] <- dsFreda::RecodeData(TargetVector = Table[[featurename]],
                                                                                              Dictionary = RecodingDictionaries[[featurename]])
                                              }
                                          }
                                      }

                                      #-----------------------------------------
                                      # Check if remediation finalization of non-conforming records is intended and applicable for current table
                                      #-----------------------------------------
                                      if (Settings$CurationProcess %>% filter(Table == tablename) %>% pull(DataRemediation) == FALSE &&
                                          Settings$CurationProcess %>% filter(Table == tablename) %>% pull(DataRemediation.NonconformingRecords) == TRUE &&
                                          length(Table) > 0 && nrow(Table) > 0)
                                      {
                                          # Get features that are supposed to be remediated from settings
                                          RemediationProcess <- Settings$DataRemediation$Process %>%
                                                                      filter(Table == tablename,
                                                                             RunRemediation == TRUE)

                                          if (length(RemediationProcess) > 0 && nrow(RemediationProcess) > 0)
                                          {
                                              for (featurename in RemediationProcess$Feature)
                                              {
                                                  # Get eligible value set for current feature as data.frame
                                                  EligibleValueSet <- MetaData$Values %>%
                                                                          filter(Table == tablename,
                                                                                 FeatureName.Curated == featurename)

                                                  if (nrow(EligibleValueSet) > 0)
                                                  {
                                                      Table[[featurename]] <- dsFreda::FinalizeDataRemediation(TargetVector = Table[[featurename]],
                                                                                                               EligibleValueSet = EligibleValueSet,
                                                                                                               UnremediatedValues.Substitute = RemediationProcess %>% filter(Feature == featurename) %>% pull(UnremediatedValues.Substitute),
                                                                                                               UnremediatedValues.Substitution = RemediationProcess %>% filter(Feature == featurename) %>% pull(UnremediatedValues.Substitution))
                                                  }
                                              }
                                          }
                                      }

                                      #-----------------------------------------
                                      # In any case: Format data in 'NonconformingRecords' (so that appending more records later does not result in compatibility errors)
                                      #-----------------------------------------

                                      FeatureTypes <- tibble()
                                      if (tablename == ".DataSetRoot")
                                      {
                                          FeatureTypes <- MetaData$Features %>%
                                                              filter(TableName.Curated %in% RootTableNames)
                                      } else {

                                          FeatureTypes <- MetaData$Features %>%
                                                              filter(TableName.Curated == tablename)
                                      }

                                      FeatureTypes <- FeatureTypes %>%
                                                          select(FeatureName.Curated,
                                                                 Type) %>%
                                                          rename(Feature = "FeatureName.Curated")

                                      if (nrow(FeatureTypes) > 0)
                                      {
                                          for (i in 1:nrow(FeatureTypes))
                                          {
                                              Table <- Table %>%
                                                            mutate(across(all_of(FeatureTypes$Feature[i]),
                                                                          ~ dsFreda::FormatData(.x, FeatureTypes$Type[i])))
                                          }
                                      }

                                      #---------------------------------------------
                                      return(Table)
                                   })
                                   # .progress = list(name = "Harmonizing non-conforming records",
                                   #                  type = "iterator"))



#===============================================================================
# MODULE F)  Secondary Table Cleaning
#===============================================================================
#   - Remove ineligible records that may have been introduced during processing
#   - Same proceedings as in primary table cleaning (MODULE C))
#-------------------------------------------------------------------------------

  # Log report: New processing stage
  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "Secondary Table Cleaning",
                                    Message = "Secondary Table Cleaning",
                                    MessageClass = "Topic",
                                    MessagePriority = 3,
                                    PrintMessage = TRUE))

  # Recreate 'DataSetRoot' by pair-wise left-joining of 'Seed' table with all 'Root' tables
  DataSetRoot <- reduce(.x = DataSet[RootTableNames[RootTableNames != SeedTableName]],
                        .f = \(TableA, TableB) left_join(TableA, TableB, by = SeedPrimaryKey),
                        .init = DataSet[[SeedTableName]])

  # Secondary cleaning of 'DataSetRoot'
  if ((Settings$CurationProcess %>% filter(Table == ".DataSetRoot") %>% pull(SecondaryTableCleaning)) == TRUE)
  {
      # dsFreda::CleanTable() returns a list with the elements 'Table', 'NonconformingRecords', 'Counter' and 'Log'
      SecondaryTableCleaning.DataSetRoot <- dsFreda::CleanTable(Table = DataSetRoot,
                                                                TableName = ".DataSetRoot",
                                                                PrimaryKey = RootPrimaryKey,
                                                                RootSubjectKey = RootPrimaryKey,
                                                                SeedSubjectKey = SeedPrimaryKey,
                                                                PrimaryKeyIgnoredInRedundancyCheck = FALSE,      # This setting is important because it considers the semantic meaning of root primary keys (e.g. two different DiagnosisIDs of the same patient should stay truly distinct, because they can be related to different diagnostic procedures)
                                                                EmptyStrings.Detect = Settings$SecondaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(EmptyStrings.Detect),
                                                                EmptyStrings.Substitute = Settings$SecondaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(EmptyStrings.Substitute),
                                                                EmptyStrings.Substitution = Settings$SecondaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(EmptyStrings.Substitution),
                                                                DuplicateRecords.Detect = Settings$SecondaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(DuplicateRecords.Detect),
                                                                DuplicateRecords.Remove = Settings$SecondaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(DuplicateRecords.Remove),
                                                                FeatureRequirements = Settings$FeatureRequirements %>% filter(Table %in% RootTableNames),
                                                                FeatureAvailabilityViolations.Detect = Settings$SecondaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(FeatureAvailabilityViolations.Detect),
                                                                FeatureAvailabilityViolations.Remove = Settings$SecondaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(FeatureAvailabilityViolations.Remove),
                                                                PrintMessages = TRUE)

      # Assess new table record and root/seed subject counts
      StageCounter.DataSetRoot <- list(.DataSetRoot = DataSetRoot) %>%
                                      dsFreda::TrackCounts(TransformationReturn = list(.DataSetRoot = PrimaryTableCleaning.DataSetRoot),
                                                           RootSubjectKeys = list(.DataSetRoot = RootPrimaryKey),
                                                           SeedSubjectKey = SeedPrimaryKey,
                                                           PrintMessages = FALSE) %>%
                                      mutate(ProcessingStage = "PRE Secondary Table Cleaning",
                                             ProcessTopic = "COUNT",
                                             CountLevel = "Monitor")

      # Update COUNTER report
      Report.Counter <- Report.Counter %>%
                            bind_rows(SecondaryTableCleaning.DataSetRoot$Counter %>% mutate(ProcessingStage = "Secondary Table Cleaning"),
                                      StageCounter.DataSetRoot)

      # Update LOG report
      Report.Log <- Report.Log %>%
                        bind_rows(SecondaryTableCleaning.DataSetRoot$Log %>% mutate(ProcessingStage = "Secondary Table Cleaning"))

      # Reassign DataSetRoot
      DataSetRoot <- SecondaryTableCleaning.DataSetRoot$Table

      # Save non-conforming records
      NonconformingRecords[[".DataSetRoot"]] <- NonconformingRecords[[".DataSetRoot"]] %>%
                                                    bind_rows(SecondaryTableCleaning.DataSetRoot$NonconformingRecords)
  }

  # Select only primary key features
  DataSetRoot <- DataSetRoot %>%
                      select(all_of(RootPrimaryKey)) %>%
                      distinct()

  # Execute Secondary Table Cleaning
  SecondaryTableCleaning <- DataSet %>%
                                imap(function(Table, tablename)
                                     {
                                        # Check in settings if secondary table cleaning should be executed for current table
                                        if ((Settings$CurationProcess %>% filter(Table == tablename) %>% pull(SecondaryTableCleaning)) == FALSE)
                                        {
                                            CurrentTableCleaning <- list(Table = Table,
                                                                         NonconformingRecords = NULL,
                                                                         Counter = NULL,
                                                                         Log = Log.New(ProcessingStage = "Secondary Table Cleaning",
                                                                                       Table = tablename,
                                                                                       ProcessTopic = "Table cleaning",
                                                                                       ProcessExecution = "Omitted",
                                                                                       Message = "Omitted.",
                                                                                       MessageClass = "Info"))

                                        # Check if current table is missing or empty
                                        } else if (length(Table) == 0 || nrow(Table) == 0) {

                                            CurrentTableCleaning <- list(Table = Table,
                                                                         NonconformingRecords = NULL,
                                                                         Counter = NULL,
                                                                         Log = Log.New(ProcessingStage = "Secondary Table Cleaning",
                                                                                       Table = tablename,
                                                                                       ProcessTopic = "Table cleaning",
                                                                                       ProcessExecution = "Inapplicable",
                                                                                       Message = "Table is missing or empty.",
                                                                                       MessageClass = "Info"))

                                        # Main Case
                                        } else {

                                            # For Root tables 'PrimaryKeyIgnoredInRedundancyCheck' must be set on FALSE (for explanation see section on DataSetRoot above)
                                            PrimaryKeyIgnoredInRedundancyCheck <- TRUE
                                            if (tablename %in% RootTableNames) { PrimaryKeyIgnoredInRedundancyCheck <- FALSE }

                                            # Perform table cleaning (this returns a list object with the elements 'Table', 'Report' and 'AffectedRootSubjects')
                                            CurrentTableCleaning <- dsFreda::CleanTable(Table = Table,
                                                                                        TableName = tablename,
                                                                                        PrimaryKey = PrimaryKeys[[tablename]],
                                                                                        RootSubjectKey = RootSubjectKeys[[tablename]],
                                                                                        SeedSubjectKey = SeedPrimaryKey,
                                                                                        PrimaryKeyIgnoredInRedundancyCheck = PrimaryKeyIgnoredInRedundancyCheck,
                                                                                        DataSetRoot = DataSetRoot,
                                                                                        UnlinkedRecords.Detect = Settings$SecondaryTableCleaning %>% filter(Table == tablename) %>% pull(UnlinkedRecords.Detect),
                                                                                        UnlinkedRecords.Remove = Settings$SecondaryTableCleaning %>% filter(Table == tablename) %>% pull(UnlinkedRecords.Remove),
                                                                                        EmptyStrings.Detect = Settings$SecondaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Detect),
                                                                                        EmptyStrings.Substitute = Settings$SecondaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Substitute),
                                                                                        EmptyStrings.Substitution = Settings$SecondaryTableCleaning %>% filter(Table == tablename) %>% pull(EmptyStrings.Substitution),
                                                                                        DuplicateRecords.Detect = Settings$SecondaryTableCleaning %>% filter(Table == tablename) %>% pull(DuplicateRecords.Detect),
                                                                                        DuplicateRecords.Remove = Settings$SecondaryTableCleaning %>% filter(Table == tablename) %>% pull(DuplicateRecords.Remove),
                                                                                        FeatureRequirements = Settings$FeatureRequirements %>% filter(Table == tablename),
                                                                                        FeatureAvailabilityViolations.Detect = Settings$SecondaryTableCleaning %>% filter(Table == tablename) %>% pull(FeatureAvailabilityViolations.Detect),
                                                                                        FeatureAvailabilityViolations.Remove = Settings$SecondaryTableCleaning %>% filter(Table == tablename) %>% pull(FeatureAvailabilityViolations.Remove),
                                                                                        PrintMessages = TRUE)

                                            # Add processing stage to COUNTER and LOG
                                            CurrentTableCleaning$Counter <- CurrentTableCleaning$Counter %>%
                                                                                mutate(ProcessingStage = "Secondary Table Cleaning")

                                            CurrentTableCleaning$Log <- CurrentTableCleaning$Log %>%
                                                                            mutate(ProcessingStage = "Secondary Table Cleaning")
                                        }

                                        return(CurrentTableCleaning)
                                    })
                                    # .progress = list(name = "Secondary table cleaning",
                                    #                  type = "iterator"))

  # Update COUNTER report
  Report.Counter <- Report.Counter %>%
                        bind_rows(SecondaryTableCleaning %>%
                                      map(\(CurrentTableCleaning) CurrentTableCleaning$Counter) %>%
                                      list_rbind())

  # Update LOG report
  Report.Log <- Report.Log %>%
                    bind_rows(SecondaryTableCleaning %>%
                                  map(\(CurrentTableCleaning) CurrentTableCleaning$Log) %>%
                                  list_rbind())

  # Print message to mark reporting of record count changes
  PrintSoloMessage(c(Topic = "Record counts after Secondary Table Cleaning"))

  # Assess new table record and root/seed subject counts
  StageCounter <- DataSet %>%
                      dsFreda::TrackCounts(TransformationReturn = SecondaryTableCleaning,
                                           RootSubjectKeys = RootSubjectKeys,
                                           SeedSubjectKey = SeedPrimaryKey,
                                           PrintMessages = TRUE) %>%
                      mutate(ProcessingStage = "Secondary Table Cleaning",
                             ProcessTopic = "COUNT",
                             CountLevel = "Stage")

  # Update COUNTER report
  Report.Counter <- Report.Counter %>%
                          bind_rows(StageCounter)

  # Reassign DataSet
  DataSet <- SecondaryTableCleaning %>%
                  imap(\(CurrentTableCleaning, tablename) CurrentTableCleaning$Table)

  # Save non-conforming records
  NonconformingRecords <- NonconformingRecords %>%
                              imap(\(CurrentNonconformingRecords, tablename) bind_rows(CurrentNonconformingRecords, SecondaryTableCleaning[[tablename]]$NonconformingRecords))

#-------------------------------------------------------------------------------
# For COUNTER: Recreate 'DataSetRoot' after Secondary Table Cleaning and count Records and Root / Seed subjects
#-------------------------------------------------------------------------------

  # Recreate 'DataSetRoot' by pair-wise left-joining of 'Seed' table with all 'Root' tables
  DataSetRoot <- reduce(.x = DataSet[RootTableNames[RootTableNames != SeedTableName]],
                        .f = \(TableA, TableB) left_join(TableA, TableB, by = SeedPrimaryKey),
                        .init = DataSet[[SeedTableName]])

  # Add DataSetRoot COUNTER to Counter report
  Report.Counter <- Report.Counter %>%
                        Counter.Add(Counter.New(ProcessingStage = "Secondary Table Cleaning",
                                                Table = ".DataSetRoot",
                                                ProcessTopic = "COUNT",
                                                CountLevel = "Stage",
                                                CountRecords.Post = nrow(DataSetRoot),
                                                CountRootSubjects.Post = n_distinct(DataSetRoot[RootPrimaryKey]),
                                                CountSeedSubjects.Post = n_distinct(DataSetRoot[SeedPrimaryKey]),
                                                Message = "DataSetRoot Stage count",
                                                MessageClass = "Info",
                                                MessagePriority = 3,
                                                PrintMessage = FALSE))



#===============================================================================
# MODULE G)  RECORD SUBSUMPTION (Find table records that can be considered redundant if they provide no additional informational value compared to a previous record)
#===============================================================================
# 1)  Process data set root tables first, since other tables hold primary key
#     Any Root subject keys that are removed due to redundancy need to be replaced in dependent tables.
# 2)  Proceed with all other tables (excluding Seed table)
#-------------------------------------------------------------------------------

  # Log: New processing stage
  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "Record Subsumption",
                                    Message = "Record Subsumption",
                                    MessageClass = "Topic",
                                    MessagePriority = 3,
                                    PrintMessage = TRUE))

  # Define process as function to avoid redundant code (since following processing is separately performed for ROOT and BRANCH tables)
  Process.RecordSubsumption <- function(Table, tablename)
                               {
                                  # Check in settings if record subsumption should be executed for current table
                                  if ((Settings$CurationProcess %>% filter(Table == tablename) %>% pull(RecordSubsumption)) == FALSE)
                                  {
                                      CurrentRecordSubsumption <- list(Table = Table,
                                                                       NonconformingRecords = NULL,
                                                                       Counter = NULL,
                                                                       Log = Log.New(ProcessingStage = "Record Subsumption",
                                                                                     Table = tablename,
                                                                                     ProcessTopic = "Record subsumption",
                                                                                     ProcessExecution = "Omitted",
                                                                                     Message = "Omitted.",
                                                                                     MessageClass = "Info",
                                                                                     PrintMessage = TRUE))

                                  # Check if current table is missing or empty
                                  } else if (length(Table) == 0 || nrow(Table) == 0) {

                                      CurrentRecordSubsumption <- list(Table = Table,
                                                                       NonconformingRecords = NULL,
                                                                       Counter = NULL,
                                                                       Log = Log.New(ProcessingStage = "Record Subsumption",
                                                                                     Table = tablename,
                                                                                     ProcessTopic = "Record subsumption",
                                                                                     ProcessExecution = "Inapplicable",
                                                                                     Message = "Table is missing or empty.",
                                                                                     MessageClass = "Info",
                                                                                     PrintMessage = TRUE))

                                  # Main Case
                                  } else {

                                      # Perform RECORD SUBSUMPTION (this returns a list object with the elements 'Table', 'NonconformingRecords', 'Counter' and 'Log')
                                      CurrentRecordSubsumption <- dsFreda::SubsumeRecords(Table = Table,
                                                                                          TableName = tablename,
                                                                                          PrimaryKey = PrimaryKeys[[tablename]],
                                                                                          RootSubjectKey = RootSubjectKeys[[tablename]],
                                                                                          SeedSubjectKey = SeedPrimaryKey,
                                                                                          SubsumptionRedundancies.Detect = Settings$RecordSubsumption %>% filter(Table == tablename) %>% pull(SubsumptionRedundancies.Detect),
                                                                                          SubsumptionRedundancies.Remove = Settings$RecordSubsumption %>% filter(Table == tablename) %>% pull(SubsumptionRedundancies.Remove),
                                                                                          SubsumptionRedundancies.DistinctiveFeatures = MetaData$Features %>% filter(TableName.Curated == tablename, Subsumption.IsDistinctive == TRUE) %>% pull(FeatureName.Curated),
                                                                                          SubsumptionRedundancies.NegligibleFeatures = MetaData$Features %>% filter(TableName.Curated == tablename, Subsumption.IsNegligible == TRUE) %>% pull(FeatureName.Curated),
                                                                                          SubsumptionRedundancies.NegligibleValues = Settings$DataRemediation$Process %>% filter(Table == tablename) %>% select(Feature, UnremediatedValues.Substitution) %>% tibble::deframe() %>% as.list(),
                                                                                          PrintMessages = TRUE)

                                      # Add processing stage to COUNTER and LOG
                                      CurrentRecordSubsumption$Counter <- CurrentRecordSubsumption$Counter %>%
                                                                              mutate(ProcessingStage = "Record Subsumption")

                                      CurrentRecordSubsumption$Log <- CurrentRecordSubsumption$Log %>%
                                                                          mutate(ProcessingStage = "Record Subsumption")
                                  }

                                  return(CurrentRecordSubsumption)
                               }

  # Record Subsumption in data set ROOT tables
  RecordSubsumption.Root <- DataSet[RootTableNames] %>%
                                imap(function(Table, tablename)
                                     {
                                        # Perform RECORD SUBSUMPTION by calling predefined process (see above)
                                        CurrentRecordSubsumption <- Process.RecordSubsumption(Table, tablename)

                                        # Create MAPPING for later replacement of foreign keys in branch tables
                                        CurrentRootSubjectKeyMapping <- NULL

                                        if (length(CurrentRecordSubsumption$NonconformingRecords) > 0)
                                        {
                                            # For the current root table, create a mapping data.frame to know which foreign keys to replace in branch tables
                                            CurrentRootSubjectKeyMapping <- CurrentRecordSubsumption$NonconformingRecords %>%
                                                                                select(all_of(PrimaryKeys[[tablename]]),
                                                                                       starts_with(".Reference."))
                                        }

                                        return(list(Table = CurrentRecordSubsumption$Table,
                                                    NonconformingRecords = CurrentRecordSubsumption$NonconformingRecords,
                                                    Counter = CurrentRecordSubsumption$Counter,
                                                    Log = CurrentRecordSubsumption$Log,
                                                    RootSubjectKeyMapping = CurrentRootSubjectKeyMapping))
                                     })


  # Create 'RootSubjectKeyMapping' holding mapping information on how to replace root subject keys (= foreign keys) in branch tables secondary to removal of root table records
  # Element names in this list correspond to Primary Key names of root tables
  RootSubjectKeyMapping <- RecordSubsumption.Root %>%
                                map(\(CurrentRecordSubsumption) CurrentRecordSubsumption$RootSubjectKeyMapping) %>%
                                set_names(PrimaryKeys[names(RecordSubsumption.Root)])


  # Foreign key REPLACEMENT and RECORD SUBSUMPTION in data set BRANCH tables
  RecordSubsumption.Branches <- DataSet[BranchTableNames] %>%
                                    imap(function(Table, tablename)
                                         {
                                            # 1) REPLACE foreign keys in branch tables where necessary
                                            #-----------------------------------
                                            TableRootSubjectKey <- RootSubjectKeys[[tablename]]
                                            TableRootSubjectKey.WithoutSeed <- TableRootSubjectKey[!(TableRootSubjectKey %in% SeedPrimaryKey)]

                                            if (length(TableRootSubjectKey.WithoutSeed) > 0)
                                            {
                                                for (keyfeature in TableRootSubjectKey.WithoutSeed)
                                                {
                                                    if (length(RootSubjectKeyMapping) > 0 && keyfeature %in% names(RootSubjectKeyMapping))
                                                    {
                                                        CurrentRootSubjectKeyMapping <- RootSubjectKeyMapping[[keyfeature]]
                                                        if (length(CurrentRootSubjectKeyMapping) > 0 && nrow(CurrentRootSubjectKeyMapping))
                                                        {
                                                            Table <- Table %>%
                                                                        left_join(CurrentRootSubjectKeyMapping, by = join_by(!!!syms(TableRootSubjectKey.WithoutSeed))) %>%
                                                                        mutate(.ReferenceRootSubjectKey = .data[[paste0(".Reference.", keyfeature)]],
                                                                               !!sym(keyfeature) := ifelse(!is.na(.ReferenceRootSubjectKey),
                                                                                                           .ReferenceRootSubjectKey,
                                                                                                           .data[[keyfeature]])) %>%
                                                                        select(-.ReferenceRootSubjectKey)
                                                        }
                                                    }
                                                }
                                            }

                                            # 2) Perform RECORD SUBSUMPTION by calling predefined process (see above)
                                            #-----------------------------------
                                            CurrentRecordSubsumption <- Process.RecordSubsumption(Table, tablename)

                                            return(CurrentRecordSubsumption)
                                         })

  # Bind results from ROOT- and BRANCH-table processing (and make sure the list elements are in same order as list of 'DataSet')
  RecordSubsumption <- c(RecordSubsumption.Root,
                         RecordSubsumption.Branches)[names(DataSet)]

  # Update COUNTER report
  Report.Counter <- Report.Counter %>%
                        bind_rows(RecordSubsumption %>%
                                      map(\(CurrentRecordSubsumption) CurrentRecordSubsumption$Counter) %>%
                                      list_rbind())

  # Update LOG report
  Report.Log <- Report.Log %>%
                    bind_rows(RecordSubsumption %>%
                                  map(\(CurrentRecordSubsumption) CurrentRecordSubsumption$Log) %>%
                                  list_rbind())

  # Print message to mark reporting of record count changes
  PrintSoloMessage(c(Topic = "Record counts after Record Subsumption"))

  # Assess new table record and root/seed subject counts
  StageCounter <- DataSet %>%
                      dsFreda::TrackCounts(TransformationReturn = RecordSubsumption,
                                           RootSubjectKeys = RootSubjectKeys,
                                           SeedSubjectKey = SeedPrimaryKey,
                                           PrintMessages = TRUE) %>%
                      mutate(ProcessingStage = "Record Subsumption",
                             ProcessTopic = "COUNT",
                             CountLevel = "Stage")

  # Update COUNTER report
  Report.Counter <- Report.Counter %>%
                        bind_rows(StageCounter)

  # Reassign DataSet
  DataSet <- RecordSubsumption %>%
                  imap(\(CurrentRecordSubsumption, tablename) CurrentRecordSubsumption$Table)

  # Save non-conforming records
  NonconformingRecords <- NonconformingRecords %>%
                              imap(\(CurrentNonconformingRecords, tablename) bind_rows(CurrentNonconformingRecords, RecordSubsumption[[tablename]]$NonconformingRecords))


#-------------------------------------------------------------------------------
# For COUNTER: Recreate 'DataSetRoot' after Record Subsumption and count Records and Root / Seed subjects
#-------------------------------------------------------------------------------

  # Recreate 'DataSetRoot' by pair-wise left-joining of 'Seed' table with all 'Root' tables
  DataSetRoot <- reduce(.x = DataSet[RootTableNames[RootTableNames != SeedTableName]],
                        .f = \(TableA, TableB) left_join(TableA, TableB, by = SeedPrimaryKey),
                        .init = DataSet[[SeedTableName]])

  # Add DataSetRoot COUNTER to Counter report
  Report.Counter <- Report.Counter %>%
                          Counter.Add(Counter.New(ProcessingStage = "Record Subsumption",
                                                  Table = ".DataSetRoot",
                                                  ProcessTopic = "COUNT",
                                                  CountLevel = "Stage",
                                                  CountRecords.Post = nrow(DataSetRoot),
                                                  CountRootSubjects.Post = n_distinct(DataSetRoot[RootPrimaryKey]),
                                                  CountSeedSubjects.Post = n_distinct(DataSetRoot[SeedPrimaryKey]),
                                                  Message = "DataSetRoot: Final record and subject count",
                                                  MessageClass = "Info",
                                                  MessagePriority = 2,
                                                  PrintMessage = FALSE))



#===============================================================================
# Get affected root subjects from non-conforming records
#===============================================================================

  AffectedRootSubjects <- NonconformingRecords %>%
                              map(function(Table)
                                  {
                                      FeatureSelection <- names(Table)[names(Table) %in% c(RootPrimaryKey, ".HasBeenRemoved", ".IsArtificial")]

                                      WithNonconformingRecords <- Table %>%
                                                                      select(all_of(FeatureSelection))
                                  }) %>%
                                list_rbind() %>%
                                distinct()

  AffectedRootSubjects.RemovedRecords <- AffectedRootSubjects %>%
                                              filter(.HasBeenRemoved == TRUE) %>%
                                              { if (".IsArtifical" %in% names(.)) { filter(., is.na(.IsArtificial) | .IsArtificial == FALSE) }}



#===============================================================================
# Final DataSet modifications
#===============================================================================

  # Conversion of Tables from tibble to data.frame, because DataSHIELD can handle data.frames better
  DataSet <- DataSet %>%
                  map(\(Table) as.data.frame(Table))

  NonconformingRecords <- NonconformingRecords %>%
                              map(\(Table) as.data.frame(Table))



#===============================================================================
# Finalize COUNTER reports
#===============================================================================

  # For DataSetRoot COUNTER entries on stage-level: Calculate changes in record and subject counts across processing stages
  Report.Counter.DataSetRoot <- Report.Counter %>%
                                    filter(CountLevel == "Stage" & Table == ".DataSetRoot") %>%
                                    mutate(CountRecords.Prior = case_when(is.na(CountRecords.Prior) ~ lag(CountRecords.Post),
                                                                          .default = CountRecords.Prior),
                                           CountRootSubjects.Prior = case_when(is.na(CountRootSubjects.Prior) ~ lag(CountRootSubjects.Post),
                                                                               .default = CountRootSubjects.Prior),
                                           CountSeedSubjects.Prior = case_when(is.na(CountSeedSubjects.Prior) ~ lag(CountSeedSubjects.Post),
                                                                               .default = CountSeedSubjects.Prior),
                                           CountRecords.Change = CountRecords.Post - CountRecords.Prior,
                                           CountRootSubjects.Change = CountRootSubjects.Post - CountRootSubjects.Prior,
                                           CountSeedSubjects.Change = CountSeedSubjects.Post - CountSeedSubjects.Prior)

  # Report.Counter: Update '.DataSetRoot' rows after modification
  Report.Counter <- Report.Counter %>%
                        filter(!(Table == ".DataSetRoot" & CountLevel == "Stage")) %>%
                        bind_rows(Report.Counter.DataSetRoot)

  # For ALL COUNTER entries on stage-level: Calculate summarizing count values (sums for record counts, MAXIMA for subject counts)
  Report.Counter.All <- Report.Counter %>%
                            filter(CountLevel == "Stage") %>%
                            group_by(ProcessingStage) %>%
                                summarize(across(c(CountRecords.Prior, CountRecords.Nonconforming, CountRecords.Change, CountRecords.Post),
                                                 ~ sum(.x[Table != ".DataSetRoot"], na.rm = TRUE)),
                                          ConsistencyCheck = ifelse(CountRecords.Change > 0, NA, CountRecords.Nonconforming == abs(CountRecords.Change)),      # Checking if the number of non-conforming records is equal to the change in record count post transformation
                                          across(c(CountRootSubjects.Prior, CountSeedSubjects.Prior,
                                                   CountRootSubjects.Change, CountSeedSubjects.Change,
                                                   CountRootSubjects.Post, CountSeedSubjects.Post),
                                                 ~ .x[Table == ".DataSetRoot"]),
                                          across(c(CountRootSubjects.Affected, CountSeedSubjects.Affected),
                                                 ~ ifelse(all(is.na(.x)), NA, max(.x, na.rm = TRUE)))) %>%
                                mutate(Table = ".All",
                                       ProcessTopic = "COUNT",
                                       CountLevel = "Stage")

  # Report.Counter: Add '.All' rows
  Report.Counter <- Report.Counter %>%
                        bind_rows(Report.Counter.All)

  # Impute missing values in Report.Counter
  Report.Counter <- Report.Counter %>%
                        mutate(CountRecords.Removed = case_when(is.na(CountRecords.Removed) & !is.na(CountRecords.Added) & !is.na(CountRecords.Change) ~ abs(CountRecords.Change) - CountRecords.Added,
                                                                is.na(CountRecords.Removed) & !is.na(CountRecords.Change) ~ abs(CountRecords.Change),
                                                                is.na(CountRecords.Removed) ~ 0,
                                                                .default = CountRecords.Removed),
                               CountRecords.Added = case_when(is.na(CountRecords.Added) ~ 0,
                                                              .default = CountRecords.Added),
                               CountRecords.Change = case_when(is.na(CountRecords.Change) ~ CountRecords.Added - CountRecords.Removed,
                                                               .default = CountRecords.Change),
                               CountRecords.Change.Proportion = case_when(is.na(CountRecords.Change.Proportion) & CountRecords.Prior > 0 ~ CountRecords.Change / CountRecords.Prior,
                                                                          .default = CountRecords.Change.Proportion),
                               CountRootSubjects.Change.Proportion = case_when(is.na(CountRootSubjects.Change.Proportion) & CountRootSubjects.Prior > 0 ~ CountRootSubjects.Change / CountRootSubjects.Prior,
                                                                               .default = CountRootSubjects.Change.Proportion),
                               CountSeedSubjects.Change.Proportion = case_when(is.na(CountSeedSubjects.Change.Proportion) & CountSeedSubjects.Prior > 0 ~ CountSeedSubjects.Change / CountSeedSubjects.Prior,
                                                                               .default = CountSeedSubjects.Change.Proportion))

  # Create a summarizing history of record and subject counts from start to finish of Curation
  Report.Counter.Summary <- Report.Counter %>%
                                filter(CountLevel == "Stage") %>%
                                select(ProcessingStage,
                                       Table,
                                       CountRecords.Change,
                                       CountRecords.Change.Proportion,
                                       CountRecords.Post,
                                       CountRootSubjects.Change,
                                       CountRootSubjects.Change.Proportion,
                                       CountRootSubjects.Post,
                                       CountSeedSubjects.Change,
                                       CountSeedSubjects.Change.Proportion,
                                       CountSeedSubjects.Post) %>%
                                rename(CountRecords = "CountRecords.Post",
                                       CountRootSubjects = CountRootSubjects.Post,
                                       CountSeedSubjects = CountSeedSubjects.Post) %>%
                                mutate(ProcessingStage = str_remove_all(ProcessingStage, " ")) %>%
                                pivot_wider(names_from = ProcessingStage,
                                            names_glue = "{ProcessingStage}.{.value}",
                                            values_from = !c(ProcessingStage, Table)) %>%
                                select(Table,
                                       Initial.CountRecords,
                                       Initial.CountRootSubjects,
                                       Initial.CountSeedSubjects,
                                       starts_with("PrimaryTableCleaning"),
                                       starts_with("TableNormalization"),
                                       starts_with("SecondaryTableCleaning"),
                                       starts_with("RecordSubsumption")) %>%
                                mutate(Final.CountRecords = RecordSubsumption.CountRecords,
                                       Final.CountRecords.Change = Final.CountRecords - Initial.CountRecords,
                                       Final.CountRecords.Change.Proportion = case_when(Initial.CountRecords > 0 ~ Final.CountRecords.Change / Initial.CountRecords,
                                                                                        .default = NA_real_),
                                       Final.CountRootSubjects = RecordSubsumption.CountRootSubjects,
                                       Final.CountRootSubjects.Change = Final.CountRootSubjects - Initial.CountRootSubjects,
                                       Final.CountRootSubjects.Change.Proportion = case_when(Initial.CountRootSubjects > 0 ~ Final.CountRootSubjects.Change / Initial.CountRootSubjects,
                                                                                             .default = NA_real_),
                                       Final.CountSeedSubjects = RecordSubsumption.CountSeedSubjects,
                                       Final.CountSeedSubjects.Change = Final.CountSeedSubjects - Initial.CountSeedSubjects,
                                       Final.CountSeedSubjects.Change.Proportion = case_when(Initial.CountSeedSubjects > 0 ~ Final.CountSeedSubjects.Change / Initial.CountSeedSubjects,
                                                                                             .default = NA_real_))

  # Split COUNTER SUMMARY into table-level and data-set-level views
  Report.Counter.TableLevel <- Report.Counter.Summary %>%
                                      filter(Table != ".All")

  Report.Counter.DataSetLevel <- Report.Counter.Summary %>%
                                      filter(Table == ".All") %>%
                                      mutate(CountTables = length(DataSet), .before = 1) %>%
                                      select(-Table)

  # Rearrange content of 'Report.Counter' into list of table-specific COUNTER data on stage-level and sub-stage-level
  Report.Counter <- Report.Counter %>%
                        split(.$Table) %>%
                        map(function(TableReport)
                            {
                                list(StageLevel = TableReport %>% filter(CountLevel == "Stage"),
                                     Details = TableReport %>% filter(CountLevel != "Stage"))
                            })



#===============================================================================
# State Curation completion in PROCESS and LOG reports
#===============================================================================

  # PROCESS report: Create/update entries
  Report.Process$Curation.CompletionCheck <- "green"
  Report.Process$Curation.End <- Sys.time()
  Report.Process$Curation.Duration <- round(lubridate::as.period(lubridate::interval(Report.Process$Curation.Start, Report.Process$Curation.End), unit = "minutes"))

  # LOG report: Add summarizing entries
  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "General",
                                    Message = paste0("Data Curation performed successfully! Duration: ", Report.Process$Curation.Duration, "."),
                                    MessageClass = "Special",
                                    PrintMessage = TRUE)) %>%
                    Log.Add(Log.New(ProcessingStage = "General",
                                    ProcessTopic = "Curation Summary",
                                    ProcessTopic.Subgroup = "Counter Summary",
                                    Message = with(Report.Counter.DataSetLevel,
                                                   paste0("Processed ", length(DataSet), " data set tables. ",
                                                          "Records: ", Initial.CountRecords,
                                                          " -> ", Final.CountRecords,
                                                          " (", Final.CountRecords.Change, "). ",
                                                          "Root subjects: ", Initial.CountRootSubjects,
                                                          " -> ", Final.CountRootSubjects,
                                                          " (", Final.CountRootSubjects.Change, "). ",
                                                          "Seed subjects: ", Initial.CountSeedSubjects,
                                                          " -> ", Final.CountSeedSubjects,
                                                          " (", Final.CountSeedSubjects.Change, ").")),
                                    MessageClass = "Info",
                                    PrintMessage = FALSE)) %>%
                    Log.Add(Log.New(ProcessingStage = "General",
                                    ProcessTopic = "Curation Summary",
                                    ProcessTopic.Subgroup = "Data Harmonization Summary",
                                    Message = with(Report.DataHarmonization.DataSetLevel,
                                                   paste0("Tracked ", CountTrackedFeatures, " features in ", CountAffectedTables, " tables. ",
                                                          "Of ", CountValues, " values ", CountValues.NonMissing, " were non-missing. ",
                                                          "Of these ", CountValues.Ineligible.Raw, " were initially ineligible. ",
                                                          "Of these ", CountValues.Harmonized, " could be harmonized.")),
                                    MessageClass = "Info",
                                    PrintMessage = FALSE))


#===============================================================================
# Finalize LOG report
#===============================================================================

  # Calculate time in seconds that went by between log entries
  Report.Log <- Report.Log %>%
                    mutate(TimeSinceLastEntry = difftime(Timestamp, lag(Timestamp)))


#===============================================================================
# PRINT Curation Summary
#===============================================================================

  # Print Curation Summary title
  SummaryTitle <- "Data Curation Summary"
  cat("\n", "\033[1m",
      paste0(rep("~", times = str_length(SummaryTitle)), collapse = ""),
      "\n", SummaryTitle, "\n",
      paste0(rep("~", times = str_length(SummaryTitle)), collapse = ""),
      "\033[0m", "\n", sep = "")

#--- Print COUNTER summary -----------------------------------------------------

  # Define print format of a COUNTER summary bullet point
  FormatCounterSummary <- function(InitialValue, FinalValue, ChangeValue, ChangeProportionValue)
  {
      ChangeProportionValue <- abs(ChangeProportionValue)
      ChangeProportionValue <- case_when(ChangeProportionValue > 0 & ChangeProportionValue < 0.01 ~ "< 1%",
                                         ChangeProportionValue >= 0.995 & ChangeProportionValue < 1 ~ "> 99%",
                                         .default = paste0(round(ChangeProportionValue * 100), "%"))

      paste0(format(InitialValue, big.mark = ","),
             "   ", symbol$en_dash, symbol$play, "   ",
             case_when(ChangeValue < 0 ~ "-",
                       ChangeValue > 0 ~ "+",
                       .default = ""),
             format(abs(ChangeValue), big.mark = ","),
             " (", ChangeProportionValue, ") ",
             "   ", symbol$en_dash, symbol$play, "   ",
             format(FinalValue, big.mark = ","))
  }

  # Print COUNTER Data Set Summary
  cat(paste0("\n", style_bold(style_underline("Data Set Count Summary")), "\n"))
  with(Report.Counter.DataSetLevel,
       cli_bullets(c("*" = style_bold(paste0("Tables: ", length(DataSet))),
                     "*" = style_bold(paste0("Seed subjects: ", FormatCounterSummary(Initial.CountSeedSubjects,
                                                                                     Final.CountSeedSubjects,
                                                                                     Final.CountSeedSubjects.Change,
                                                                                     Final.CountSeedSubjects.Change.Proportion))),
                    "*" = style_bold(paste0("Root subjects: ", FormatCounterSummary(Initial.CountRootSubjects,
                                                                                    Final.CountRootSubjects,
                                                                                    Final.CountRootSubjects.Change,
                                                                                    Final.CountRootSubjects.Change.Proportion))),
                    "*" = style_bold(paste0("Total Records: ", FormatCounterSummary(Initial.CountRecords,
                                                                                    Final.CountRecords,
                                                                                    Final.CountRecords.Change,
                                                                                    Final.CountRecords.Change.Proportion))))))

  # Print table-specific COUNTER Summaries
  cat(paste0(style_bold(style_underline("Table-specific Record counts")), "\n"))

  Report.Counter.TableLevel %>%
      filter(Table != ".DataSetRoot") %>%
      mutate(Print = paste0("'", Table, "': ", case_when(is.na(Initial.CountRecords) | Initial.CountRecords == 0 ~ "--- missing ---",
                                                         .default = FormatCounterSummary(Initial.CountRecords,
                                                                                         Final.CountRecords,
                                                                                         Final.CountRecords.Change,
                                                                                         Final.CountRecords.Change.Proportion)))) %>%
      pull(Print) %>%
      set_names(nm = "*") %>%
      cli_bullets()


#--- Print DATA HARMONIZATION summary -----------------------------------------------------

  # Print DATA HARMONIZATION Data Set Summary
  cat(paste0(style_bold(style_underline("Data Harmonization Summary")), "\n"))
  with(Report.DataHarmonization.DataSetLevel,
       cli_bullets(c("*" = style_bold(paste0("Tracked data harmonization of ", CountTrackedFeatures, " features in ", CountAffectedTables, " tables.")),
                     "*" = style_bold(paste0("Total values: ", format(CountValues, big.mark = ","))),
                     "*" = style_bold(paste0("Non-missing values: ", format(CountValues.NonMissing, big.mark = ","))),
                     "*" = style_bold(paste0("Ineligible values: ", format(CountValues.Ineligible.Raw, big.mark = ","))),
                     "*" = style_bold(paste0("Harmonized values: ", format(CountValues.Harmonized, big.mark = ","), " (", round(ProportionValues.Harmonized * 100, 1), "%)")))))

  # Define print format of a DATA HARMONIZATION summary bullet point
  FormatDataHarmonizationSummary <- function(CountTrackedFeatures, CountValues.Ineligible.Raw, CountValues.Harmonized, ProportionValues.Harmonized)
  {
      ProportionValue <- case_when(ProportionValues.Harmonized > 0 & ProportionValues.Harmonized < 0.01 ~ "< 1%",
                                   ProportionValues.Harmonized > 0.99 & ProportionValues.Harmonized < 1 ~ "> 99%",
                                   .default = paste0(round(ProportionValues.Harmonized * 100), "%"))

      paste0(format(CountTrackedFeatures, big.mark = ","), " tracked features had ",
             format(CountValues.Ineligible.Raw, big.mark = ","), " ineligible values, of which ",
             format(CountValues.Harmonized, big.mark = ","), " (", ProportionValue, ") were harmonized.")
  }

  # Print table-specific DATA HARMONIZATION Summaries
  cat(paste0(style_bold(style_underline("Table-specific Data Harmonization summaries")), "\n"))

  Report.DataHarmonization.TableLevel %>%
      mutate(Print = paste0("'", Table, "': ", case_when(CountValues.Ineligible.Raw == 0 ~ paste0(format(CountTrackedFeatures, big.mark = ","), " tracked features had no ineligible values."),
                                                         .default = FormatDataHarmonizationSummary(CountTrackedFeatures,
                                                                                                   CountValues.Ineligible.Raw,
                                                                                                   CountValues.Harmonized,
                                                                                                   ProportionValues.Harmonized)))) %>%
      pull(Print) %>%
      set_names(nm = "*") %>%
      cli_bullets()



#===============================================================================
# Compile content of main 'Report' object
#===============================================================================

  Report <- list(Settings = NULL,      # TO DO: Report chosen settings
                 Process = Report.Process,
                 Log = Report.Log,
                 Counter = list(Summary = list(DataSetLevel = Report.Counter.DataSetLevel,
                                               TableLevel = Report.Counter.TableLevel),
                                Extensive = Report.Counter),
                 DataHarmonization = list(Reports = list(DataSetLevel = Report.DataHarmonization.DataSetLevel,
                                                         TableLevel = Report.DataHarmonization.TableLevel,
                                                         FeatureLevel = Report.DataHarmonization.FeatureLevel,
                                                         ValueLevel = Report.DataHarmonization.ValueLevel,
                                                         ValueSets = Report.DataHarmonization.ValueSets),
                                          Monitors = list(TableLevel = Monitor.DataHarmonization.TableLevel,
                                                          FeatureLevel = Monitor.DataHarmonization.FeatureLevel,
                                                          ValueLevel = Monitor.DataHarmonization.ValueLevel)))

  # },
  #
  # # In case of occurring warning:
  # #===============================================================================
  # warning = function(w)
  #           {
  #               CompletionCheck <- "yellow"
  #               Report.Process$FinalMessage <- paste0("Completed Curation with following warning: \n", w)
  #           },
  #
  # # In case of occurring error:
  # #===============================================================================
  # error = function(e)
  #         {
  #             CompletionCheck <- "red"
  #             Report.Process$FinalMessage <- paste0("An error occured: \n", e)
  #         },
  #
  # #===============================================================================
  # # RETURN STATEMENT
  # #===============================================================================
  # finally =
  # {
  #   # Return the Curated Data Set (CDS), a Report object (defined above) and Messages
    return(list(DataSet = DataSet,
                NonconformingRecords = NonconformingRecords,
                AffectedRootSubjects = list(HadNonconformingRecords = AffectedRootSubjects,
                                            HadRemovedRecords = AffectedRootSubjects.RemovedRecords),
                Report = Report,
                Messages = Report.Process))
  # })

}

