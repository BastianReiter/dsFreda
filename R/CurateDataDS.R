
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
#'         \itemize{\item CuratedDataSet \code{list}
#'                      \itemize{ \item BioSampling
#'                                \item Diagnosis
#'                                \item DiseaseStatus
#'                                \item GeneralCondition
#'                                \item Histology
#'                                \item Metastasis
#'                                \item MolecularDiagnostics
#'                                \item OtherClassification
#'                                \item Patient
#'                                \item RadiationTherapy
#'                                \item Staging
#'                                \item Surgery
#'                                \item SystemicTherapy
#'                                \item TherapyRecommendation}
#'                  \item Report \code{list}
#'                      \itemize{ \item Log \code{tibble}
#'                                \item RecordCounts \code{tibble}
#'                                \item DataHarmonization \code{list}
#'                                    \itemize{ \item Overviews
#'                                              \item TransformationMonitors
#'                                              \item TransformationMonitors.Overviews
#'                                              \item TransformationMonitors.FullValueSets}}
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
#     1)  'Split and expand' where necessary (as determined by arguments / meta data)
#
#   MODULE E)  DATA HARMONIZATION
#     2) Definition of features to monitor during Transformation
#     3) Tracking of raw feature values
#     4) Data Remediation
#     5) Tracking of remediated feature values
#     6) Data recoding
#     7) Data formatting
#     8) Tracking of recoded feature values
#     9) Finalize data remediation (Substitution/Removal of ineligible values)
#     10) Tracking of finalized feature values
#     11) Compilation of monitor objects for reporting
#     12) Perform data harmonization (remediation, recoding, formatting) on non-conforming records
#
#   MODULE F)  SECONDARY TABLE CLEANING
#     1)  Remove duplicate records
#     2)  Remove records in CDS missing required features (defined in meta data or passed as optional argument)
#
#   MODULE G)  RECORD SUBSUMPTION
#     1)  Process table 'Diagnosis' first, since other tables hold primary key 'DiagnosisID'.
#         Any DiagnosisIDs that are removed due to redundancy need to be replaced in dependent tables.
#     2)  Proceed with all other tables (excluding 'Patient')
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
  # Profile.TransformativeExpressions.S <- "Defaul"

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

      PassedProfileName <- eval(parse(text = SettingsMapping$ProfileArgument[i]))
      RegisteredProfiles <- eval(parse(text = paste0("unique(", SettingsMapping$ObjectName[i], "$Profile)")))

      # Check if passed profile name for current settings object is present/registered in settings data.frame
      errormessage <- paste0("Value of argument '", SettingsMapping$ProfileArgument[i], "' does not occur in '", paste0(ModulePackageName, "::", SettingsMapping$ObjectName[i]), "'! Please pass one of the following registered profile names: ", paste0("'", RegisteredProfiles, "'", collapse = ", "), ".")
      if (!(PassedProfileName %in% RegisteredProfiles)) { stop(errormessage) }
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

  # Initiate main LOG report
  Report.Log <- Log.New(ProcessingStage = "General",
                        Message = "Starting Data Curation...",
                        MessageClass = "Special",
                        PrintMessage = TRUE)

  Report.DataRemediation <- tibble(Table = character(),
                                   Feature = character(),
                                   CountRecords = integer(),
                                   CountValues.NonMissing = integer(),
                                   CountValues.Ineligible.Prior = integer(),
                                   CountValues.Ineligible.Post = integer(),
                                   CountValues.Remediated = integer(),
                                   ProportionValues.Remediated = double())

  # Initiate 'CompletionCheck' variable
  CompletionCheck <- "red"

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


# Prepare list 'NonconformingRecords'
#-------------------------------------------------------------------------------

  NonconformingRecords <- c(".DataSetRoot", names(DataSet)) %>%
                              map(\(tablename) data.frame()) %>%
                              set_names(c(".DataSetRoot", names(DataSet)))


# Rename features from harmonized raw feature names to curated feature names
#-------------------------------------------------------------------------------

  # Looping through tables to rename features
  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          # Create named vector to look up matching feature names in meta data ('OldName' = 'NewName')
                          Lookup <- filter(MetaData$Features, TableName.Curated == tablename)$FeatureName.Raw
                          names(Lookup) <- filter(MetaData$Features, TableName.Curated == tablename)$FeatureName.Curated

                          if (length(Table) > 0)
                          {
                              # Rename feature names according to look-up vector
                              Table %>% rename(any_of(Lookup))      # Returns a tibble

                          } else {

                              # Create empty data.frame with pre-defined column names
                              EmptyTable <- data.frame(matrix(nrow = 0,
                                                              ncol = length(names(Lookup)))) %>%
                                                setNames(names(Lookup)) %>%
                                                mutate(across(everything(), ~ as.character(.x)))

                              return(EmptyTable)
                          }
                       })


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
# INITIATE TRACKER: Initial record count and creation of reporting object
#===============================================================================

  # Print message to mark reporting of record counts
  PrintSoloMessage(c(Topic = "Initial record counts"))

  # Initiate 'Report.Tracker' as data.frame holding table record and root subject counts throughout processing
  Report.Tracker <- DataSet %>%
                          imap(function(Table, tablename)
                               {
                                  Table %>%
                                      summarize(CountRecords.Prior = n(),
                                                CountRootSubjects.Prior = n_distinct(pick(RootSubjectKeys[[tablename]])),
                                                CountRecords.Post = CountRecords.Prior,
                                                CountRootSubjects.Post = CountRootSubjects.Prior)
                               }) %>%
                          list_rbind(names_to = "Table") %>%
                          mutate(ProcessingStage = "Initial",
                                 ProcessTopic = "COUNT",
                                 CountLevel = "Stage",
                                 Message = paste0(CountRecords.Prior, " records belonging to ", CountRootSubjects.Prior, " root subjects."),
                                 MessageClass = "Info",
                                 MessagePriority = 2) %>%
                          Tracker.Make()

  # Add initial record and root subject counts to 'Report.Log'
  Report.Log <- Report.Log %>%
                    Log.Add(Log.Make(Report.Tracker),
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
#     - Create auxiliary data.frame containing all eligible 'root subjects' by merging 'Seed' table with all 'Root' tables (in case 'Root' does not only consist of 'Seed')
#     - Filter out any record that has missing values in features marked as required in meta data (thereby also removing 'rogue'/unlinked patient or diagnosis records because this way every patient needs to have at least one related diagnosis and vice versa)
#     - Note: Setting 'PrimaryKeyIgnoredInRedundancyCheck' on FALSE is crucial (e.g. because different DiagnosisIDs of same patient and diagnosis can e.g. be related to different Histologies).
#-------------------------------------------------------------------------------

  # The reduce()-call performs pair-wise left-joining of 'Seed' table with all 'Root' tables
  DataSetRoot <- reduce(.x = DataSet[RootTableNames[RootTableNames != SeedTableName]],      # All 'Root' tables that are not 'Seed'
                        .f = \(TableA, TableB) left_join(TableA, TableB, by = SeedPrimaryKey),
                        .init = DataSet[[SeedTableName]])

  # Add initial DataSetRoot record count to Tracker report
  Report.Tracker <- Report.Tracker %>%
                          Tracker.Add(Tracker.New(ProcessingStage = "Initial",
                                                            Table = ".DataSetRoot",
                                                            ProcessTopic = "Record count",
                                                            CountLevel = "Monitor",
                                                            CountRootSubjects.Prior = n_distinct(DataSetRoot[RootPrimaryKey]),
                                                            CountRecords.Prior = nrow(DataSetRoot),
                                                            Message = "DataSetRoot: Initial record count",
                                                            MessageClass = "Info",
                                                            MessagePriority = 2,
                                                            PrintMessage = FALSE))

  # Clean DataSetRoot
  if ((Settings$CurationProcess %>% filter(Table == ".DataSetRoot") %>% pull(PrimaryTableCleaning)) == TRUE)
  {
      # dsFreda::CleanTable() returns a list object with the elements 'Table', 'NonconformingRecords', 'Tracker' and 'Log'
      PrimaryTableCleaning.DataSetRoot <- dsFreda::CleanTable(Table = DataSetRoot,
                                                              TableName = ".DataSetRoot",
                                                              PrimaryKey = RootPrimaryKey,
                                                              RootSubjectKey = RootPrimaryKey,
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

      # Reassign DataSetRoot
      DataSetRoot <- PrimaryTableCleaning.DataSetRoot$Table

      # Save data.frame of non-conforming records in predefined list element
      NonconformingRecords[[".DataSetRoot"]] <- NonconformingRecords[[".DataSetRoot"]] %>%
                                                    bind_rows(PrimaryTableCleaning.DataSetRoot$NonconformingRecords)

      # Update TRACKER report
      Report.Tracker <- Report.Tracker %>%
                            bind_rows(PrimaryTableCleaning.DataSetRoot$Tracker %>% mutate(ProcessingStage = "Secondary Table Cleaning"))

      # Update LOG report
      Report.Log <- Report.Log %>%
                        bind_rows(PrimaryTableCleaning.DataSetRoot$Log %>% mutate(ProcessingStage = "Secondary Table Cleaning"))
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
                                                                       Tracker = NULL,
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
                                                                       Tracker = NULL,
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

                                          # Perform table cleaning (this returns a list object with the elements 'Table', 'NonconformingRecords', 'Tracker' and 'Log')
                                          CurrentTableCleaning <- dsFreda::CleanTable(Table = Table,
                                                                                      TableName = tablename,
                                                                                      PrimaryKey = PrimaryKeys[[tablename]],
                                                                                      RootSubjectKey = RootSubjectKeys[[tablename]],
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

                                          # Add processing stage to TRACKER and LOG
                                          CurrentTableCleaning$Tracker <- CurrentTableCleaning$Tracker %>%
                                                                              mutate(ProcessingStage = "Primary Table Cleaning")

                                          CurrentTableCleaning$Log <- CurrentTableCleaning$Log %>%
                                                                              mutate(ProcessingStage = "Primary Table Cleaning")
                                      }

                                      return(CurrentTableCleaning)
                                  })
                                  # .progress = list(name = "Primary table cleaning",
                                  #                  type = "iterator"))

  # Update TRACKER report
  Report.Tracker <- Report.Tracker %>%
                        bind_rows(PrimaryTableCleaning %>%
                                      map(\(CurrentTableCleaning) CurrentTableCleaning$Tracker) %>%
                                      list_rbind())

  # Update LOG report
  Report.Log <- Report.Log %>%
                    bind_rows(PrimaryTableCleaning %>%
                                  map(\(CurrentTableCleaning) CurrentTableCleaning$Log) %>%
                                  list_rbind())

  # Print message to mark reporting of record count changes
  PrintSoloMessage(c(Topic = "Record counts after Primary Table Cleaning"))

  # Assess new table record and root subject counts
  StageTracker <- DataSet %>%
                      dsFreda::TrackCounts(TransformationReturn = PrimaryTableCleaning,
                                           RootSubjectKeys = RootSubjectKeys,
                                           PrintMessages = TRUE) %>%
                      mutate(ProcessingStage = "Primary Table Cleaning",
                             ProcessTopic = "COUNT",
                             CountLevel = "Stage")

  # Update TRACKER report
  Report.Tracker <- Report.Tracker %>%
                          bind_rows(StageTracker)

  # Reassign DataSet
  DataSet <- PrimaryTableCleaning %>%
                  map(\(CurrentTableCleaning) CurrentTableCleaning$Table)

  # Save non-conforming records
  NonconformingRecords <- NonconformingRecords %>%
                              imap(\(CurrentNonconformingRecords, tablename) bind_rows(CurrentNonconformingRecords, PrimaryTableCleaning[[tablename]]$NonconformingRecords))



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
                                                                          Tracker = NULL,
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
                                                                          Tracker = NULL,
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
                                                                          Tracker = NULL,
                                                                          Log = Log.New(ProcessingStage = "Table Normalization",
                                                                                        Table = tablename,
                                                                                        ProcessTopic = "Table Normalization",
                                                                                        ProcessExecution = "Omitted",
                                                                                        Message = "Table is missing or empty.",
                                                                                        MessageClass = "Info",
                                                                                        PrintMessage = TRUE))
                                    # Main Case
                                    } else {

                                        # Perform table normalization operations (this returns a list object with the elements 'Table', 'Tracker' and 'Log')
                                        CurrentTableNormalization <- Table %>%
                                                                        dsFreda::NormalizeTable(TableName = tablename,
                                                                                                PrimaryKey = PrimaryKeys[[tablename]],
                                                                                                RootSubjectKey = RootSubjectKeys[[tablename]],
                                                                                                RuleSet = NormalizationRules,
                                                                                                PrintMessages = TRUE)

                                        # Add processing stage to TRACKER and LOG
                                        CurrentTableNormalization$Tracker <- CurrentTableNormalization$Tracker %>%
                                                                                  mutate(ProcessingStage = "Table Normalization")

                                        CurrentTableNormalization$Log <- CurrentTableNormalization$Log %>%
                                                                              mutate(ProcessingStage = "Table Normalization")
                                    }

                                    return(CurrentTableNormalization)
                                 })
                                 # .progress = list(name = "Table normalization",
                                 #                  type = "iterator"))

  # Update TRACKER report
  Report.Tracker <- Report.Tracker %>%
                        bind_rows(TableNormalization %>%
                                      map(\(CurrentTableNormalization) CurrentTableNormalization$Tracker) %>%
                                      list_rbind())

  # Update LOG report
  Report.Log <- Report.Log %>%
                    bind_rows(TableNormalization %>%
                                  map(\(CurrentTableNormalization) CurrentTableNormalization$Log) %>%
                                  list_rbind())

  # Print message to mark reporting of record count changes
  PrintSoloMessage(c(Topic = "Record counts after Table Normalization"))

  # Assess new table record and root subject counts
  StageTracker <- DataSet %>%
                      dsFreda::TrackCounts(TransformationReturn = TableNormalization,
                                           RootSubjectKeys = RootSubjectKeys,
                                           PrintMessages = TRUE) %>%
                      mutate(ProcessingStage = "Table Normalization",
                             ProcessTopic = "COUNT",
                             CountLevel = "Stage")

  # Update TRACKER report
  Report.Tracker <- Report.Tracker %>%
                          bind_rows(StageTracker)

  # Reassign DataSet
  DataSet <- TableNormalization %>%
                  map(\(CurrentTableNormalization) CurrentTableNormalization$Table)



#===============================================================================
# MODULE E)  Data Harmonization
#===============================================================================
#   The data harmonization process consists of the following steps:
#     - Data remediation
#     - Data recoding
#     - Data formatting
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
#     - Create meta data on eligible value sets of features to be tracked / monitored during curation process
#     - Element object syntax: List of vectors
#         - Vector names = Name of feature to be monitored during Curation (Transformation)
#         - Vector values = Set of eligible values obtained from meta data / passed rule set
#     - If a feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

  MonitorMetaData <- names(DataSet) %>%
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
  stopifnot("Internal ERROR: Names of 'MonitorMetaData' and 'DataSet' must be exactly aligned." = (all(names(MonitorMetaData) == names(DataSet))))

  # Initiate list of data.frames containing transformation tracks
  # First step: Store copied raw values of monitored features and mark them with 'TrackID' to track them along transformation process
  TransformationTracks <- map2(.x = DataSet,
                               .y = MonitorMetaData,
                               .f = function(Table, TableMonitorMetaData)
                                    {
                                        if (length(TableMonitorMetaData) == 0)
                                        {
                                            return(data.frame())

                                        } else {

                                            Table %>%
                                                 select(names(TableMonitorMetaData)) %>%
                                                 rename_with(.fn = ~ str_c(., "___Raw"),   # Three underscores for later use in pivot_longer()
                                                             .cols = everything()) %>%
                                                 mutate(.TrackID = row_number(), .before = 1)   # Create TrackID to enable correct mapping and further processing
                                       }
                                    })

  ValueCounts.Raw <- map2(.x = DataSet,
                          .y = MonitorMetaData,
                          .f = function(Table, TableMonitorMetaData)
                               {
                                  Table %>%
                                      dsFreda::TrackValueCounts(FeatureNames = names(TableMonitorMetaData),
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

                                  # Initiate table-specific reporting objects
                                  TableReport.Log <- NULL
                                  TableReport.DataRemediation.Details <- NULL
                                  TableReport.DataRemediation.Summary <- NULL
                                  TableReport.DataRemediation <- NULL

                                  # Get table-specific settings on data remediation process
                                  RemediationProcess <- Settings$DataRemediation$Process %>%
                                                              filter(Table == tablename,
                                                                     RunRemediation == TRUE) %>%
                                                              arrange(RemediationOrder)      # Defines the order in which features within a table are being remediated (this can be relevant in transformative espressions that contain inter-feature dependencies)

                                  # Check if there are any features in current table that should be remediated
                                  if (length(RemediationProcess) == 0 || nrow(RemediationProcess) == 0)
                                  {
                                      TableReport.Log <- Log.New(Table = tablename,
                                                                 ProcessTopic = "Transforming ineligible values",
                                                                 ProcessExecution = "Inapplicable",
                                                                 Message = "No procedures provided.",
                                                                 MessageClass = "Info",
                                                                 PrintMessage = TRUE)

                                  # Check in settings if normalization procedures should be executed for current table
                                  } else if ((Settings$CurationProcess %>% filter(Table == tablename) %>% pull(DataRemediation)) == FALSE) {

                                      TableReport.Log <- Log.New(Table = tablename,
                                                                 ProcessTopic = "Transforming ineligible values",
                                                                 ProcessExecution = "Omitted",
                                                                 Message = "Omitted",
                                                                 MessageClass = "Info",
                                                                 PrintMessage = TRUE)

                                  # Check if current table is missing or empty
                                  } else if (length(Table) == 0 || nrow(Table) == 0) {

                                      TableReport.Log <- Log.New(Table = tablename,
                                                                 ProcessTopic = "Transforming ineligible values",
                                                                 ProcessExecution = "Inapplicable",
                                                                 Message = "Table is missing or empty.",
                                                                 MessageClass = "Info",
                                                                 PrintMessage = TRUE)

                                  # Main Case
                                  } else {

                                      # Loop through table features that are supposed to be remediated
                                      for (featurename in RemediationProcess$Feature)
                                      {
                                          # Initiate feature-specific data remediation report
                                          FeatureReport.DataRemediation <- tibble(Feature = featurename,
                                                                                  CountValues.NonMissing = sum(!is.na(Table[[featurename]])))

                                          # Get set of eligible values for current feature
                                          EligibleValueSet <- MetaData$Values %>%
                                                                  filter(Table == tablename,
                                                                         FeatureName.Curated == featurename) %>%
                                                                  pull(Value.Raw)   # Eligible Values PRIOR to recoding

                                          # For TRACKING purposes: Count ineligible values in current feature PRIOR to data remediation
                                          CountValues.Ineligible.Prior = sum(!is.na(Table[[featurename]]) & Table[[featurename]] %notin% EligibleValueSet)

                                          # Check if there are no ineligible feature values needing remediation
                                          if (is.na(CountValues.Ineligible.Prior) || CountValues.Ineligible.Prior == 0)
                                          {
                                              # Update feature-specific data remediation report
                                              FeatureReport.DataRemediation <- FeatureReport.DataRemediation %>%
                                                                                      mutate(CountValues.Ineligible.Prior = CountValues.Ineligible.Prior,
                                                                                             Timestamp = Sys.time())

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
                                                                                             EligibleValueSet = EligibleValueSet,
                                                                                             TransformativeExpressions = TransformativeExpressions,
                                                                                             Dictionary = Dictionary,
                                                                                             FuzzyStringMatching = FuzzyStringMatching)

                                              # Calculate feature-specific data remediation report measures
                                              FeatureReport.DataRemediation <- FeatureReport.DataRemediation %>%
                                                                                      mutate(CountValues.Ineligible.Prior = CountValues.Ineligible.Prior,
                                                                                             CountValues.Ineligible.Post = sum(!is.na(Table[[featurename]]) & Table[[featurename]] %notin% EligibleValueSet, na.rm = TRUE),
                                                                                             CountValues.Remediated = CountValues.Ineligible.Prior - CountValues.Ineligible.Post,
                                                                                             ProportionValues.Remediated = CountValues.Remediated / CountValues.Ineligible.Prior,
                                                                                             Timestamp = Sys.time())
                                          }

                                          # Add feature-specific report to table REPORT DETAILS on executed data remediation
                                          TableReport.DataRemediation.Details <- TableReport.DataRemediation.Details %>%
                                                                                        bind_rows(FeatureReport.DataRemediation)
                                      }
                                      #--- End of feature-loop -----------------

                                      # Create table-specific summary from feature-specific data remediation reports
                                      TableReport.DataRemediation.Summary <- TableReport.DataRemediation.Details %>%
                                                                                    summarize(across(starts_with("CountValues"), ~ sum(.x, na.rm = TRUE))) %>%
                                                                                    mutate(ProportionValues.Remediated = ifelse(CountValues.Ineligible.Prior == 0,
                                                                                                                                NA,
                                                                                                                                CountValues.Remediated / CountValues.Ineligible.Prior),
                                                                                           Feature = ".All",
                                                                                           Timestamp = Sys.time())

                                      # Create table-specific DATA REMEDIATION REPORT
                                      TableReport.DataRemediation <- TableReport.DataRemediation.Summary %>%
                                                                            bind_rows(TableReport.DataRemediation.Details) %>%
                                                                            mutate(CountRecords = nrow(Table),
                                                                                   Table = tablename)

                                      # Create LOG REPORT SUMMARY
                                      TableReport.Log.Summary <- TableReport.DataRemediation.Summary %>%
                                                                      mutate(Table = tablename,
                                                                             ProcessTopic = "Transforming ineligible values",
                                                                             Message = ifelse(CountValues.Ineligible.Prior == 0,
                                                                                              "Table contained no ineligible values.",
                                                                                              paste0("Table contained a total of ",
                                                                                                     CountValues.Ineligible.Prior, " ineligible values of which ",
                                                                                                     CountValues.Remediated, " (", round(ProportionValues.Remediated * 100, 1), "%) were remediated.")),
                                                                             MessageClass = "Success") %>%
                                                                      select(Table,
                                                                             ProcessTopic,
                                                                             Message,
                                                                             MessageClass) %>%
                                                                      Log.Make(PrintMessage = TRUE)

                                      # Create LOG REPORT DETAILS
                                      TableReport.Log.Details <- TableReport.DataRemediation.Details %>%
                                                                      mutate(Table = tablename,
                                                                             ProcessTopic = "Transforming ineligible values",
                                                                             ProcessTopic.Subgroup = Feature,
                                                                             Message = ifelse(CountValues.Ineligible.Prior == 0,
                                                                                              paste0("Feature '", Feature, "' had no ineligible values."),
                                                                                              paste0("Feature '", Feature, "' had ",
                                                                                                     CountValues.Ineligible.Prior, " ineligible values of which ",
                                                                                                     CountValues.Remediated, " (", round(ProportionValues.Remediated * 100, 1), "%) were remediated.")),
                                                                             MessageClass = "Details.Success") %>%
                                                                      select(Table,
                                                                             ProcessTopic,
                                                                             ProcessTopic.Subgroup,
                                                                             Message,
                                                                             MessageClass) %>%
                                                                      Log.Make()

                                      # Create table-specific LOG REPORT
                                      TableReport.Log <- TableReport.Log.Summary %>%
                                                              bind_rows(TableReport.Log.Details) %>%
                                                              mutate(ProcessExecution = "Executed")
                                  }

                                  # Complement table-specific log report
                                  TableReport.Log <- TableReport.Log %>%
                                                          mutate(ProcessingStage = "Data Harmonization")

                                  #---------------------------------------------
                                  return(list(Table = Table,
                                              Report.Log = TableReport.Log,
                                              Report.DataRemediation = TableReport.DataRemediation))
                               })
                               # .progress = list(name = "Transforming ineligible values",
                               #                     type = "iterator"))

  # Reassign DataSet
  DataSet <- DataRemediation %>%
                  map(\(CurrentDataRemediation) CurrentDataRemediation$Table)

  # Extract LOG reports and bind them to main log report
  Report.Log <- Report.Log %>%
                    Log.Add(DataRemediation %>%
                                  map(\(CurrentDataRemediation) CurrentDataRemediation$Report.Log) %>%
                                  list_rbind())

  # Extract DATA REMDIATION REPORT data.frames and bind them together
  Report.DataRemediation <- Report.DataRemediation %>%
                                  bind_rows(DataRemediation %>%
                                                map(\(CurrentDataRemediation) CurrentDataRemediation$Report.DataRemediation) %>%
                                                list_rbind())


#===============================================================================
#   MODULE E 5)  Track feature values after Remediation
#===============================================================================

# Map raw values to their remediated state to get transformation tracks
#===============================================================================

  # Internal guard condition: Make sure lists have exactly the same element names (in aligned order)
  stopifnot("Internal ERROR: Names of 'TransformationTracks', 'MonitorMetaData' and 'DataSet' must be exactly aligned." = (all(names(TransformationTracks) == names(DataSet)) && (all(names(MonitorMetaData) == names(DataSet)))))

  TransformationTracks <- pmap(.l = list(TransformationTracks,
                                         DataSet,
                                         MonitorMetaData),
                               .f = function(CurrentTransformationTracks, RemediatedTable, TableMonitorMetaData)
                                    {
                                        if (length(TableMonitorMetaData) == 0)
                                        {
                                            return(data.frame())

                                        } else {

                                            RemediatedValues <- RemediatedTable %>%
                                                                     select(c(".TrackID", names(TableMonitorMetaData))) %>%
                                                                     rename_with(.fn = ~ str_c(., "___Remediated"),   # Three underscores for later use in pivot_longer()
                                                                                 .cols = all_of(names(TableMonitorMetaData)))

                                            CurrentTransformationTracks %>%
                                                left_join(RemediatedValues, by = join_by(.TrackID)) %>%
                                                distinct(pick(contains("___Raw")), .keep_all = TRUE)
                                       }
                                   })


# Get counts of all distinct values in remediated tables
#===============================================================================

  # Internal guard condition: Make sure two lists have exactly the same element names (aligned in order)
  stopifnot("Internal ERROR: Names of 'MonitorMetaData' and 'DataSet' must be exactly aligned." = (names(MonitorMetaData) == names(DataSet)))

  ValueCounts.Remediated <- map2(.x = DataSet,
                                 .y = MonitorMetaData,
                                 .f = function(Table, TableMonitorMetaData)
                                      {
                                          Table %>%
                                              dsFreda::TrackValueCounts(FeatureNames = names(TableMonitorMetaData),
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
#   MODULE E 7)  Data formatting
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
#   MODULE E 8)  Track feature values after Recoding
#===============================================================================

# Map raw values to their recoded state to get transformation tracks
#===============================================================================

  # Internal guard condition: Make sure lists have exactly the same element names (in aligned order)
  stopifnot("Internal ERROR: Names of 'TransformationTracks', 'MonitorMetaData' and 'DataSet' must be exactly aligned." = (all(names(TransformationTracks) == names(DataSet)) && (all(names(MonitorMetaData) == names(DataSet)))))

  TransformationTracks <- pmap(.l = list(TransformationTracks,
                                         DataSet,
                                         MonitorMetaData),
                               .f = function(TableTransformationTracks, RecodedTable, TableMonitorMetaData)
                                    {
                                        if (length(TableMonitorMetaData) == 0)
                                        {
                                            return(data.frame())

                                        } else {

                                            RecodedValues <- RecodedTable %>%
                                                                 select(c(".TrackID", names(TableMonitorMetaData))) %>%
                                                                 rename_with(.fn = ~ str_c(., "___Recoded"),   # Three underscores for later use in pivot_longer()
                                                                             .cols = all_of(names(TableMonitorMetaData)))

                                            TableTransformationTracks %>%
                                                left_join(RecodedValues,
                                                          by = join_by(.TrackID)) %>%
                                                distinct(pick(contains("___Raw")), .keep_all = TRUE)
                                        }
                                   })


# Get counts of all distinct values in recoded data sets
#===============================================================================

  # Internal guard condition: Make sure two lists have exactly the same element names (aligned in order)
  stopifnot("Internal ERROR: Names of 'MonitorMetaData' and 'DataSet' must be exactly aligned." = (all(names(MonitorMetaData) == names(DataSet))))

  ValueCounts.Recoded <- map2(.x = DataSet,
                              .y = MonitorMetaData,
                              .f = function(Table, TableMonitorMetaData)
                                   {
                                      Table %>%
                                         dsFreda::TrackValueCounts(FeatureNames = names(TableMonitorMetaData),
                                                                   TransformationStage = "Recoded") %>%
                                         select(Feature,
                                                Value,
                                                Frequency) %>%
                                         rename(Value.Recoded = Value,
                                                Count.Recoded = Frequency)
                                   })



#===============================================================================
#   MODULE E 9)  Finalize data remediation using dsFreda::FinalizeDataRemediation()
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
                                                Report.Finalization <- tibble()

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
#   MODULE E 10)  Track feature values after finalized Remediation
#===============================================================================

# Map raw values to their finalized state to get transformation tracks
#===============================================================================

  # Internal guard condition: Make sure lists have exactly the same element names (in aligned order)
  stopifnot("Internal ERROR: Names of 'TransformationTracks', 'MonitorMetaData' and 'DataSet' must be exactly aligned." = (all(names(TransformationTracks) == names(DataSet)) && (all(names(MonitorMetaData) == names(DataSet)))))

  TransformationTracks <- pmap(.l = list(TransformationTracks,
                                         DataSet,
                                         MonitorMetaData),
                               .f = function(TableTransformationTracks, FinalizedTable, TableMonitorMetaData)
                                    {
                                        if (length(TableMonitorMetaData) == 0)
                                        {
                                            return(data.frame())

                                        } else {

                                            FinalizedValues <- FinalizedTable %>%
                                                                   select(c(".TrackID", names(TableMonitorMetaData))) %>%
                                                                   rename_with(.fn = ~ str_c(., "___Final"),   # Three underscores for later use in pivot_longer()
                                                                               .cols = all_of(names(TableMonitorMetaData)))

                                            TableTransformationTracks %>%
                                                left_join(FinalizedValues,
                                                          by = join_by(.TrackID)) %>%
                                                distinct(pick(contains("___Raw")), .keep_all = TRUE)
                                        }
                                    })


# Get counts of all distinct values in finalized data sets
#===============================================================================

  # Internal guard condition: Make sure two lists have exactly the same element names (aligned in order)
  stopifnot("Internal ERROR: Names of 'MonitorMetaData' and 'DataSet' must be exactly aligned." = (all(names(MonitorMetaData) == names(DataSet))))

  ValueCounts.Final <- map2(.x = DataSet,
                            .y = MonitorMetaData,
                            .f = function(Table, TableMonitorMetaData)
                                 {
                                     Table %>%
                                        dsFreda::TrackValueCounts(FeatureNames = names(TableMonitorMetaData),
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
                                                   MonitorMetaData),
                                         .f = function(TableTransformationTracks,
                                                       TableMonitorMetaData)
                                              {
                                                  if (length(TableTransformationTracks) == 0 || nrow(TableTransformationTracks) == 0)
                                                  {
                                                      return(data.frame())

                                                  } else {

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
                                                                     rowwise() %>%
                                                                     mutate(IsOccurring = TRUE,
                                                                            IsEligible.Raw = ifelse(is.na(Value.Raw) | is.null(TableMonitorMetaData[[Feature]]),      # If value is NA or if there is no set of eligible values, set variable NA...
                                                                                                    NA,
                                                                                                    Value.Raw %in% TableMonitorMetaData[[Feature]]$Value.Raw),      # ... else check if specific row value is in set of eligible values
                                                                            IsEligible.Remediated = ifelse(is.na(Value.Remediated) | is.null(TableMonitorMetaData[[Feature]]),
                                                                                                           NA,
                                                                                                           Value.Remediated %in% TableMonitorMetaData[[Feature]]$Value.Raw),
                                                                            IsEligible.Recoded = ifelse(is.na(Value.Recoded) | is.null(TableMonitorMetaData[[Feature]]),
                                                                                                        NA,
                                                                                                        Value.Recoded %in% TableMonitorMetaData[[Feature]]$Value.Curated),
                                                                            IsEligible.Final = ifelse(is.na(Value.Final),
                                                                                                      NA,
                                                                                                      TRUE)) %>%
                                                                     ungroup()

                                                      # Add set of all eligible values regardless of occurrence to summary
                                                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                      for (i in 1:length(TableMonitorMetaData))   # Loop through all monitored features of a table
                                                      {
                                                         AllEligibleValues <- tibble(Feature = names(TableMonitorMetaData)[i],
                                                                                     Value.Raw = TableMonitorMetaData[[i]]$Value.Raw,
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


# Create detailed transformation monitors
#===============================================================================
#   - Joining of info from transformation tracks and value counts
#-------------------------------------------------------------------------------
  TransformationMonitors <- pmap(.l = list(TransformationTracks.Summaries,
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
                                                                                     TRUE ~ Count.Remediated),
                                                        Count.Recoded = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                  TRUE ~ Count.Recoded),
                                                        Count.Final = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                TRUE ~ Count.Final)) %>%
                                                 arrange(Feature,
                                                         desc(IsOccurring),
                                                         desc(IsEligible.Raw),
                                                         desc(IsEligible.Remediated),
                                                         Value.Raw)
                                         }
                                      })


# Create overview of value eligibility in different transformation stages
#===============================================================================
  TransformationMonitors.Overviews <- pmap(.l = list(TransformationMonitors,
                                                     MonitorMetaData),
                                           .f = function(TableTransformationMonitor,
                                                         TableMonitorMetaData)
                                                {
                                                    if (length(TableTransformationMonitor) == 0 || nrow(TableTransformationMonitor) == 0)
                                                    {
                                                        return(data.frame())

                                                    } else {

                                                        # Filter out features that are not meant to be monitored, e.g. do not have applicable eligibility criteria
                                                        TableTransformationMonitor <- TableTransformationMonitor %>%
                                                                            filter(Feature %in% names(TableMonitorMetaData))

                                                        SummaryRaw <- TableTransformationMonitor %>%
                                                                          group_by(Feature, IsEligible.Raw) %>%
                                                                              summarize(Count.Raw = sum(Count.Raw, na.rm = TRUE)) %>%
                                                                              rename(Eligibility = IsEligible.Raw)

                                                        SummaryRemediated <- TableTransformationMonitor %>%
                                                                                  distinct(pick(Feature, Value.Remediated), .keep_all = TRUE) %>%
                                                                                  group_by(Feature, IsEligible.Remediated) %>%
                                                                                      summarize(Count.Remediated = sum(Count.Remediated, na.rm = TRUE)) %>%
                                                                                      rename(Eligibility = IsEligible.Remediated)

                                                        SummaryRecoded <- TableTransformationMonitor %>%
                                                                              distinct(pick(Feature, Value.Recoded), .keep_all = TRUE) %>%
                                                                              group_by(Feature, IsEligible.Recoded) %>%
                                                                                  summarize(Count.Recoded = sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                                  rename(Eligibility = IsEligible.Recoded)

                                                        SummaryFinal <- TableTransformationMonitor %>%
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
                                                                        mutate(across(c(Count.Raw, Count.Remediated, Count.Recoded, Count.Final), ~ case_when(is.na(.x) ~ 0, .default = .x))) %>%       # Turn all NAs into 0 in count columns
                                                                        group_by(Feature) %>%
                                                                            mutate(across(c(Count.Raw, Count.Remediated, Count.Recoded, Count.Final), ~ .x / sum(.x), .names = "Proportion.{.col}")) %>%      # Create proportional value columns
                                                                        ungroup() %>%
                                                                        arrange(Feature, factor(Eligibility, levels = c("Eligible", "Ineligible", "Missing")))
                                                    }
                                                })


# Create overview of value eligibility in different transformation stages
#===============================================================================
  TransformationMonitors.FullValueSets <- TransformationMonitors %>%
                                              map(function(TableTransformationMonitor)
                                                  {
                                                      if (length(TableTransformationMonitor) == 0 || nrow(TableTransformationMonitor) == 0)
                                                      {
                                                          return(list())

                                                      } else {

                                                          ValueSets <- list()

                                                          ValueSets$Raw <- TableTransformationMonitor %>%
                                                                                select(Feature, Value.Raw, IsOccurring, IsEligible.Raw, Count.Raw) %>%
                                                                                group_by(Feature) %>%
                                                                                    mutate(Proportion.Raw = Count.Raw / sum(Count.Raw, na.rm = TRUE)) %>%
                                                                                ungroup()

                                                          ValueSets$Remediated <- TableTransformationMonitor %>%
                                                                                      group_by(Feature, Value.Remediated, IsEligible.Remediated) %>%
                                                                                          summarize(Count.Remediated = sum(Count.Remediated, na.rm = TRUE)) %>%
                                                                                      ungroup() %>%
                                                                                      distinct(Feature, Value.Remediated, .keep_all = TRUE) %>%
                                                                                      group_by(Feature) %>%
                                                                                          mutate(Proportion.Remediated = Count.Remediated / sum(Count.Remediated, na.rm = TRUE)) %>%
                                                                                      ungroup()

                                                          ValueSets$Recoded <- TableTransformationMonitor %>%
                                                                                    group_by(Feature, Value.Recoded, IsEligible.Recoded) %>%
                                                                                        summarize(Count.Recoded = sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                                    ungroup() %>%
                                                                                    distinct(Feature, Value.Recoded, .keep_all = TRUE) %>%
                                                                                    group_by(Feature) %>%
                                                                                        mutate(Proportion.Recoded = Count.Recoded / sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                                    ungroup()

                                                          ValueSets$Final <- TableTransformationMonitor %>%
                                                                                  group_by(Feature, Value.Final, IsEligible.Final) %>%
                                                                                      summarize(Count.Final = sum(Count.Final, na.rm = TRUE)) %>%
                                                                                  ungroup() %>%
                                                                                  distinct(Feature, Value.Final, .keep_all = TRUE) %>%
                                                                                  group_by(Feature) %>%
                                                                                      mutate(Proportion.Final = Count.Final / sum(Count.Final, na.rm = TRUE)) %>%
                                                                                  ungroup()

                                                          return(ValueSets)
                                                      }
                                                  })


# Delete artificial 'TrackID'-column from data.frames (not needed anymore)
#===============================================================================

  DataSet <- DataSet %>%
                  map(function(Table)
                      {
                          try(Table %>% select(-.TrackID))
                      })

  # Print info message
  cli::cat_bullet("Data transformation monitors are stored in 'Report$DataHarmonization$TransformationMonitors'", bullet = "info")
  cat("\n")



#===============================================================================
#   MODULE E 12)  Perform data harmonization (remediation, recoding, formatting) on data.frames of non-conforming records (without tracking / reporting)
#===============================================================================

  NonconformingRecords <- NonconformingRecords %>%
                              imap(function(Table, tablename)
                                   {
                                      #---------------------------------------------
                                      # Check if data remediation was performed on source table and if it should be performed on non-conforming records
                                      #---------------------------------------------
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

                                      #---------------------------------------------
                                      # In any case: Recode / Format / Re-type data in 'NonconformingRecords' (so that appending more records later does not result in compatibility errors)
                                      #---------------------------------------------

                                      if (length(Table) > 0 && nrow(Table) > 0)
                                      {
                                          # Recode
                                          #-----------------------------------------
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

                                          # Format / Re-type table data as defined in meta data
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
                                      }

                                      #---------------------------------------------
                                      # Check if remediation finalization of non-conforming records is intended and applicable for current table
                                      #---------------------------------------------
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
      # dsFreda::CleanTable() returns a list with the elements 'Table', 'NonconformingRecords', 'Tracker' and 'Log'
      SecondaryTableCleaning.DataSetRoot <- dsFreda::CleanTable(Table = DataSetRoot,
                                                                TableName = ".DataSetRoot",
                                                                PrimaryKey = RootPrimaryKey,
                                                                RootSubjectKey = RootPrimaryKey,
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

      # Reassign DataSetRoot
      DataSetRoot <- SecondaryTableCleaning.DataSetRoot$Table

      # Save non-conforming records
      NonconformingRecords[[".DataSetRoot"]] <- NonconformingRecords[[".DataSetRoot"]] %>%
                                                    bind_rows(SecondaryTableCleaning.DataSetRoot$NonconformingRecords)

      # Update TRACKER report
      Report.Tracker <- Report.Tracker %>%
                            bind_rows(SecondaryTableCleaning.DataSetRoot$Tracker %>% mutate(ProcessingStage = "Secondary Table Cleaning"))

      # Update LOG report
      Report.Log <- Report.Log %>%
                        bind_rows(SecondaryTableCleaning.DataSetRoot$Log %>% mutate(ProcessingStage = "Secondary Table Cleaning"))
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
                                                                         Tracker = NULL,
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
                                                                         Tracker = NULL,
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

                                            # Add processing stage to TRACKER and LOG
                                            CurrentTableCleaning$Tracker <- CurrentTableCleaning$Tracker %>%
                                                                                mutate(ProcessingStage = "Secondary Table Cleaning")

                                            CurrentTableCleaning$Log <- CurrentTableCleaning$Log %>%
                                                                            mutate(ProcessingStage = "Secondary Table Cleaning")
                                        }

                                        return(CurrentTableCleaning)
                                    })
                                    # .progress = list(name = "Secondary table cleaning",
                                    #                  type = "iterator"))

  # Update TRACKER report
  Report.Tracker <- Report.Tracker %>%
                        bind_rows(SecondaryTableCleaning %>%
                                      map(\(CurrentTableCleaning) CurrentTableCleaning$Tracker) %>%
                                      list_rbind())

  # Update LOG report
  Report.Log <- Report.Log %>%
                    bind_rows(SecondaryTableCleaning %>%
                                  map(\(CurrentTableCleaning) CurrentTableCleaning$Log) %>%
                                  list_rbind())

  # Print message to mark reporting of record count changes
  PrintSoloMessage(c(Topic = "Record counts after Secondary Table Cleaning"))

  # Assess new table record and root subject counts
  StageTracker <- DataSet %>%
                      dsFreda::TrackCounts(TransformationReturn = SecondaryTableCleaning,
                                           RootSubjectKeys = RootSubjectKeys,
                                           PrintMessages = TRUE) %>%
                      mutate(ProcessingStage = "Secondary Table Cleaning",
                             ProcessTopic = "COUNT",
                             CountLevel = "Stage")

  # Update TRACKER report
  Report.Tracker <- Report.Tracker %>%
                          bind_rows(StageTracker)

  # Reassign DataSet
  DataSet <- SecondaryTableCleaning %>%
                  imap(\(CurrentTableCleaning, tablename) CurrentTableCleaning$Table)

  # Save non-conforming records
  NonconformingRecords <- NonconformingRecords %>%
                              imap(\(CurrentNonconformingRecords, tablename) bind_rows(CurrentNonconformingRecords, SecondaryTableCleaning[[tablename]]$NonconformingRecords))



#===============================================================================
# MODULE G)  RECORD SUBSUMPTION (Find table records that can be considered redundant if they provide no additional informational value compared to a previous record)
#===============================================================================
# 1)  Process data set root tables first, since other tables hold primary key
#     Any DiagnosisIDs that are removed due to redundancy need to be replaced in dependent tables.
# 2)  Proceed with all other tables (excluding 'Patient')
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
                                                                       Tracker = NULL,
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
                                                                       Tracker = NULL,
                                                                       Log = Log.New(ProcessingStage = "Record Subsumption",
                                                                                     Table = tablename,
                                                                                     ProcessTopic = "Record subsumption",
                                                                                     ProcessExecution = "Inapplicable",
                                                                                     Message = "Table is missing or empty.",
                                                                                     MessageClass = "Info",
                                                                                     PrintMessage = TRUE))

                                  # Main Case
                                  } else {

                                      # Perform RECORD SUBSUMPTION (this returns a list object with the elements 'Table', 'NonconformingRecords', 'Tracker' and 'Log')
                                      CurrentRecordSubsumption <- dsFreda::SubsumeRecords(Table = Table,
                                                                                          TableName = tablename,
                                                                                          PrimaryKey = PrimaryKeys[[tablename]],
                                                                                          RootSubjectKey = RootSubjectKeys[[tablename]],
                                                                                          SubsumptionRedundancies.Detect = Settings$RecordSubsumption %>% filter(Table == tablename) %>% pull(SubsumptionRedundancies.Detect),
                                                                                          SubsumptionRedundancies.Remove = Settings$RecordSubsumption %>% filter(Table == tablename) %>% pull(SubsumptionRedundancies.Remove),
                                                                                          SubsumptionRedundancies.DistinctiveFeatures = MetaData$Features %>% filter(TableName.Curated == tablename, Subsumption.IsDistinctive == TRUE) %>% pull(FeatureName.Curated),
                                                                                          SubsumptionRedundancies.NegligibleFeatures = MetaData$Features %>% filter(TableName.Curated == tablename, Subsumption.IsNegligible == TRUE) %>% pull(FeatureName.Curated),
                                                                                          SubsumptionRedundancies.NegligibleValues = Settings$DataRemediation$Process %>% filter(Table == tablename) %>% select(Feature, UnremediatedValues.Substitution) %>% tibble::deframe() %>% as.list(),
                                                                                          PrintMessages = TRUE)

                                      # Add processing stage to TRACKER and LOG
                                      CurrentRecordSubsumption$Tracker <- CurrentRecordSubsumption$Tracker %>%
                                                                              mutate(ProcessingStage = "Record Subsumption")

                                      CurrentRecordSubsumption$Log <- CurrentRecordSubsumption$Log %>%
                                                                          mutate(ProcessingStage = "Record Subsumption")
                                  }

                                  return(CurrentRecordSubsumption)
                               }

  # Record Subsumption in data set ROOT tables, excluding 'Seed' table
  RecordSubsumption.Root <- DataSet[RootTableNames[RootTableNames != SeedTableName]] %>%
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
                                                    Tracker = CurrentRecordSubsumption$Tracker,
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

                                            for (keyfeature in TableRootSubjectKey)
                                            {
                                                if (length(RootSubjectKeyMapping) > 0 && keyfeature %in% names(RootSubjectKeyMapping))
                                                {
                                                    CurrentRootSubjectKeyMapping <- RootSubjectKeyMapping[[keyfeature]]
                                                    if (length(CurrentRootSubjectKeyMapping) > 0 && nrow(CurrentRootSubjectKeyMapping))
                                                    {
                                                        Table <- Table %>%
                                                                    left_join(CurrentRootSubjectKeyMapping, by = join_by(!!!syms(TableRootSubjectKey))) %>%
                                                                    mutate(.ReferenceRootSubjectKey = .data[[paste0(".Reference.", keyfeature)]],
                                                                           !!sym(keyfeature) := ifelse(!is.na(.ReferenceRootSubjectKey),
                                                                                                       .ReferenceRootSubjectKey,
                                                                                                       .data[[keyfeature]])) %>%
                                                                    select(-.ReferenceRootSubjectKey)
                                                    }
                                                }
                                            }

                                            # 2) Perform RECORD SUBSUMPTION by calling predefined process (see above)
                                            CurrentRecordSubsumption <- Process.RecordSubsumption(Table, tablename)

                                            return(CurrentRecordSubsumption)
                                         })

  # Update TRACKER report
  Report.Tracker <- Report.Tracker %>%
                        bind_rows(c(RecordSubsumption.Root,
                                    RecordSubsumption.Branches) %>%
                                        map(\(CurrentRecordSubsumption) CurrentRecordSubsumption$Tracker) %>%
                                        list_rbind())

  # Update LOG report
  Report.Log <- Report.Log %>%
                    bind_rows(c(RecordSubsumption.Root,
                                RecordSubsumption.Branches) %>%
                                    map(\(CurrentRecordSubsumption) CurrentRecordSubsumption$Log) %>%
                                    list_rbind())

  # Print message to mark reporting of record count changes
  PrintSoloMessage(c(Topic = "Record counts after Record Subsumption"))

  # Assess new table record and root subject counts
  StageTracker.Root <- DataSet[RootTableNames[RootTableNames != SeedTableName]] %>%
                            dsFreda::TrackCounts(TransformationReturn = RecordSubsumption.Root,
                                                 RootSubjectKeys = RootSubjectKeys,
                                                 PrintMessages = TRUE)

  StageTracker.Branches <- DataSet[BranchTableNames] %>%
                                dsFreda::TrackCounts(TransformationReturn = RecordSubsumption.Branches,
                                                     RootSubjectKeys = RootSubjectKeys,
                                                     PrintMessages = TRUE)

  StageTracker <- bind_rows(StageTracker.Root,
                            StageTracker.Branches) %>%
                      mutate(ProcessingStage = "Record Subsumption",
                             ProcessTopic = "COUNT",
                             CountLevel = "Stage")

  # Update TRACKER report
  Report.Tracker <- Report.Tracker %>%
                        bind_rows(StageTracker)

  # Reassign DataSet ROOT tables (excl. 'Seed' table, which was not processed)
  DataSet[RootTableNames[RootTableNames != SeedTableName]] <- RecordSubsumption.Root %>%
                                                                  imap(\(CurrentRecordSubsumption, tablename) CurrentRecordSubsumption$Table)

  # Reassign DataSet BRANCH tables
  DataSet[BranchTableNames] <- RecordSubsumption.Branches %>%
                                    imap(\(CurrentRecordSubsumption, tablename) CurrentRecordSubsumption$Table)

  # Save non-conforming records
  NonconformingRecords[RootTableNames[RootTableNames != SeedTableName]] <- NonconformingRecords[RootTableNames[RootTableNames != SeedTableName]] %>%
                                                                                imap(\(CurrentNonconformingRecords, tablename) bind_rows(CurrentNonconformingRecords, RecordSubsumption.Root[[tablename]]$NonconformingRecords))

  NonconformingRecords[BranchTableNames] <- NonconformingRecords[BranchTableNames] %>%
                                                imap(\(CurrentNonconformingRecords, tablename) bind_rows(CurrentNonconformingRecords, RecordSubsumption.Branches[[tablename]]$NonconformingRecords))



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
                                              filter(.HasBeenRemoved == TRUE,
                                                     is.na(.IsArtificial) | .IsArtificial == FALSE)


#===============================================================================
# Final DataSet modifications
#===============================================================================

  # Conversion of Tables from tibble to data.frame, because DataSHIELD can handle data.frames better
  DataSet <- DataSet %>%
                  map(\(Table) as.data.frame(Table))

  NonconformingRecords <- NonconformingRecords %>%
                              map(\(Table) as.data.frame(Table))


#===============================================================================
# Rearrange content of 'Report.Tracker'
#===============================================================================

  # Impute missing values in 'Report.Tracker'
  Report.Tracker <- Report.Tracker %>%
                        mutate(CountRecords.Removed = case_when(is.na(CountRecords.Removed) & !is.na(CountRecords.Added) & !is.na(Change.CountRecords) ~ abs(Change.CountRecords) - CountRecords.Added,
                                                                is.na(CountRecords.Removed) & !is.na(Change.CountRecords) ~ abs(Change.CountRecords),
                                                                is.na(CountRecords.Removed) ~ 0,
                                                                .default = CountRecords.Removed),
                               CountRecords.Added = case_when(is.na(CountRecords.Added) ~ 0,
                                                              .default = CountRecords.Added),
                               Change.CountRecords = case_when(is.na(Change.CountRecords) ~ CountRecords.Added - CountRecords.Removed,
                                                               .default = Change.CountRecords)) %>%
                        split(.$Table) %>%
                        map(function(TableReport)
                            {
                                View.Stages <- TableReport %>%
                                                    filter(CountLevel == "Stage")

                                View.Details <- TableReport %>%
                                                    filter(CountLevel != "Stage")

                                list(Stages = View.Stages,
                                     Details = View.Details)
                            })



  # Comb <- tidyr::expand_grid(ProcessingStage = c("Initial",
  #                                                "PrimaryTableCleaning",
  #                                                "TableNormalization",
  #                                                "SecondaryTableCleaning",
  #                                                "RecordSubsumption"),
  #                            Table = TableNames)
  #
  #
  #
  # Report.RecordCounts <- Report.Log %>%
  #                             filter(IsRelevantForRecordCount == TRUE)
                              # split(.$Table) %>%
                              # map(\(TableReport) split(TableReport, TableReport$RecordCountLevel))

  # Report.RecordCounts <- Report.RecordCounts %>%
  #                             select(-Message,
  #                                    -MessageClass) %>%
  #                             filter(ReportType %in% c("Stage", "Details")) %>%
  #                             split(.$Table) %>%
  #                             map(\(TableReport) split(TableReport, TableReport$ReportType))


#===============================================================================
# Compile content of main 'Report' object
#===============================================================================

  Report <- list(Settings = NULL,      # TO DO: Report chosen settings
                 Log = Report.Log,
                 Tracker = Report.Tracker,
                 DataHarmonization = list(DataRemediation = Report.DataRemediation,
                                          TransformationMonitors = TransformationMonitors,
                                          TransformationMonitors.Overviews = TransformationMonitors.Overviews,
                                          TransformationMonitors.ValueSet = TransformationMonitors.FullValueSets))

#===============================================================================
# Report completion of curation process
#===============================================================================

  CompletionCheck <- "green"

  # Log Report: Completion message
  Report.Log <- Report.Log %>%
                    Log.Add(Log.New(ProcessingStage = "General",
                                    Message = "Data Curation performed successfully!",
                                    MessageClass = "Special",
                                    PrintMessage = TRUE))

  # },
  #
  # # In case of occurring warning:
  # #===============================================================================
  # warning = function(w)
  #           {
  #               CompletionCheck <- "yellow"
  #               Messages$FinalMessage <- paste0("Completed Curation with following warning: \n", w)
  #           },
  #
  # # In case of occurring error:
  # #===============================================================================
  # error = function(e)
  #         {
  #             CompletionCheck <- "red"
  #             Messages$FinalMessage <- paste0("An error occured: \n", e)
  #         },
  #
  # #===============================================================================
  # # RETURN STATEMENT
  # #===============================================================================
  # finally =
  # {
  #   # Return the Curated Data Set (CDS), a Report object (defined above) and Messages
    return(list(CuratedDataSet = DataSet,
                NonconformingRecords = NonconformingRecords,
                AffectedRootSubjects = list(HadNonconformingRecords = AffectedRootSubjects,
                                            HadRemovedRecords = AffectedRootSubjects.RemovedRecords),
                Report = Report,
                CompletionCheck = CompletionCheck))
  # })

}

