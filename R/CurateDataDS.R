
#' CurateDataDS
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Transforms Raw Data Set (RDS) into Curated Data Set (CDS) while tracking data set transformation.
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{character} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#' @param MetaData.S \code{list} - A \code{list} of \code{data.frames} describing the structural and semantic characteristics of a hierarchical data model.
#'            \itemize{ \item \code{Meta.Tables}
#'                      \item \code{Meta.Features}
#'                      \item \code{Meta.Values}}
#' @param Proc.TableNormalization.S \code{data.frame} - Default: \code{dsCCPhos::Proc.TableNormalization}
#' @param Proc.TableNormalization.Profile.S \code{string} - Profile name defining rule set to be used for table normalization. Profile name must be stated in \code{TableNormalization$RuleSet} - Default: 'Default'
#' @param Set.CurationProcess.S \code{data.frame}
#' @param Set.CurationProcess.Profile.S \code{string}
#' @param Set.DataHarmonization.S \code{data.frame} - Default: \code{dsCCPhos::Set.DataHarmonization}
#' @param Set.DataHarmonization.Profile.S \code{string} - Profile used in \emph{DataHarmonization} - Default: 'Default'
#' @param Set.DataHarmonization.TransformativeExpressions.S \code{data.frame} - Default: \code{dsCCPhos::Set.TransformativeExpressions}
#' @param Set.DataHarmonization.TransformativeExpressions.Profile.S \code{string} - Profile used in \emph{TransformativeExpressions} - Default: 'Default'
#' @param Set.DataHarmonization.Dictionary.S \code{data.frame} - Default: \code{dsCCPhos::Set.Dictionary}
#' @param Set.DataHarmonization.Dictionary.Profile.S \code{string} - Profile used in \emph{Dictionary} - Default: 'Default'
#' @param Set.DataHarmonization.FuzzyStringMatching.S \code{data.frame} - Default: \code{dsCCPhos::Set.FuzzyStringMatching}
#' @param Set.DataHarmonization.FuzzyStringMatching.Profile.S \code{string} - Profile used in \emph{FuzzyStringMatching} - Default: 'Default'
#' @param Set.DataHarmonization.ExcludeIneligibleValues.S \code{logical} - Default: \code{TRUE}
#' @param Set.FeatureRequirements.S \code{data.frame} - Default: \code{dsCCPhos::Set.FeatureRequirements}
#' @param Set.FeatureRequirements.Profile.S \code{string} - Profile name defining strict and trans-feature requirements - Default: 'Default'
#' @param Set.FeatureTracking.S \code{data.frame} - Default: \code{dsCCPhos::Set.FeatureTracking}
#' @param Set.FeatureTracking.Profile.S \code{string} - Profile name defining which features should be tracked/monitored during curation process. Profile name must be stated in \code{FeatureTracking$RuleSet} - Default: 'Default'
#' @param Set.PrimaryTableCleaning.S \code{data.frame} - Default: \code{dsCCPhos::Set.PrimaryTableCleaning}
#' @param Set.PrimaryTableCleaning.Profile.S \code{logical} - Default: 'Default'
#' @param Set.SecondaryTableCleaning.S \code{data.frame} - Default: \code{dsCCPhos::Set.SecondaryTableCleaning}
#' @param Set.SecondaryTableCleaning.Profile.S \code{logical} - Default: 'Default'
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
#'                  \item CurationReport \code{list}
#'                      \itemize{ \item RecordCounts \code{tibble}
#'                                    \itemize{ \item Table
#'                                              \item InitialCount
#'                                              \item ExcludedPrimary
#'                                              \item AfterPrimaryExclusion
#'                                              \item ExcludedSecondary
#'                                              \item AfterSecondaryExclusion
#'                                              \item ExcludedSecondaryRedundancy
#'                                              \item AfterSecondaryRedundancyExclusion}
#'                                \item Transformation (list of lists)
#'                                    \itemize{ \item Monitors
#'                                              \item EligibilityOverviews
#'                                              \item ValueSetOverviews}}
#'                  \item Messages \code{list}}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CurateDataDS <- function(RawDataSetName.S = "RawDataSet",
                         MetaData.S = list(Tables = dsCCPhos::Meta.Tables,
                                           Features = dsCCPhos::Meta.Features,
                                           Values = dsCCPhos::Meta.Values),
                         Proc.TableNormalization.S = dsCCPhos::Proc.TableNormalization,
                         Proc.TableNormalization.Profile.S = "Default",
                         Set.CurationProcess.S = dsCCPhos::Set.CurationProcess,
                         Set.CurationProcess.Profile.S = "Default",
                         Set.DataHarmonization.S = dsCCPhos::Set.DataHarmonization,
                         Set.DataHarmonization.Profile.S = "Default",
                         Set.DataHarmonization.TransformativeExpressions.S = dsCCPhos::Set.TransformativeExpressions,
                         Set.DataHarmonization.TransformativeExpressions.Profile.S = "Default",
                         Set.DataHarmonization.Dictionary.S = dsCCPhos::Set.Dictionary,
                         Set.DataHarmonization.Dictionary.Profile.S = "Default",
                         Set.DataHarmonization.FuzzyStringMatching.S = dsCCPhos::Set.FuzzyStringMatching,
                         Set.DataHarmonization.FuzzyStringMatching.Profile.S = "Default",
                         Set.DataHarmonization.ExcludeIneligibleValues.S = TRUE,
                         Set.FeatureRequirements.S = dsCCPhos::Set.FeatureRequirements,
                         Set.FeatureRequirements.Profile.S = "Default",
                         Set.FeatureTracking.S = dsCCPhos::Set.FeatureTracking,
                         Set.FeatureTracking.Profile.S = "Default",
                         Set.PrimaryTableCleaning.S = dsCCPhos::Set.PrimaryTableCleaning,
                         Set.PrimaryTableCleaning.Profile.S = "Default",
                         Set.SecondaryTableCleaning.S = dsCCPhos::Set.SecondaryTableCleaning,
                         Set.SecondaryTableCleaning.Profile.S = "Default")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{

#===============================================================================
# - OVERVIEW -
#===============================================================================
#
#   SETUP
#
#   MODULE A)  Harmonization of Raw Data Set meta data and structure
#     1)  Transform table names
#     2)  Add empty tables in data set if they are missing in raw data
#     3)  Rename features
#     4)  In tables with missing features, add empty features accordingly
#
#   MODULE B)  Primary Table Cleaning
#     1)  Remove records that are not linked to related tables
#     2)  Remove duplicate records
#     3)  Remove records in RDS missing required features (defined in meta data or passed as optional argument)
#
#   MODULE C)  Table Normalization
#     1)  'Split and expand' where necessary (as determined by arguments / meta data)
#
#   MODULE D)  Data Harmonization / Transformation
#     1) Definition of features to monitor during Transformation
#     2) Tracking of raw feature values
#     3) Data Harmonization with
#        3.1) Transformative Expressions
#        3.2) Dictionary
#        3.3) Fuzzy String Matching
#     4) Tracking of harmonized feature values
#     5) Data recoding and formatting
#     6) Tracking of recoded / formatted feature values
#     7) Finalize transformation of data
#         - Removing of ineligible values
#         - Optional conversion to factor
#     8) Tracking of finalized feature values
#     9) Compilation of monitor objects for reporting
#
#   MODULE E)  Secondary Table Cleaning
#     1)  Remove duplicate records
#     2)  Remove records in CDS missing required features (defined in meta data or passed as optional argument)
#
#   MODULE F)  Finding and removing secondary redundancies
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
  # MetaData.S <- list(Tables = dsCCPhos::Meta.Tables,
  #                    Features = dsCCPhos::Meta.Features,
  #                    Values = dsCCPhos::Meta.Values)
  # Proc.TableNormalization.S <- dsCCPhos::Proc.TableNormalization
  # Proc.TableNormalization.Profile.S <- "Default"
  # Set.CurationProcess.S <- dsCCPhos::Set.CurationProcess
  # Set.CurationProcess.Profile.S <- "Default"
  # Set.DataHarmonization.S <- dsCCPhos::Set.DataHarmonization
  # Set.DataHarmonization.Profile.S <- "Default"
  # Set.DataHarmonization.TransformativeExpressions.S <- dsCCPhos::Set.TransformativeExpressions
  # Set.DataHarmonization.TransformativeExpressions.Profile.S <- "Default"
  # Set.DataHarmonization.Dictionary.S <- dsCCPhos::Set.Dictionary
  # Set.DataHarmonization.Dictionary.Profile.S <- "Default"
  # Set.DataHarmonization.FuzzyStringMatching.S <- dsCCPhos::Set.FuzzyStringMatching
  # Set.DataHarmonization.FuzzyStringMatching.Profile.S <- "Default"
  # Set.DataHarmonization.ExcludeIneligibleValues.S <- TRUE
  # Set.FeatureRequirements.S <- dsCCPhos::Set.FeatureRequirements
  # Set.FeatureRequirements.Profile.S <- "Default"
  # Set.FeatureTracking.S <- dsCCPhos::Set.FeatureTracking
  # Set.FeatureTracking.Profile.S <- "Default"
  # Set.PrimaryTableCleaning.S <- dsCCPhos::Set.PrimaryTableCleaning
  # Set.PrimaryTableCleaning.Profile.S <- "Default"
  # Set.SecondaryTableCleaning.S <- dsCCPhos::Set.SecondaryTableCleaning
  # Set.SecondaryTableCleaning.Profile.S <- "Default"


  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName.S),
              is.list(MetaData.S),
              is.data.frame(MetaData.S$Tables),
              is.data.frame(MetaData.S$Features),
              is.data.frame(MetaData.S$Values),
              is.data.frame(Proc.TableNormalization.S),
              is.string(Proc.TableNormalization.Profile.S),
              is.data.frame(Set.CurationProcess.S),
              is.data.frame(Set.DataHarmonization.S),
              is.string(Set.DataHarmonization.Profile.S),
              is.data.frame(Set.DataHarmonization.TransformativeExpressions.S),
              is.string(Set.DataHarmonization.TransformativeExpressions.Profile.S),
              is.data.frame(Set.DataHarmonization.Dictionary.S),
              is.string(Set.DataHarmonization.Dictionary.Profile.S),
              is.data.frame(Set.DataHarmonization.FuzzyStringMatching.S),
              is.string(Set.DataHarmonization.FuzzyStringMatching.Profile.S),
              is.flag(Set.DataHarmonization.ExcludeIneligibleValues.S),
              is.data.frame(Set.FeatureRequirements.S),
              is.string(Set.FeatureRequirements.Profile.S),
              is.data.frame(Set.FeatureTracking.S),
              is.string(Set.FeatureTracking.Profile.S),
              is.data.frame(Set.PrimaryTableCleaning.S),
              is.string(Set.PrimaryTableCleaning.Profile.S),
              is.data.frame(Set.SecondaryTableCleaning.S),
              is.string(Set.SecondaryTableCleaning.Profile.S))

  # Special validation rules implemented with base::stopifnot() instead of assertthat::assert_that()
  stopifnot("ERROR: Value of argument 'Proc.TableNormalization.Profile.S' does not occur in 'Proc.TableNormalization.S'." = (Proc.TableNormalization.Profile.S %in% Proc.TableNormalization.S$Profile))
  stopifnot("ERROR: Value of argument 'Set.CurationProcess.Profile.S' does not occur in 'Set.CurationProcess.S'." = (Set.CurationProcess.Profile.S %in% Set.CurationProcess.S$Profile))
  stopifnot("ERROR: Value of argument 'Set.DataHarmonization.Profile.S' does not occur in 'Set.DataHarmonization.S'." = (Set.DataHarmonization.Profile.S %in% Set.DataHarmonization.S$Profile))
  stopifnot("ERROR: Value of argument 'Set.DataHarmonization.TransformativeExpressions.Profile.S' does not occur in 'Set.DataHarmonization.TransformativeExpressions.S'." = (Set.DataHarmonization.TransformativeExpressions.Profile.S %in% Set.DataHarmonization.TransformativeExpressions.S$Profile))
  stopifnot("ERROR: Value of argument 'Set.DataHarmonization.Dictionary.Profile.S' does not occur in 'Set.DataHarmonization.Dictionary.S'." = (Set.DataHarmonization.Dictionary.Profile.S %in% Set.DataHarmonization.Dictionary.S$Profile))
  stopifnot("ERROR: Value of argument 'Set.DataHarmonization.FuzzyStringMatching.Profile.S' does not occur in 'Set.DataHarmonization.FuzzyStringMatching.S'." = (Set.DataHarmonization.FuzzyStringMatching.Profile.S %in% Set.DataHarmonization.FuzzyStringMatching.S$Profile))
  stopifnot("ERROR: Value of argument 'Set.FeatureRequirements.Profile.S' does not occur in 'Set.FeatureRequirements.S'." = (Set.FeatureRequirements.Profile.S %in% Set.FeatureRequirements.S$Profile))
  stopifnot("ERROR: Value of argument 'Set.FeatureTracking.Profile.S' does not occur in 'Set.FeatureTracking.S'." = (Set.FeatureTracking.Profile.S %in% Set.FeatureTracking.S$Profile))
  stopifnot("ERROR: Value of argument 'Set.PrimaryTableCleaning.Profile.S' does not occur in 'Set.PrimaryTableCleaning.S'." = (Set.PrimaryTableCleaning.Profile.S %in% Set.PrimaryTableCleaning.S$Profile))


#-------------------------------------------------------------------------------
# - Equip 'Settings' with default values in case of missing arguments -
#-------------------------------------------------------------------------------

  # Bundle settings arguments in a list
  Settings <- list(CurationProcess = Set.CurationProcess.S %>% filter(Profile == Set.CurationProcess.Profile.S),
                   DataHarmonization = list(Process = Set.DataHarmonization.S %>% filter(Profile == Set.DataHarmonization.Profile.S),
                                            TransformativeExpressions = Set.DataHarmonization.TransformativeExpressions.S %>% filter(Profile == Set.DataHarmonization.TransformativeExpressions.Profile.S),
                                            Dictionary = Set.DataHarmonization.Dictionary.S %>% filter(Profile == Set.DataHarmonization.Dictionary.Profile.S),
                                            FuzzyStringMatching = Set.DataHarmonization.FuzzyStringMatching.S %>% filter(Profile == Set.DataHarmonization.FuzzyStringMatching.Profile.S),
                                            ExcludeIneligibleValues = Set.DataHarmonization.ExcludeIneligibleValues.S),
                   FeatureRequirements = Set.FeatureRequirements.S %>% filter(Profile == Set.FeatureRequirements.Profile.S),
                   FeatureTracking = Set.FeatureTracking.S %>% filter(Profile == Set.FeatureTracking.Profile.S),
                   PrimaryTableCleaning = Set.PrimaryTableCleaning.S %>% filter(Profile == Set.PrimaryTableCleaning.Profile.S),
                   SecondaryTableCleaning = Set.SecondaryTableCleaning.S %>% filter(Profile == Set.SecondaryTableCleaning.Profile.S),
                   TableNormalization = Proc.TableNormalization.S %>% filter(Profile == Proc.TableNormalization.Profile.S))


  # If list of 'Settings' passed to function is incomplete, complete it with default values
  # if (is.null(Settings$DataHarmonization$Run)) { Settings$DataHarmonization$Run <- TRUE }
  # if (is.null(Settings$DataHarmonization$Process)) { Settings$DataHarmonization$Process <- dsCCPhos::Set.DataHarmonization }
  # if (is.null(Settings$DataHarmonization$Process.Profile)) { Settings$DataHarmonization$Process.Profile <- "Default" }
  # if (is.null(Settings$DataHarmonization$TransformativeExpressions)) { Settings$DataHarmonization$TransformativeExpressions <- dsCCPhos::Set.TransformativeExpressions }
  # if (is.null(Settings$DataHarmonization$TransformativeExpressions.Profile)) { Settings$DataHarmonization$TransformativeExpressions.Profile <- "Default" }
  # if (is.null(Settings$DataHarmonization$Dictionary)) { Settings$DataHarmonization$Dictionary <- dsCCPhos::Set.Dictionary }
  # if (is.null(Settings$DataHarmonization$Dictionary.Profile)) { Settings$DataHarmonization$Dictionary.Profile <- "Default" }
  # if (is.null(Settings$DataHarmonization$FuzzyStringMatching)) { Settings$DataHarmonization$FuzzyStringMatching <- dsCCPhos::Set.FuzzyStringMatching }
  # if (is.null(Settings$DataHarmonization$FuzzyStringMatching.Profile)) { Settings$DataHarmonization$FuzzyStringMatching.Profile <- "Default" }
  # if (is.null(Settings$DataHarmonization$ExcludeIneligibleValues)) { Settings$DataHarmonization$ExcludeIneligibleValues <- TRUE }
  # if (is.null(Settings$FeatureRequirements$RuleSet)) { Settings$FeatureRequirements$RuleSet <- dsCCPhos::Set.FeatureRequirements }
  # if (is.null(Settings$FeatureRequirements$Profile)) { Settings$FeatureRequirements$Profile <- "Default" }
  # if (is.null(Settings$FeatureTracking$RuleSet)) { Settings$FeatureTracking$RuleSet <- dsCCPhos::Set.FeatureTracking }
  # if (is.null(Settings$FeatureTracking$Profile)) { Settings$FeatureTracking$Profile <- "Default" }
  # if (is.null(Settings$TableCleaning$Run)) { Settings$TableCleaning$Run <- TRUE }
  # if (is.null(Settings$TableNormalization$Run)) { Settings$TableNormalization$Run <- TRUE }
  # if (is.null(Settings$TableNormalization$RuleSet)) { Settings$TableNormalization$RuleSet <- dsCCPhos::Proc.TableNormalization }
  # if (is.null(Settings$TableNormalization$Profile)) { Settings$TableNormalization$Profile <- "Default" }



#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  DataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())


#-------------------------------------------------------------------------------
# - Initial statements -
#-------------------------------------------------------------------------------

  # Print starting message
  cat("\n")
  cli::cat_bullet("Starting Data Curation...", bullet = "star")
  cat("\n")

  # Suppress summarize info messages
  options(dplyr.summarise.inform = FALSE)

  # Initiate Report object
  Report <- tibble(CurationStage = character(),
                   Table = character(),
                   ProcessTopic = character(),
                   ProcessExecution = character(),
                   ReportType = character(),
                   DetailsGroup = character(),
                   CountRootSubjects.Affected = integer(),
                   CountRootSubjects.Removed = integer(),
                   CountRecords.Affected = integer(),
                   CountRecords.Removed = integer(),
                   CountRecords.Added = integer(),
                   Message = character(),
                   MessageClass = character(),
                   Timestamp = as.POSIXct(character()))

  # Initiate Messaging objects
  Messages <- list()
  Messages$AddedFeatures <- character()
  Messages$RemovedFeatures <- character()
  Messages$ExcludedRecords <- list()

  Messages$ExcludedRecords_Primary <- character()
  Messages$ExcludedRecords_Secondary <- character()
  Messages$ExcludedRecords_SecondaryRedundancy <- character()
  Messages$CheckCurationCompletion <- "red"
  Messages$FinalMessage <- "Curation not completed"

#-------------------------------------------------------------------------------


  # Use tryCatch to catch warnings and errors
  # Note: Warnings and errors must be defined and thrown explicitly for this to work. Unspecified errors will not be caught directly but will also not lead to harsh stops.
  #tryCatch({


#===============================================================================
# SETUP:  Processing of data set meta data
#===============================================================================

  # Renaming meta data object for better code readability
  MetaData <- MetaData.S

  # Extracting all table names from meta data
  TableNames <- MetaData$Tables$TableName.Curated

  # Assuming a hierarchical data model, the following terminology is used to handle dependencies in the data set:
  #   - 'Root subjects' are the main entities that are described by the data set. There can be more then one class of 'Root subject' in a data set (e.g. 'Patient' or 'Patient + Diagnosis')
  #   - 'Seed table' refers to exactly one table that is highest in the hierarchy when it comes to describing the 'Root subjects' (e.g. 'Patient'). It is part of every 'Root'.
  #   - 'Root table' refers to tables that contain further data on 'Root subjects' (e.g. 'Diagnosis').
  #   - 'Branch table' refers to tables that are lowest in the hierarchy and are directly descended from at least one 'Root' feature

  SeedTableName <- MetaData$Tables %>%
                        filter(Role == "Seed") %>%
                        pull(TableName.Curated)

  stopifnot("ERROR in meta data: There must be exactly one table with role 'Seed'!" = (length(SeedTableName) == 1))

  SeedPrimaryKey <- MetaData$Features %>%
                        filter(TableName.Curated == SeedTableName,
                               IsPrimaryKey == TRUE) %>%
                        pull(FeatureName.Curated)


  RootTableNames <- MetaData$Tables %>%
                        filter(Role == "Seed" | Role == "Root") %>%
                        pull(TableName.Curated)


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

  ForeignKeys <- TableNames %>%
                      map(function(tablename)
                          {
                              MetaData$Features %>%
                                  filter(TableName.Curated == tablename,
                                         IsForeignKey == TRUE) %>%
                                  pull(FeatureName.Curated)
                          }) %>%
                      set_names(TableNames)



#===============================================================================
# MODULE A)  Harmonization of Raw Data Set meta data and structure
#===============================================================================
#   - Add empty tables in data set if they are missing in raw data
#   - Recode feature names according to meta data
#   - Add empty features in case of missing feature names
#   - Remove unknown features
#-------------------------------------------------------------------------------


# If tables are missing, create corresponding empty tables for easier management throughout following processing
#-------------------------------------------------------------------------------

  MissingTableNames <- TableNames[!(TableNames %in% names(DataSet))]

  # Create empty data frames for missing tables
  if (length(MissingTableNames) > 0)
  {
      for (tablename in MissingTableNames)
      {
          DataSet[[tablename]] <- data.frame()
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
                          vc_Lookup <- filter(MetaData$Features, TableName.Curated == tablename)$FeatureName.Raw
                          names(vc_Lookup) <- filter(MetaData$Features, TableName.Curated == tablename)$FeatureName.Curated

                          if (length(Table) > 0)
                          {
                              # Rename feature names according to look-up vector
                              Table %>% rename(any_of(vc_Lookup))      # Returns a tibble

                          } else {

                              # Create empty data.frame with pre-defined column names
                              df <- data.frame(matrix(nrow = 0,
                                                      ncol = length(names(vc_Lookup)))) %>%
                                        setNames(names(vc_Lookup)) %>%
                                        mutate(across(everything(), ~ as.character(.x)))

                              return(df)
                          }
                       })


# Add empty features in case of missing feature names and remove unknown existing features
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          # Determine missing and unknown features
                          RequiredFeatureNames <- dplyr::filter(MetaData$Features, TableName.Curated == tablename)$FeatureName.Curated
                          PresentFeatureNames <- names(Table)
                          MissingFeatures <- RequiredFeatureNames[!(RequiredFeatureNames %in% PresentFeatureNames)]
                          UnknownFeatures <- names(Table)[!(names(Table) %in% RequiredFeatureNames)]

                          # If a table misses features, add empty columns accordingly
                          if (length(MissingFeatures) > 0)
                          {
                              Table <- Table %>%
                                            mutate(!!!set_names(rep(list(NA_character_), length(MissingFeatures)), MissingFeatures))

                              # Print message
                              Message <- paste0("Table '", tablename, "': Added empty vector for missing features ", paste0("'", MissingFeatures, "'", collapse = ", "), ".")
                              cli::cat_bullet(Message, bullet = "info")
                              cat("\n")

                              # Save message for output
                              Messages$AddedFeatures <- c(Messages$AddedFeatures,
                                                          Info = Message)
                          }

                          # The following effectively removes all unknown features
                          Table <- Table %>%
                                        select(all_of(RequiredFeatureNames))

                          # Print and save messages for removed features
                          if (length(UnknownFeatures) > 0)
                          {
                              Message <- paste0("Table '", tablename, "': Removed unknown features ", paste0("'", UnknownFeatures, "'", collapse = ", "), "!")
                              cli::cat_bullet(Message, bullet = "warning", bullet_col = "red")
                              cat("\n")

                              Messages$RemovedFeatures <- c(Messages$RemovedFeatures,
                                                            Warning = Message)
                          }

                          return(Table)
                       })



#===============================================================================
# MODULE B)  Primary Table Cleaning
#===============================================================================
#   1) Creating auxiliary data.frame 'DataSetRoot' by linking all tables with Role 'Seed' and 'Root'
#   2) Table cleaning with detection/removal of:
#       - Records that are not linked to any data set root subject
#       - Duplicate records
#       - Records with missing required features (determined in meta data / passed through Settings)
#       - Records that are not consistent with special trans-feature requirement rules (defined in meta data / passed through Settings)
#-------------------------------------------------------------------------------

#===============================================================================
# MONITORING: Count table records
#===============================================================================

  # Count records in initial data.frames
  CountRecords_Initial <- DataSet %>%
                              map_int(\(Table) ifelse (!is.null(nrow(Table)), nrow(Table), 0))

#-------------------------------------------------------------------------------


#===============================================================================
#   MODULE B1)  Create and clean 'DataSetRoot'
#===============================================================================
#     - Create auxiliary data.frame containing all eligible 'root subjects' by merging 'Seed' table with all 'Root' tables (in case 'Root' does not only consist of 'Seed')
#     - Filter out any record that has missing values in features marked as required in meta data (thereby also removing 'rogue'/unlinked patient or diagnosis records because this way every patient needs to have at least one related diagnosis and vice versa)
#     - Note: Setting 'PrimaryKeyIgnoredInRedundancyCheck' on FALSE is crucial (e.g. because different DiagnosisIDs of same patient and diagnosis can e.g. be related to different Histologies).
#-------------------------------------------------------------------------------

  # The reduce()-call performs pair-wise left-joining of 'Seed' table with all 'Root' tables
  DataSetRoot <- reduce(.x = DataSet[RootTableNames[RootTableNames != SeedTableName]],
                        .f = \(TableA, TableB) left_join(TableA, TableB, by = SeedPrimaryKey),
                        .init = DataSet[[SeedTableName]])

  # Clean DataSetRoot
  if ((Settings$CurationProcess %>% filter(Table == ".DataSetRoot") %>% pull(PrimaryTableCleaning)) == TRUE)
  {
      TableCleaning.DataSetRoot <- dsFreda::CleanTable(Table = DataSetRoot,
                                                       PrimaryKey = RootPrimaryKey,
                                                       ForeignKey = RootPrimaryKey,
                                                       PrimaryKeyIgnoredInRedundancyCheck = FALSE,      # This setting is important because it considers the semantic meaning of root primary keys (e.g. two different DiagnosisIDs of the same patient should stay truly distinct, because they can be related to different diagnostic procedures)
                                                       EmptyStrings.Detect = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(EmptyStrings.Detect),
                                                       EmptyStrings.Substitute = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(EmptyStrings.Substitute),
                                                       EmptyStrings.Substitution = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(EmptyStrings.Substitution),
                                                       DuplicateRecords.Detect = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(DuplicateRecords.Detect),
                                                       DuplicateRecords.Remove = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(DuplicateRecords.Remove),
                                                       FeatureRequirements = Settings$FeatureRequirements %>% filter(Table %in% RootTableNames),
                                                       FeatureAvailabilityViolations.Detect = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(FeatureAvailabilityViolations.Detect),
                                                       FeatureAvailabilityViolations.Remove = Settings$PrimaryTableCleaning %>% filter(Table == ".DataSetRoot") %>% pull(FeatureAvailabilityViolations.Remove))

      # Bind report to main report object
      Report <- Report %>%
                    bind_rows(TableCleaning.DataSetRoot$Report %>%
                                  mutate(CurationStage = "Primary cleaning",
                                         Table = "DataSetRoot"))

      # Reassign DataSetRoot
      DataSetRoot <- TableCleaning.DataSetRoot$Table
  }

  DataSetRoot <- DataSetRoot %>%
                      select(all_of(RootPrimaryKey)) %>%
                      distinct()


#===============================================================================
#   MODULE B2)  Table Cleaning
#===============================================================================
#     - Looping through all tables in 'DataSet' to perform table cleaning
#-------------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  ProgressBar <- progress::progress_bar$new(format = "Primary table cleaning: Excluding ineligible table records... [:bar] :percent in :elapsed  :spin",
                                            total = length(DataSet), clear = FALSE, width = 100)
  #-----------------------------------------------------------------------------

  PrimaryTableCleaning <- DataSet %>%
                              imap(function(Table, tablename)
                                   {
                                      try(ProgressBar$tick())

                                      # Return-content if table cleaning is omitted
                                      CurrentTableCleaning <- list(Table = Table,
                                                                   Report = tibble(CurationStage = "Primary table cleaning",
                                                                                   Table = tablename,
                                                                                   ProcessTopic = "Table cleaning",
                                                                                   ProcessExecution = "Omitted",
                                                                                   ReportType = "Message",
                                                                                   Message = paste0("Primary cleaning of table '", tablename, "' omitted."),
                                                                                   MessageClass = "Info",
                                                                                   Timestamp = Sys.time()),
                                                                   AffectedRootSubjects = NULL)

                                      # Check in settings if primary table cleaning should be executed for current table
                                      if ((Settings$CurationProcess %>% filter(Table == tablename) %>% pull(PrimaryTableCleaning)) == TRUE)
                                      {
                                          # If current table is missing or empty
                                          if (length(Table) == 0 || nrow(Table) == 0)
                                          {
                                              CurrentTableCleaning <- list(Table = Table,
                                                                           Report = tibble(CurationStage = "Primary table cleaning",
                                                                                           Table = tablename,
                                                                                           ProcessTopic = "Table cleaning",
                                                                                           ProcessExecution = "Omitted",
                                                                                           ReportType = "Message",
                                                                                           Message = paste0("Primary table cleaning: Table '", tablename, "' is missing or empty."),
                                                                                           MessageClass = "Info",
                                                                                           Timestamp = Sys.time()),
                                                                           AffectedRootSubjects = NULL)
                                          } else {

                                              # For Root tables 'PrimaryKeyIgnoredInRedundancyCheck' must be set on FALSE (for explanation see section on DataSetRoot above)
                                              PrimaryKeyIgnoredInRedundancyCheck <- TRUE
                                              if (tablename %in% RootTableNames) { PrimaryKeyIgnoredInRedundancyCheck <- FALSE }

                                              # Perform table cleaning (this returns a list object with the elements 'Table', 'Report' and 'AffectedRootSubjects')
                                              CurrentTableCleaning <- dsFreda::CleanTable(Table = Table,
                                                                                          PrimaryKey = PrimaryKeys[[tablename]],
                                                                                          ForeignKey = ForeignKeys[[tablename]],
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
                                                                                          FeatureAvailabilityViolations.Remove = Settings$PrimaryTableCleaning %>% filter(Table == tablename) %>% pull(FeatureAvailabilityViolations.Remove))

                                              # Add curation stage and current table name to report
                                              CurrentTableCleaning$Report <- CurrentTableCleaning$Report %>%
                                                                                  mutate(CurationStage = "Primary table cleaning",
                                                                                         Table = tablename)
                                          }
                                      }

                                      return(CurrentTableCleaning)
                                  })

  try(ProgressBar$terminate())

  # Reassign DataSet
  DataSet <- PrimaryTableCleaning %>%
                  imap(\(CurrentTableCleaning, tablename) CurrentTableCleaning$Table)

  # Extract Report data.frames and bind them to main report
  Report <- Report %>%
                bind_rows(PrimaryTableCleaning %>%
                              imap(\(CurrentTableCleaning, tablename) CurrentTableCleaning$Report) %>%
                              list_rbind())



#===============================================================================
# MONITORING: Count ineligible records
#===============================================================================

  # Count records in data frames after primary exclusion
  CountRecords_AfterPrimaryExclusion <- DataSet %>%
                                            map_int(\(Table) ifelse (!is.null(nrow(Table)), nrow(Table), 0))

  # Count excluded records
  CountExcludedRecords_Primary <- CountRecords_Initial - CountRecords_AfterPrimaryExclusion


  # Print messages for live monitoring in local tests
  for (i in 1:length(CountExcludedRecords_Primary))
  {
      Message <- paste0("Primary exclusion: Removed ", CountExcludedRecords_Primary[i], " ineligible records from '", names(CountExcludedRecords_Primary)[i], "' table.")
      cli::cat_bullet(Message, bullet = "info")

      # Save messages in output object
      Messages$ExcludedRecords_Primary <- c(Messages$ExcludedRecords_Primary,
                                            Info = Message)
  }
  cat("\n")




#===============================================================================
# MODULE C)  Table Normalization
#===============================================================================
#   - Perform procedures like 'split and expand' where necessary (as determined by settings / meta data)
#-------------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  ProgressBar <- progress::progress_bar$new(format = "Table Normalization... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet), clear = FALSE, width = 100)
  #-----------------------------------------------------------------------------

  TableNormalization <- DataSet %>%
                            imap(function(Table, tablename)
                                 {
                                    try(ProgressBar$tick())

                                    NormalizationRules <- Settings$TableNormalization %>% filter(Table == tablename)

                                    if (length(NormalizationRules) == 0 || nrow(NormalizationRules) == 0)
                                    {
                                        CurrentTableNormalization <- list(Table = Table,
                                                                          Report = tibble(CurationStage = "Table normalization",
                                                                                          Table = tablename,
                                                                                          ProcessTopic = "Table normalization",
                                                                                          ProcessExecution = "Inapplicable",
                                                                                          ReportType = "Message",
                                                                                          Message = paste0("Table normalization: No procedures for table '", tablename, "'."),
                                                                                          MessageClass = "Info",
                                                                                          Timestamp = Sys.time()))
                                    } else {

                                        CurrentTableNormalization <- list(Table = Table,
                                                                          Report = tibble(CurationStage = "Table normalization",
                                                                                          Table = tablename,
                                                                                          ProcessTopic = "Table normalization",
                                                                                          ProcessExecution = "Omitted",
                                                                                          ReportType = "Message",
                                                                                          Message = paste0("Table normalization omitted for table '", tablename, "'."),
                                                                                          MessageClass = "Info",
                                                                                          Timestamp = Sys.time()))

                                        # Check in settings if normalization procedures should be executed for current table
                                        if ((Settings$CurationProcess %>% filter(Table == tablename) %>% pull(TableNormalization)) == TRUE)
                                        {
                                            # If current table is missing or empty
                                            if (length(Table) == 0 || nrow(Table) == 0)
                                            {
                                                CurrentTableNormalization <- list(Table = Table,
                                                                                  Report = tibble(CurationStage = "Table normalization",
                                                                                                  Table = tablename,
                                                                                                  ProcessTopic = "Table normalization",
                                                                                                  ProcessExecution = "Omitted",
                                                                                                  ReportType = "Message",
                                                                                                  Message = paste0("Table normalization: Table '", tablename, "' is missing or empty."),
                                                                                                  MessageClass = "Info",
                                                                                                  Timestamp = Sys.time()))
                                            } else {

                                                # Perform table normalization operations (this returns a list object with the elements 'Table', 'Report' and 'AffectedRootSubjects')
                                                CurrentTableNormalization <- Table %>%
                                                                                dsFreda::NormalizeTable(PrimaryKey = PrimaryKeys[[tablename]],
                                                                                                        ForeignKey = ForeignKeys[[tablename]],
                                                                                                        RuleSet = NormalizationRules)

                                                # Add curation stage and current table name to report
                                                CurrentTableNormalization$Report <- CurrentTableNormalization$Report %>%
                                                                                        mutate(CurationStage = "TableNormalization",
                                                                                               Table = tablename)
                                            }
                                        }
                                    }

                                    return(CurrentTableNormalization)
                                 })

  try(ProgressBar$terminate())

  # Reassign DataSet
  DataSet <- TableNormalization %>%
                  imap(\(CurrentTableNormalization, tablename) CurrentTableNormalization$Table)

  # Extract Report data.frames and bind them to main report
  Report <- Report %>%
                bind_rows(TableNormalization %>%
                              imap(\(CurrentTableNormalization, tablename) CurrentTableNormalization$Report) %>%
                              list_rbind())



#===============================================================================
# MODULE D)  Data Harmonization / Transformation
#===============================================================================
#   1) Conversion of all features into type character prior to definitive feature-specific formatting
#   2) Definition of features to monitor during Transformation
#   3) Tracking of raw feature values
#   4) Data harmonization (correctional transformation)
#   5) Tracking of harmonized feature values
#   6) Data recoding and formatting
#   7) Tracking of recoded / formatted feature values
#   8) Finalize transformation of data
#        - Removing of ineligible values
#        - Optional conversion to factor
#   9) Tracking of finalized feature values
#   10) Compilation of monitor objects for reporting
#-------------------------------------------------------------------------------

#===============================================================================
#   Module D 1)  Conversion of all features into type character prior to definitive formatting
#===============================================================================

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          Table <- Table %>%
                                      mutate(across(everything(), ~ as.character(.x)))
                       })


#===============================================================================
#   Module D 2)  Definition of tracked features and their sets of eligible values
#===============================================================================
#     - Create meta data on eligible value sets of features to be tracked / monitored during curation process
#     - Element object syntax: List of vectors
#         - Vector names = Name of feature to be monitored during Curation (Transformation)
#         - Vector values = Set of eligible values obtained from meta data / passed rule set
#     - If a feature should be monitored but has no specific set of eligible values, set it NULL
#-------------------------------------------------------------------------------

  ls_MonitorMetaData <- names(DataSet) %>%
                            map(function(tablename)
                                {
                                    vc_FeaturesToTrack <- Settings$FeatureTracking$RuleSet %>%
                                                              rename(IsTracked = all_of(Settings$FeatureTracking$Profile)) %>%      # Renaming feature based on passed argument
                                                              filter(Table == tablename,
                                                                     IsTracked == TRUE) %>%
                                                              pull(Feature)

                                    ls_EligibleValues <- vc_FeaturesToTrack %>%
                                                              map(function(featurename)
                                                                  {
                                                                      Values <- MetaData$Values %>%
                                                                                    filter(Table == tablename,
                                                                                           FeatureName.Curated == featurename) %>%
                                                                                    select(Value.Raw,
                                                                                           Value.Curated)

                                                                      if (nrow(Values) == 0) { return(NULL) } else { return(Values) }
                                                                  }) %>%
                                                              set_names(vc_FeaturesToTrack)
                                }) %>%
                            set_names(names(DataSet))



#===============================================================================
#   Module D 3)  Track feature values of raw data
#===============================================================================
#     - Get unique raw values and their frequencies for monitoring
#     - Copy values of monitored features and mark them with TrackID that has correspondent in actually processed data frames
#-------------------------------------------------------------------------------

  # Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      # Initiate list of data frames containing transformation tracks
      # First step: Store copied raw values of monitored features and mark them with "TrackID" to track them along transformation process
      ls_TransformationTracks <- map2(.x = DataSet,
                                      .y = ls_MonitorMetaData,
                                      .f = function(DataFrame, MonitorMetaData)
                                           {
                                              if (purrr::is_empty(MonitorMetaData) == FALSE)
                                              {
                                                   DataFrame %>%
                                                        select(names(MonitorMetaData)) %>%
                                                        rename_with(.fn = ~ str_c(., "__Raw"),   # Two underscores for later use in pivot_longer()
                                                                    .cols = everything()) %>%
                                                        mutate(TrackID = row_number(), .before = 1)   # Create TrackID to enable correct mapping and further processing
                                              }
                                              else { return(data.frame()) }
                                           })

      ls_ValueCounts_Raw <- map2(.x = DataSet,
                                 .y = ls_MonitorMetaData,
                                 .f = function(DataFrame, MonitorMetaData)
                                      {
                                         DataFrame %>%
                                             dsFreda::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                        TransformationStage = "Raw") %>%
                                             select(Feature,
                                                    Value,
                                                    Frequency) %>%
                                             rename(Value.Raw = Value,
                                                    Count.Raw = Frequency)
                                      })
  } else {
      stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }



#===============================================================================
#   Module D 4)  Data Harmonization (correctional transformation)
#===============================================================================
#     Step-wise approach incorporating the following methods (feature-specific selection and order defined by passed settings)
#       - Transformative expressions
#       - Dictionary look-up
#       - Fuzzy String Matching
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress::progress_bar$new(format = "Harmonizing data... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          Table <- Table %>% mutate(TrackID = row_number())      # Enables tracking of transformation (see above)

                          if (Settings$DataHarmonization$Run == TRUE)      # The check is placed here and not before the mapping function so that the 'TrackID' is appended regardless (see above)
                          {
                              HarmonizationProcess <- Settings$DataHarmonization$Process %>%
                                                          filter(Profile == Settings$DataHarmonization$Process.Profile,
                                                                 Table == tablename,
                                                                 RunHarmonization == TRUE) %>%
                                                          arrange(HarmonizationOrder)      # Defines the order in which features within a table are being harmonized (this can be relevant in transformative espressions that contain inter-feature dependencies)

                              for (featurename in HarmonizationProcess$Feature)
                              {
                                  Methods <- Settings$DataHarmonization$Process %>%
                                                  filter(Table == tablename,
                                                         Feature == featurename) %>%
                                                  as.list()

                                  EligibleValueSet <- MetaData$Values %>%
                                                          filter(Table == tablename,
                                                                 FeatureName.Curated == featurename) %>%
                                                          pull(Value.Raw)   # Eligible Values BEFORE recoding

                                  TransformativeExpressions <- Settings$DataHarmonization$TransformativeExpressions %>%
                                                                    filter(Table == tablename,
                                                                           Feature == featurename)

                                  Dictionary <- Settings$DataHarmonization$Dictionary %>%
                                                    filter(Table == tablename,
                                                           Feature == featurename) %>%
                                                    pull(var = NewValue,
                                                         name = LookupValue)

                                  FuzzyStringMatching <- Settings$DataHarmonization$FuzzyStringMatching %>%
                                                              filter(Table == tablename,
                                                                     Feature == featurename) %>%
                                                              as.list()

                                  Table[[featurename]] <- dsFreda::HarmonizeFeature(Feature = Table[[featurename]],
                                                                                    FeatureName = featurename,
                                                                                    ContextDataFrame = Table,
                                                                                    Methods = Methods,
                                                                                    EligibleValueSet = EligibleValueSet,
                                                                                    TransformativeExpressions = TransformativeExpressions,
                                                                                    Dictionary = Dictionary,
                                                                                    FuzzyStringMatching = FuzzyStringMatching)
                              }
                          }

                          return(Table)
                       })

  try(ProgressBar$terminate())



#===============================================================================
#   Module D 5)  Track feature values after Harmonization
#===============================================================================

# Map raw values to their harmonized state to get transformation tracks
#===============================================================================

  # Check if object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(ls_TransformationTracks) == names(DataSet)) & all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      ls_TransformationTracks <- pmap(.l = list(ls_TransformationTracks,
                                                DataSet,
                                                ls_MonitorMetaData),
                                      .f = function(TransformationTracks, HarmonizedDataFrame, MonitorMetaData)
                                           {
                                               if (purrr::is_empty(MonitorMetaData) == FALSE)
                                               {
                                                   HarmonizedValues <- HarmonizedDataFrame %>%
                                                                             select(c("TrackID", names(MonitorMetaData))) %>%
                                                                             rename_with(.fn = ~ str_c(., "__Harmonized"),   # Two underscores for later use in pivot_longer()
                                                                                         .cols = all_of(names(MonitorMetaData)))

                                                   TransformationTracks %>%
                                                       left_join(HarmonizedValues, by = join_by(TrackID)) %>%
                                                       distinct(pick(contains("__Raw")), .keep_all = TRUE)
                                               }
                                               else { return(data.frame()) }
                                           })
  } else {
      stop("Internal error: Object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }


# Get counts of all distinct values in harmonized data sets
#===============================================================================

  # Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      ls_ValueCounts_Harmonized <- map2(.x = DataSet,
                                        .y = ls_MonitorMetaData,
                                        .f = function(DataFrame, MonitorMetaData)
                                             {
                                                 DataFrame %>%
                                                    dsFreda::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                              TransformationStage = "Harmonized") %>%
                                                    select(Feature,
                                                           Value,
                                                           Frequency) %>%
                                                    rename(Value.Harmonized = Value,
                                                           Count.Harmonized = Frequency)
                                             })
  } else {
      stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }



#===============================================================================
#   Module D 6)  Data recoding and formatting
#===============================================================================
#     - Recoding data using dsFreda::RecodeData() based on specifications in MetaData$Values
#     - RecodeData() uses a dictionary in the form of a named vector to perform recoding on a target vector
#     - Format / Re-type data using dsFreda::FormatData() based on specifications in MetaData$Features
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress::progress_bar$new(format = "Recoding and formatting data... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          if (length(Table) > 0 && nrow(Table) > 0)
                          {
                              # Recode table data as defined in meta data
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                              FeaturesWithValueSets <- MetaData$Values %>%
                                                            filter(Table == tablename) %>%
                                                            pull(FeatureName.Curated) %>%
                                                            unique()

                              if (length(FeaturesWithValueSets) > 0)
                              {
                                  RecodingDictionaries <- MetaData$Values %>%
                                                              filter(Table == tablename) %>%
                                                              split(.$FeatureName.Curated) %>%      # 'split' is a base function and needs '.$' to address 'FeatureName.Curated'
                                                              map(\(Values) with(Values, set_names(Value.Curated, Value.Raw)))

                                  for (featurename in FeaturesWithValueSets)
                                  {
                                      Table[[featurename]] <- dsFreda::RecodeData(TargetVector = Table[[featurename]],
                                                                                  Dictionary = RecodingDictionaries[[featurename]])
                                  }
                              }

                              # Format / Re-type table data as defined in meta data
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                              FeatureTypes <- MetaData$Features %>%
                                                  filter(TableName.Curated == tablename,
                                                         Type != "character") %>%
                                                  select(FeatureName.Curated, Type) %>%
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

                          return(Table)
                       })

  try(ProgressBar$terminate())



#===============================================================================
#   Module D 7)  Track feature values after Recoding
#===============================================================================

# Map raw values to their recoded state to get transformation tracks
#===============================================================================

  # Check if object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(ls_TransformationTracks) == names(DataSet)) & all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      ls_TransformationTracks <- pmap(.l = list(ls_TransformationTracks,
                                               DataSet,
                                               ls_MonitorMetaData),
                                      .f = function(TransformationTracks, RecodedDataFrame, MonitorMetaData)
                                           {
                                               if (purrr::is_empty(MonitorMetaData) == FALSE)
                                               {
                                                   RecodedValues <- RecodedDataFrame %>%
                                                                         select(c("TrackID", names(MonitorMetaData))) %>%
                                                                         rename_with(.fn = ~ str_c(., "__Recoded"),   # Two underscores for later use in pivot_longer()
                                                                                     .cols = all_of(names(MonitorMetaData)))

                                                   TransformationTracks %>%
                                                       left_join(RecodedValues,
                                                                 by = join_by(TrackID)) %>%
                                                       distinct(pick(contains("__Raw")), .keep_all = TRUE)
                                               }
                                               else { return(data.frame()) }
                                           })
  } else {
      stop("Internal error: Object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }


# Get counts of all distinct values in recoded data sets
#===============================================================================

  # Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      ls_ValueCounts_Recoded <- map2(.x = DataSet,
                                     .y = ls_MonitorMetaData,
                                     .f = function(DataFrame, MonitorMetaData)
                                          {
                                              DataFrame %>%
                                                 dsFreda::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                            TransformationStage = "Recoded") %>%
                                                 select(Feature,
                                                        Value,
                                                        Frequency) %>%
                                                 rename(Value.Recoded = Value,
                                                        Count.Recoded = Frequency)
                                          })
  } else {
      stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }



#===============================================================================
#   Module D 8)  Finalize transformation of data values using dsFreda::FinalizeDataTransformation()
#===============================================================================
#     - (Optional / Default) Exclusion of ineligible data (including data that could not be transformed)
#     - (Optional) Conversion to ordered factor
#     - (Optional) Assignment of factor labels   <-- Conversion to factor is put off for now, 02/2024
#     - All predefined information stored in MetaData$Values
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress::progress_bar$new(format = "Finalizing data transformation... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet), clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          if (length(Table) > 0 && nrow(Table) > 0)
                          {
                              # Get features that are supposed to be harmonized from settings
                              HarmonizationProcess <- Settings$DataHarmonization$Process %>%
                                                          filter(Profile == Settings$DataHarmonization$Process.Profile,
                                                                 Table == tablename,
                                                                 RunHarmonization == TRUE)

                              for (featurename in HarmonizationProcess$Feature)
                              {
                                  # Get eligible value set for current feature as data.frame including data on factoring
                                  EligibleValueSet <- MetaData$Values %>%
                                                          filter(Table == tablename,
                                                                 FeatureName.Curated == featurename)

                                  if (nrow(EligibleValueSet) > 0)
                                  {
                                      Table[[featurename]] <- dsFreda::FinalizeDataTransformation(TargetVector = Table[[featurename]],
                                                                                                  EligibleValueSet = EligibleValueSet,
                                                                                                  ExcludeIneligibleValues = Settings$DataHarmonization$ExcludeIneligibleValues)
                                  }
                              }
                          }

                          return(Table)
                       })

  try(ProgressBar$terminate())



#===============================================================================
#   Module D 9)  Track Feature Values after Finalized Transformation
#===============================================================================

# Map raw values to their finalized state to get transformation tracks
#===============================================================================

  # Check if object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(ls_TransformationTracks) == names(DataSet)) & all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      ls_TransformationTracks <- pmap(.l = list(ls_TransformationTracks,
                                               DataSet,
                                               ls_MonitorMetaData),
                                      .f = function(TransformationTrack, FinalizedDataFrame, MonitorMetaData)
                                           {
                                               if (purrr::is_empty(MonitorMetaData) == FALSE)
                                               {
                                                   FinalizedValues <- FinalizedDataFrame %>%
                                                                           select(c("TrackID", names(MonitorMetaData))) %>%
                                                                           rename_with(.fn = ~ str_c(., "__Final"),   # Two underscores for later use in pivot_longer()
                                                                                       .cols = all_of(names(MonitorMetaData)))

                                                   TransformationTrack %>%
                                                       left_join(FinalizedValues,
                                                                 by = join_by(TrackID)) %>%
                                                       distinct(pick(contains("__Raw")), .keep_all = TRUE)
                                               }
                                               else { return(data.frame()) }
                                           })
  } else {
      stop("Internal error: Object names in ls_TransformationTracks, DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }


# Get counts of all distinct values in finalized data sets
#===============================================================================

  # Check if object names in DataSet and ls_MonitorMetaData are identical to avoid incorrect mapping
  if (all(names(DataSet) == names(ls_MonitorMetaData)))
  {
      ls_ValueCounts_Final <- map2(.x = DataSet,
                                   .y = ls_MonitorMetaData,
                                   .f = function(DataFrame, MonitorMetaData)
                                        {
                                            DataFrame %>%
                                                dsFreda::TrackValueCounts(FeatureNames = names(MonitorMetaData),
                                                                           TransformationStage = "Final") %>%
                                                select(Feature,
                                                       Value,
                                                       Frequency) %>%
                                                rename(Value.Final = Value,
                                                       Count.Final = Frequency)
                                        })
  } else {
      stop("Internal error: Object names in DataSet and ls_MonitorMetaData must be identical and in the same order.")
  }



#===============================================================================
#   Module D 10)  Merge monitor objects into coherent summaries
#===============================================================================

# Summarize Transformation Tracks
#===============================================================================
  ls_TransformationTracks_Summaries <- pmap(.l = list(ls_TransformationTracks,
                                                      ls_MonitorMetaData),
                                            .f = function(TrackData,
                                                          MonitorMetaData)
                                                 {
                                                     if (length(TrackData) > 0 && nrow(TrackData) > 0)
                                                     {
                                                         Summary <- TrackData %>%
                                                                         mutate(across(everything(), ~ as.character(.x))) %>%      # Turn all columns into character (necessary for pivot_longer() to work correctly)
                                                                         tidyr::pivot_longer(cols = c(everything(), -TrackID),
                                                                                             names_to = c("Feature", "Stage"),
                                                                                             names_sep = "(__)",      # Separate by '__'-string (two underscores)
                                                                                             values_to = "Value") %>%
                                                                         tidyr::pivot_wider(names_from = Stage,
                                                                                            values_from = Value) %>%
                                                                         select(-TrackID) %>%
                                                                         distinct() %>%
                                                                         rename(Value.Raw = Raw,
                                                                                Value.Harmonized = Harmonized,
                                                                                Value.Recoded = Recoded,
                                                                                Value.Final = Final) %>%
                                                                         rowwise() %>%
                                                                         mutate(IsOccurring = TRUE,
                                                                                IsEligible.Raw = ifelse(is.na(Value.Raw) | is.null(MonitorMetaData[[Feature]]),      # If value is NA or if there is no set of eligible values, set variable NA...
                                                                                                        NA,
                                                                                                        Value.Raw %in% MonitorMetaData[[Feature]]$Value.Raw),      # ... else check if specific row value is in set of eligible values
                                                                                IsEligible.Harmonized = ifelse(is.na(Value.Harmonized) | is.null(MonitorMetaData[[Feature]]),
                                                                                                               NA,
                                                                                                               Value.Harmonized %in% MonitorMetaData[[Feature]]$Value.Raw),
                                                                                IsEligible.Recoded = ifelse(is.na(Value.Recoded) | is.null(MonitorMetaData[[Feature]]),
                                                                                                            NA,
                                                                                                            Value.Recoded %in% MonitorMetaData[[Feature]]$Value.Curated),
                                                                                IsEligible.Final = ifelse(is.na(Value.Final),
                                                                                                          NA,
                                                                                                          TRUE)) %>%
                                                                         ungroup()

                                                         # Add set of all eligible values regardless of occurrence to summary
                                                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                         for (i in 1:length(MonitorMetaData))   # Loop through all monitored features of a table
                                                         {
                                                             AllEligibleValues <- tibble(Feature = names(MonitorMetaData)[i],
                                                                                         Value.Raw = MonitorMetaData[[i]]$Value.Raw,
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
                                                                                desc(IsEligible.Harmonized),
                                                                                Value.Raw)

                                                         return(Summary)

                                                     } else { return (data.frame()) }
                                                 })


# Create detailed transformation monitors
#===============================================================================
#   - Joining of info from transformation tracks and value counts
#-------------------------------------------------------------------------------
  ls_TransformationMonitors <- pmap(.l = list(ls_TransformationTracks_Summaries,
                                              ls_ValueCounts_Raw,
                                              ls_ValueCounts_Harmonized,
                                              ls_ValueCounts_Recoded,
                                              ls_ValueCounts_Final),
                                    .f = function(TransformationTracksSummary,
                                                 ValueCountsRaw,
                                                 ValueCountsHarmonized,
                                                 ValueCountsRecoded,
                                                 ValueCountsFinal)
                                         {
                                             if (nrow(TransformationTracksSummary) > 0)
                                             {
                                                 TransformationTracksSummary %>%
                                                     left_join(ValueCountsRaw, by = c("Feature", "Value.Raw")) %>%
                                                     left_join(ValueCountsHarmonized, by = c("Feature", "Value.Harmonized")) %>%
                                                     left_join(ValueCountsRecoded, by = c("Feature", "Value.Recoded")) %>%
                                                     left_join(ValueCountsFinal, by = c("Feature", "Value.Final")) %>%
                                                     mutate(Count.Harmonized = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                         TRUE ~ Count.Harmonized),
                                                            Count.Recoded = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                      TRUE ~ Count.Recoded),
                                                            Count.Final = case_when(IsOccurring == FALSE ~ NA_integer_,
                                                                                    TRUE ~ Count.Final)) %>%
                                                     arrange(Feature,
                                                             desc(IsOccurring),
                                                             desc(IsEligible.Raw),
                                                             desc(IsEligible.Harmonized),
                                                             Value.Raw)
                                             }
                                             else { return(NULL) }
                                         })


# Create overview of value eligibility in different transformation stages
#===============================================================================
  ls_EligibilityOverviews <- pmap(.l = list(ls_TransformationMonitors,
                                            ls_MonitorMetaData),
                                  .f = function(MonitorData,
                                                MonitorMetaData)
                                       {
                                          if (length(MonitorData) > 0 && nrow(MonitorData) > 0)
                                          {
                                              # Filter out features that are not meant to be monitored, e.g. do not have applicable eligibility criteria
                                              MonitorData <- MonitorData %>%
                                                                  filter(Feature %in% names(MonitorMetaData))

                                              SummaryRaw <- MonitorData %>%
                                                                group_by(Feature, IsEligible.Raw) %>%
                                                                    summarize(Raw = sum(Count.Raw, na.rm = TRUE)) %>%
                                                                    rename(Eligibility = IsEligible.Raw)

                                              SummaryHarmonized <- MonitorData %>%
                                                                        distinct(pick(Feature, Value.Harmonized), .keep_all = TRUE) %>%
                                                                        group_by(Feature, IsEligible.Harmonized) %>%
                                                                            summarize(Harmonized = sum(Count.Harmonized, na.rm = TRUE)) %>%
                                                                            rename(Eligibility = IsEligible.Harmonized)

                                              SummaryRecoded <- MonitorData %>%
                                                                    distinct(pick(Feature, Value.Recoded), .keep_all = TRUE) %>%
                                                                    group_by(Feature, IsEligible.Recoded) %>%
                                                                        summarize(Recoded = sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                        rename(Eligibility = IsEligible.Recoded)

                                              SummaryFinal <- MonitorData %>%
                                                                  distinct(pick(Feature, Value.Final), .keep_all = TRUE) %>%
                                                                  group_by(Feature, IsEligible.Final) %>%
                                                                      summarize(Final = sum(Count.Final, na.rm = TRUE)) %>%
                                                                      rename(Eligibility = IsEligible.Final)

                                              Overview <- SummaryRaw %>%
                                                              full_join(SummaryHarmonized, by = join_by(Feature, Eligibility)) %>%
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
                                                              mutate(across(c(Raw, Harmonized, Recoded, Final), ~ case_when(is.na(.x) ~ 0, .default = .x))) %>%       # Turn all NAs into 0 in count columns
                                                              group_by(Feature) %>%
                                                                  mutate(across(c(Raw, Harmonized, Recoded, Final), ~ .x / sum(.x), .names = "{.col}_Proportional")) %>%      # Create proportional value columns
                                                              ungroup() %>%
                                                              arrange(Feature, factor(Eligibility, levels = c("Eligible", "Ineligible", "Missing")))

                                          } else { return(data.frame()) }
                                      })


# Create overview of value eligibility in different transformation stages
#===============================================================================
  ls_ValueSetOverviews <- ls_TransformationMonitors %>%
                              map(function(MonitorData)
                                  {
                                      if (length(MonitorData) > 0 && nrow(MonitorData) > 0)
                                      {
                                          ValueSets <- list()

                                          ValueSets$Raw <- MonitorData %>%
                                                                select(Feature, Value.Raw, IsOccurring, IsEligible.Raw, Count.Raw) %>%
                                                                group_by(Feature) %>%
                                                                    mutate(Proportion_Raw = Count.Raw / sum(Count.Raw, na.rm = TRUE)) %>%
                                                                ungroup()

                                          ValueSets$Harmonized <- MonitorData %>%
                                                                      group_by(Feature, Value.Harmonized, IsEligible.Harmonized) %>%
                                                                          summarize(Count.Harmonized = sum(Count.Harmonized, na.rm = TRUE)) %>%
                                                                      ungroup() %>%
                                                                      distinct(Feature, Value.Harmonized, .keep_all = TRUE) %>%
                                                                      group_by(Feature) %>%
                                                                          mutate(Proportion_Harmonized = Count.Harmonized / sum(Count.Harmonized, na.rm = TRUE)) %>%
                                                                      ungroup()

                                          ValueSets$Recoded <- MonitorData %>%
                                                                    group_by(Feature, Value.Recoded, IsEligible.Recoded) %>%
                                                                        summarize(Count.Recoded = sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                    ungroup() %>%
                                                                    distinct(Feature, Value.Recoded, .keep_all = TRUE) %>%
                                                                    group_by(Feature) %>%
                                                                        mutate(Proportion_Recoded = Count.Recoded / sum(Count.Recoded, na.rm = TRUE)) %>%
                                                                    ungroup()

                                          ValueSets$Final <- MonitorData %>%
                                                                  group_by(Feature, Value.Final, IsEligible.Final) %>%
                                                                      summarize(Count.Final = sum(Count.Final, na.rm = TRUE)) %>%
                                                                  ungroup() %>%
                                                                  distinct(Feature, Value.Final, .keep_all = TRUE) %>%
                                                                  group_by(Feature) %>%
                                                                      mutate(Proportion_Final = Count.Final / sum(Count.Final, na.rm = TRUE)) %>%
                                                                  ungroup()

                                          return(ValueSets)

                                      } else { return(list()) }
                                  })



# Delete artificial "TrackID"-column from data frames (not needed anymore)
#===============================================================================

  DataSet <- DataSet %>%
                  map(function(Table)
                      {
                          try(Table %>% select(-TrackID))
                      })

  # Print info message
  cli::cat_bullet("Data transformation monitors are stored in 'CurationReport$Transformation'", bullet = "info")
  cat("\n")




#===============================================================================
# MODULE E)  Secondary record exclusion
#===============================================================================
#   - Remove ineligible records that may have been introduced during processing
#   - Same proceedings as in primary table cleaning (Module B))
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress::progress_bar$new(format = "Secondary exclusion: Excluding ineligible table records... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet), clear = FALSE, width = 100)
#-------------------------------------------------------------------------------

  DataSetRoot <- DataSet$Patient %>%
                      left_join(DataSet$Diagnosis, by = join_by(PatientID)) %>%
                      {   if (Settings$TableCleaning$Run == TRUE)
                          {
                              dsFreda::CleanTable(Table = .,
                                                  TableName = c("Diagnosis", "Patient"),
                                                  RemoveDuplicateRecords = FALSE,
                                                  FeatureRequirements = Settings$FeatureRequirements)
                          } else {.}
                      } %>%
                      select(PatientID, DiagnosisID) %>%
                      distinct()

  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          try(ProgressBar$tick())

                          if (!(is.null(Table) | length(Table) == 0 | nrow(Table) == 0))
                          {
                              # Join current table with preselection of 'DataSetRoot'
                              if (all(c("PatientID", "DiagnosisID") %in% names(Table)))
                              {
                                  Table <- DataSetRoot %>%
                                                left_join(Table, by = join_by(PatientID, DiagnosisID))

                              } else {

                                  Table <- DataSetRoot %>%
                                              select(PatientID) %>%
                                              distinct() %>%
                                              left_join(Table, by = join_by(PatientID))
                              }

                              # Clean current table using auxiliary function dsFreda::CleanTable()
                              if (Settings$TableCleaning$Run == TRUE)
                              {
                                  Table <- Table %>%
                                                dsFreda::CleanTable(TableName = tablename,
                                                                    RemoveDuplicateRecords = TRUE,
                                                                    FeatureRequirements = Settings$FeatureRequirements)
                              }

                              return(Table)

                          } else {

                              return(Table)
                          }
                       })

  try(ProgressBar$terminate())


#===============================================================================
# MONITORING: Count ineligible records after secondary exclusion
#===============================================================================

  # Count records in data frames after secondary exclusion
  CountRecords_AfterSecondaryExclusion <- DataSet %>%
                                              map_int(\(Table) ifelse (!is.null(nrow(Table)), nrow(Table), 0))

  # Count excluded records
  CountExcludedRecords_Secondary <- CountRecords_AfterPrimaryExclusion - CountRecords_AfterSecondaryExclusion


  # Print messages for live monitoring in local tests
  for (i in 1:length(CountExcludedRecords_Secondary))
  {
      Message <- paste0("Secondary exclusion: Removed ", CountExcludedRecords_Secondary[i], " ineligible records from '", names(CountExcludedRecords_Secondary)[i], "' table.")
      cli::cat_bullet(Message, bullet = "info")

      # Save messages in output object
      Messages$ExcludedRecords_Secondary <- c(Messages$ExcludedRecords_Secondary,
                                              Info = Message)
  }
  cat("\n")




#===============================================================================
# MODULE F)  Find and remove secondary redundancies (table records that can be considered redundant when they provide no additional informational value compared to a previous record)
#===============================================================================
# 1)  Process table 'Diagnosis' first, since other tables hold primary key 'DiagnosisID'.
#     Any DiagnosisIDs that are removed due to redundancy need to be replaced in dependent tables.
# 2)  Proceed with all other tables (excluding 'Patient')
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
  ProgressBar <- progress::progress_bar$new(format = "Removing redundant table records... [:bar] :percent in :elapsed  :spin",
                                  total = length(DataSet) - 1, clear = FALSE, width= 100)
#-------------------------------------------------------------------------------

  try(ProgressBar$tick())

  # Using dsFreda::FindRedundantRecords(), mark redundant records in table 'Diagnosis' for further processing
  Aux_DiagnosisRedundancies <- DataSet$Diagnosis %>%
                                    dsFreda::FindRedundantRecords(PrimaryKeyFeature = "DiagnosisID",
                                                                  DiscriminatoryFeatures = MetaData$Features %>%
                                                                                               filter(TableName.Curated == "Diagnosis", IsDiscriminatory == TRUE) %>%
                                                                                               pull(FeatureName.Curated),
                                                                  EssentialFeatures = MetaData$Features %>%
                                                                                           filter(TableName.Curated == "Diagnosis", IsEssential == TRUE) %>%
                                                                                           pull(FeatureName.Curated),
                                                                  RemoveRedundantRecords = FALSE)

  # Any DiagnosisIDs that are removed due to redundancy need to be replaced in dependent tables
  # Create a mapping structure to know which IDs to replace
  Aux_RedundanciesIDMapping <- Aux_DiagnosisRedundancies %>%
                                    filter(IsRedundant == TRUE) %>%
                                    select(PatientID, DiagnosisID, ReferenceID)

  # Afterwards, remove redundant records from 'Diagnosis'
  DataSet$Diagnosis <- Aux_DiagnosisRedundancies %>%
                            filter(IsRedundant == FALSE) %>%
                            select(-IsRedundant,
                                   -ReferenceID)

  # Find and remove redundant records in other tables (except 'Patient')
  DataSet <- DataSet %>%
                  imap(function(Table, tablename)
                       {
                          if (nrow(Table) > 0 & !(tablename %in% c("Patient", "Diagnosis")))
                          {
                              try(ProgressBar$tick())

                              # For all tables that use 'DiagnosisID', make sure the previously removed DiagnosisIDs are replaced by respective reference DiagnosisIDs...
                              if (all(c("PatientID", "DiagnosisID") %in% names(Table)))
                              {
                                  Table <- Table %>%
                                                left_join(Aux_RedundanciesIDMapping, by = join_by(PatientID,
                                                                                                  DiagnosisID)) %>%
                                                mutate(DiagnosisID = ifelse(!is.na(ReferenceID),
                                                                            ReferenceID,
                                                                            DiagnosisID)) %>%
                                                select(-ReferenceID)
                              }

                              # ... then proceed with secondary redundancy removal
                              Table <- Table %>%
                                            dsFreda::FindRedundantRecords(PrimaryKeyFeature = MetaData$Features %>%
                                                                                                   filter(TableName.Curated == tablename, IsPrimaryKey == TRUE) %>%
                                                                                                   pull(FeatureName.Curated),
                                                                          DiscriminatoryFeatures = MetaData$Features %>%
                                                                                                       filter(TableName.Curated == tablename, IsDiscriminatory == TRUE) %>%
                                                                                                       pull(FeatureName.Curated),
                                                                          EssentialFeatures = MetaData$Features %>%
                                                                                                   filter(TableName.Curated == tablename, IsEssential == TRUE) %>%
                                                                                                   pull(FeatureName.Curated),
                                                                          RemoveRedundantRecords = TRUE)

                          } else { return(Table) }
                       })

  try(ProgressBar$terminate())


#===============================================================================
# MONITORING: Count secondary redundancies
#===============================================================================

  # Count records in data frames after secondary redundancy removal
  CountRecords_AfterSecondaryRedundancyExclusion <- DataSet %>%
                                                        map_int(\(Table) ifelse (!is.null(nrow(Table)), nrow(Table), 0))

  # Count excluded records
  CountExcludedRecords_SecondaryRedundancy <- CountRecords_AfterSecondaryExclusion - CountRecords_AfterSecondaryRedundancyExclusion


  # Print messages for live monitoring in local tests
  for (i in 1:length(CountExcludedRecords_SecondaryRedundancy))
  {
      Message <- paste0("Secondary redundancy: Removed ", CountExcludedRecords_SecondaryRedundancy[i], " redundant records from '", names(CountExcludedRecords_SecondaryRedundancy)[i], "' table.")
      cli::cat_bullet(Message, bullet = "info")

      # Save messages in output object
      Messages$ExcludedRecords_SecondaryRedundancy <- c(Messages$ExcludedRecords_SecondaryRedundancy,
                                                        Info = Message)
  }
  cat("\n")




#===============================================================================
# Final modifications
#===============================================================================

  # Conversion of Tables from tibble to data.frame, because DataSHIELD can handle data.frames better
  DataSet <- DataSet %>%
                  map(\(Table) as.data.frame(Table))




#===============================================================================
# Compile content of 'CurationReport'
#===============================================================================

  CurationReport <- list(RecordCounts = data.frame(Table = names(DataSet),
                                                  InitialCount = CountRecords_Initial,
                                                  ExcludedPrimary = CountExcludedRecords_Primary,
                                                  AfterPrimaryExclusion = CountRecords_AfterPrimaryExclusion,
                                                  ExcludedSecondary = CountExcludedRecords_Secondary,
                                                  AfterSecondaryExclusion = CountRecords_AfterSecondaryExclusion,
                                                  ExcludedSecondaryRedundancy = CountExcludedRecords_SecondaryRedundancy,
                                                  AfterSecondaryRedundancyExclusion = CountRecords_AfterSecondaryRedundancyExclusion),
                         Transformation = list(Monitors = ls_TransformationMonitors,
                                               EligibilityOverviews = ls_EligibilityOverviews,
                                               ValueSetOverviews = ls_ValueSetOverviews))

  Messages$CheckCurationCompletion <- "green"
  Messages$FinalMessage <- "Data Curation performed successfully!"

  # Print completion message
  Message <- paste0("Data Curation performed successfully!")
  cli::cat_bullet(Message, bullet = "tick")
  cat("\n")


  # },
  #
  # # In case of occurring warning:
  # #===============================================================================
  # warning = function(w)
  #           {
  #               Messages$CheckCurationCompletion <- "yellow"
  #               Messages$FinalMessage <- paste0("Completed Curation with following warning: \n", w)
  #           },
  #
  # # In case of occurring error:
  # #===============================================================================
  # error = function(e)
  #         {
  #             Messages$CheckCurationCompletion <- "red"
  #             Messages$FinalMessage <- paste0("An error occured: \n", e)
  #         },
  #
  # #===============================================================================
  # # RETURN STATEMENT
  # #===============================================================================
  # finally =
  # {
  #   # Return the Curated Data Set (CDS) a Curation Report (defined above) and Messages
    return(list(CuratedDataSet = DataSet,
                CurationReport = CurationReport,
                Messages = Messages))
  # })

}

