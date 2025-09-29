
#' GetDataSetCheckDS
#'
#' Checks out a given data set (list of data.frames) and returns an informative list object.
#'
#' Server-side AGGREGATE method
#'
#' @param DataSetName.S \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param DataSetMetaData.S Optional \code{list} of \code{data.frames} 'Meta.Tables' / 'Meta.Features' / 'Meta.Values'
#' @param RequiredTableNames.S Optional \code{character} - Names of tables that are expected/required to be in the data set - Default: Names of elements in object evaluated from \code{DataSetName.S}
#' @param RequiredFeatureNames.S Optional \code{list} of \code{character vectors} - Features that are expected/required in each table of the data set - Default: Names of features in respective table
#' @param EligibleValueSets.S \code{list} of \code{character} vectors containing sets of eligible values for corresponding feature
#' @param TransformationStage.S Optional \code{string} - Indicating transformation stage of addressed data set. This is relevant for which names and values to look up in passed meta data. Options: 'Raw' / 'Curated' / 'Augmented'
#'
#' @return A \code{list} containing meta data about tables in a data set
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetDataSetCheckDS <- function(DataSetName.S,
                              DataSetMetaData.S = NULL,
                              TransformationStage.S = "Raw")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)
  require(purrr)
  require(stringr)

  # --- For Testing Purposes ---
  # DataSetName.S <- "RawDataSet"
  # DataSet <- RawDataSet
  # DataSetMetaData <- NULL
  # RequiredTableNames.S <- NULL
  # RequiredFeatureNames.S <- NULL
  # AssumeCCPDataSet.S <- TRUE
  # RequiredTableNames.S = paste0("RDS_", dsCCPhos::Meta.Tables$TableName.Curated)
  # RequiredFeatureNames.S = RequiredTableNames.S %>%
  #                             map(\(tablename) filter(dsCCPhos::Meta.Features, TableName.Curated == str_remove(tablename, "RDS_"))$FeatureName_Raw) %>%
  #                             set_names(RequiredTableNames.S)

  # --- Argument Assertions ---
  assert_that(is.string(DataSetName.S),
              is.string(TransformationStage.S))
  if (!is.null(DataSetMetaData.S)) { assert_that(is.list(DataSetMetaData.S)) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  DataSet <- eval(parse(text = DataSetName.S), envir = parent.frame())

#-------------------------------------------------------------------------------


  # Default (if no meta data is passed): Required table names are just the present table names
  RequiredTableNames <- names(DataSet)
  RequiredFeatureNames <- list()
  EligibleValueSets <- NULL


  # If meta data is passed, get required table and feature names from it
  if (!is.null(DataSetMetaData.S$Meta.Tables) & !is.null(DataSetMetaData.S$Meta.Features))
  {
      # Defining relevant column names depending on transformation stage ('Raw' or 'Curated')
      TableNameColumn <- paste0("TableName.", TransformationStage.S)
      FeatureNameColumn <- paste0("FeatureName.", TransformationStage.S)
      ValueColumn <- paste0("Value.", TransformationStage.S)

      # Get required table names as character vector
      RequiredTableNames <- DataSetMetaData.S$Meta.Tables %>%
                                pull({{ TableNameColumn }})

      # Get required feature names as list of character vectors (one for each table)
      RequiredFeatureNames <- RequiredTableNames %>%
                                  map(\(tablename) DataSetMetaData.S$Meta.Features %>%
                                                        filter(TableName.Curated == tablename) %>%
                                                        pull({{ FeatureNameColumn }})) %>%
                                  set_names(nm = RequiredTableNames)

      # Get sets of eligible value sets as a list of lists of character vectors
      EligibleValueSets <- RequiredTableNames %>%
                                map(function(tablename)
                                    {
                                        RelevantFeatures <- DataSetMetaData.S$Meta.Values %>%
                                                                filter(Table == tablename) %>%
                                                                pull(Feature) %>%
                                                                unique()

                                        ValueSet <- RelevantFeatures %>%
                                                        map(\(featurename) DataSetMetaData.S$Meta.Values %>%
                                                                                filter(Table == tablename,
                                                                                       Feature == featurename) %>%
                                                                                pull({{ ValueColumn }})) %>%
                                                        set_names(nm = RelevantFeatures)

                                        return(ValueSet)
                                    }) %>%
                                set_names(nm = RequiredTableNames)
  }


  # Create Table check templates for all required Data Set tables ("assume as empty/missing")
  DataSetCheckTemplate <- RequiredTableNames %>%
                              map(function(tablename)
                                  {
                                      CheckTable(Table = NULL,
                                                 RequiredFeatureNames = RequiredFeatureNames[[tablename]])

                                  }) %>%
                              set_names(RequiredTableNames)


  # Go through actually existing tables in 'DataSet' and check for feature completeness as well as feature types, row counts and rates of non-missing values
  DataSetCheckExisting <- DataSet %>%
                              imap(function(Table, tablename)
                                   {
                                      if (length(Table) > 0 && !is.null(Table) && !is_empty(Table) && nrow(Table) > 0)
                                      {
                                          CheckTable(Table = Table,
                                                     RequiredFeatureNames = RequiredFeatureNames[[tablename]],
                                                     EligibleValueSets = EligibleValueSets[[tablename]])
                                      }
                                      else { return(NULL) }
                                   }) %>%
                              compact()      # This removes all NULL elements from list


  # Replace info in TableCheckTemplate with info about existing tables to get coherent summary
  DataSetCheck <- DataSetCheckTemplate %>%
                      imap(function(Table, tablename)
                           {
                              if (tablename %in% names(DataSetCheckExisting)) { return(DataSetCheckExisting[[tablename]]) }
                              else { return(Table) }
                           })

#-------------------------------------------------------------------------------
  return(DataSetCheck)
}
