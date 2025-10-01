
#' GetDataSetCheckDS
#'
#' Checks out a data set (list of data.frames) and returns an informative list object. Options:
#' \enumerate{
#'    \item The data set can be inspected naively without passing any additional information about it
#'    \item Meta data about the data set can be passed in the form of a \code{list} containing the \code{data.frames} 'Meta.Tables' / 'Meta.Features' / 'Meta.Values'
#'    \item A Module identifier from a list of registered modules can be passed ('CCP' / 'P21' / ...), which leads to meta data being taken from a linked package }
#'
#' Server-side AGGREGATE method
#'
#' @param DataSetName.S \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param DataSetMetaData.S Optional \code{list} of \code{data.frames} 'Meta.Tables' / 'Meta.Features' / 'Meta.Values'
#' @param Module.S Optional \code{string} identifying a defined data set (Examples: 'CCP' / 'P21')
#' @param TransformationStage.S Optional \code{string} - Indicating transformation stage of addressed data set. This is relevant for which names and values to look up in passed meta data. Options: 'Raw' / 'Curated'
#'
#' @return A \code{list} containing meta data about tables in a data set
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetDataSetCheckDS <- function(DataSetName.S,
                              DataSetMetaData.S = NULL,
                              Module.S = NULL,
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
  # DataSetMetaData.S <- NULL
  # Module.S <- "CCP"
  # TransformationStage.S <- "Raw"

  # --- Argument Assertions ---
  assert_that(is.string(DataSetName.S),
              is.string(TransformationStage.S))
  if (!is.null(DataSetMetaData.S)) { assert_that(is.list(DataSetMetaData.S)) }
  if (!is.null(Module.S)) { assert_that(is.string(Module.S)) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  DataSet <- eval(parse(text = DataSetName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  # Initiate defaults (if no meta data is passed)
  Meta.Tables <- NULL
  Meta.Features <- NULL
  Meta.Values <- NULL
  RequiredTableNames <- names(DataSet)      # Required table names are just the present table names
  RequiredFeatureNames <- list()
  EligibleValueSets <- list()

  if (!is.null(DataSetMetaData.S))
  {
      Meta.Tables <- DataSetMetaData.S$Meta.Tables
      Meta.Features <- DataSetMetaData.S$Meta.Features
      Meta.Values <- DataSetMetaData.S$Meta.Values
  }

  if (!is.null(Module.S) && Module.S %in% names(dsFreda::ModuleRegistration))
  {
      ModulePackageName <- dsFreda::ModuleRegistration[[Module.S]]
      Meta.Tables <- eval(parse(text = paste0(ModulePackageName, "::Meta.Tables")))
      Meta.Features <- eval(parse(text = paste0(ModulePackageName, "::Meta.Features")))
      Meta.Values <- eval(parse(text = paste0(ModulePackageName, "::Meta.Values")))
  }


  # If meta data is passed, get required table and feature names from it
  if (length(Meta.Tables) > 0 & length(Meta.Features) > 0 & length(Meta.Values) > 0)
  {
      TableNameColumn <- "TableName.Curated"      # Once tables are loaded into R session they should already have 'curated' table names

      # Defining relevant column names depending on transformation stage ('Raw' or 'Curated')
      FeatureNameColumn <- paste0("FeatureName.", TransformationStage.S)
      ValueColumn <- paste0("Value.", TransformationStage.S)

      # Get required table names as character vector
      RequiredTableNames <- Meta.Tables %>%
                                pull({{ TableNameColumn }})

      # Get required feature names as list of character vectors (one for each table)
      RequiredFeatureNames <- RequiredTableNames %>%
                                  map(\(tablename) Meta.Features %>%
                                                        filter(TableName.Curated == tablename) %>%
                                                        pull({{ FeatureNameColumn }})) %>%
                                  set_names(nm = RequiredTableNames)

      # Get sets of eligible value sets as a list of lists of character vectors
      EligibleValueSets <- RequiredTableNames %>%
                                map(function(tablename)
                                    {
                                        RelevantFeatures <- Meta.Values %>%
                                                                filter(Table == tablename) %>%
                                                                pull({{ FeatureNameColumn }}) %>%
                                                                unique()

                                        ValueSet <- RelevantFeatures %>%
                                                        map(\(featurename) Meta.Values %>%
                                                                                filter(Table == tablename,
                                                                                       .data[[FeatureNameColumn]] == featurename) %>%
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
                                      if (length(Table) > 0 && nrow(Table) > 0)
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
