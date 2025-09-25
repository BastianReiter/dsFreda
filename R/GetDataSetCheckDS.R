
#' GetDataSetCheckDS
#'
#' Checks out a given data set (list of data.frames) and returns an informative list object.
#'
#' Server-side AGGREGATE method
#'
#' @param DataSetName.S \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param RequiredTableNames.S \code{character} - Names of tables that are expected/required to be in the data set - Default: Names of elements in object evaluated from \code{DataSetName.S}
#' @param RequiredFeatureNames.S \code{list} of \code{character vectors} - Features that are expected/required in each table of the data set - Default: Names of features in respective table
#' @param AssumeCCPDataSet.S \code{logical} - Whether or not the data set to be checked out is one of the main data sets used in CCPhos - Default: FALSE
#'
#' @return A \code{list} containing meta data about tables in a data set
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetDataSetCheckDS <- function(DataSetName.S,
                              RequiredTableNames.S = NULL,
                              RequiredFeatureNames.S = NULL,
                              AssumeCCPDataSet.S = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(dplyr)
  require(purrr)
  require(stringr)

  # --- For Testing Purposes ---
  # DataSetName.S <- "RawDataSet"
  # DataSet <- RawDataSet
  # RequiredTableNames.S <- NULL
  # RequiredFeatureNames.S <- NULL
  # AssumeCCPDataSet.S <- TRUE
  # RequiredTableNames.S = paste0("RDS_", dsCCPhos::Meta.Tables$TableName.Curated)
  # RequiredFeatureNames.S = RequiredTableNames.S %>%
  #                             map(\(tablename) filter(dsCCPhos::Meta.Features, TableName.Curated == str_remove(tablename, "RDS_"))$FeatureName_Raw) %>%
  #                             set_names(RequiredTableNames.S)

  # --- Argument Assertions ---
  assert_that(is.string(DataSetName.S),
              is.flag(AssumeCCPDataSet.S))
  if (!is.null(RequiredTableNames.S)) { assert_that(is.character(RequiredTableNames.S)) }
  if (!is.null(RequiredFeatureNames.S)) { assert_that(is.list(RequiredFeatureNames.S)) }

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  DataSet <- eval(parse(text = DataSetName.S), envir = parent.frame())

#-------------------------------------------------------------------------------


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check existence and completeness of tables
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # If argument 'RequiredTableNames.S' is not passed, assign present table names in 'DataSet' per default
  if (is.null(RequiredTableNames.S)) { RequiredTableNames.S <- names(DataSet) }

  # If no feature names list is passed, assign an empty list
  if (is.null(RequiredFeatureNames.S)) { RequiredFeatureNames.S <- list() }


  if (AssumeCCPDataSet.S == TRUE)
  {
      if (DataSetName.S == "RawDataSet")
      {
          RequiredTableNames.S <- paste0("RDS.", dsCCPhos::Meta.Tables$TableName.Curated)
          RequiredFeatureNames.S <- RequiredTableNames.S %>%
                                        map(\(tablename) filter(dsCCPhos::Meta.Features, TableName.Curated == str_remove(tablename, "RDS_"))$FeatureName_Raw) %>%
                                        set_names(RequiredTableNames.S)
      }
      if (DataSetName.S == "CuratedDataSet")
      {
          RequiredTableNames.S <- dsCCPhos::Meta.Tables$TableName.Curated
          RequiredFeatureNames.S <- RequiredTableNames.S %>%
                                        map(\(tablename) filter(dsCCPhos::Meta.Features, TableName.Curated == tablename)$FeatureName_Curated) %>%
                                        set_names(RequiredTableNames.S)
      }
  }


  # Create Table check templates for all required Data Set tables ("assume as empty/missing")
  DataSetCheckTemplate <- RequiredTableNames.S %>%
                              map(function(tablename)
                                  {
                                      CheckTable(Table = NULL,
                                                 RequiredFeatureNames = RequiredFeatureNames.S[[tablename]])

                                  }) %>%
                              set_names(RequiredTableNames.S)


  # Go through actually existing tables in 'DataSet' and check for feature completeness as well as feature types, row counts and rates of non-missing values
  DataSetCheckExisting <- DataSet %>%
                              imap(function(Table, tablename)
                                   {
                                      if (length(Table) > 0 && !is.null(Table) && !is_empty(Table) && nrow(Table) > 0)
                                      {
                                          CheckTable(Table = Table,
                                                     RequiredFeatureNames = RequiredFeatureNames.S[[tablename]])
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
