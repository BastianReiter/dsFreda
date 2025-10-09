
#' GetRDSValidationReportDS
#'
#' Performs validation operations on Raw Data Set (RDS) and returns a list containing informative data frames.
#'
#' Server-side AGGREGATE method
#'
#' @param RawDataSetName.S \code{string} - Name of Raw Data Set object (list) on server - Default: 'RawDataSet'
#'
#' @return A \code{list} of validation report \code{data.frames}
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetRDSValidationReportDS <- function(RawDataSetName.S = "RawDataSet")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # RawDataSetName.S <- "RawDataSet"

  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName.S))

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  RawDataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Rename features in tables to make sure R object naming rules are respected (this renaming only extends to the scope of this function)
#-------------------------------------------------------------------------------

  RawDataSet <- RawDataSet %>%
                    imap(function(dataframe, name)
                         {
                            # Create named vector to look up matching feature names in meta data ('OldName' = 'NewName')
                            name <- str_remove(name, "RDS.")   # Remove "RDS." prefix from table names
                            vc_Lookup <- dplyr::filter(dsCCPhos::Meta.Features, TableName.Curated == name)$FeatureName.Raw
                            names(vc_Lookup) <- dplyr::filter(dsCCPhos::Meta.Features, TableName.Curated == name)$FeatureName.Curated

                            if (!is_empty(dataframe))
                            {
                                # Rename feature names according to look-up vector
                                dplyr::rename(dataframe, any_of(vc_Lookup))      # Returns a tibble
                            }
                            else { return(dataframe) }
                         })


#-------------------------------------------------------------------------------
# Define validation rules in data frames according to syntax demanded by package "validate"
#-------------------------------------------------------------------------------

  ValidationRules <- names(RawDataSet) %>%
                        map(function(tablename)
                             {
                                # Get table feature names from meta data
                                tablename <- str_remove(tablename, "RDS.")   # Remove "RDS." prefix from table names
                                # Raw feature names
                                vc_FeatureNames <- dplyr::filter(dsCCPhos::Meta.Features, TableName.Curated == tablename)$FeatureName.Raw
                                # Corresponding curated feature names (used for ensured compatibility with R naming rules)
                                names(vc_FeatureNames) <- dplyr::filter(dsCCPhos::Meta.Features, TableName.Curated == tablename)$FeatureName.Curated

                                # Validation rules: data type
                                Rules_DataType <- names(vc_FeatureNames) %>%
                                                      map(\(curatedfeaturename) data.frame(name = paste0("DataTypeCharacter...", tablename, "...", vc_FeatureNames[curatedfeaturename]),
                                                                                           description = "",
                                                                                           rule = paste0("is.character(", curatedfeaturename, ")"))) %>%
                                                      list_rbind()

                                # Validation rules: missing data
                                Rules_Missings <- names(vc_FeatureNames) %>%
                                                      map(\(curatedfeaturename) data.frame(name = paste0("NonMissingValues...", tablename, "...", vc_FeatureNames[curatedfeaturename]),
                                                                                           description = "",
                                                                                           rule = paste0("!is.na(", curatedfeaturename, ")"))) %>%
                                                      list_rbind()


                                # Return consolidated data frame of validation rules
                                return(rbind(Rules_DataType,
                                             Rules_Missings))

                             }) %>% set_names(names(RawDataSet))


#-------------------------------------------------------------------------------
# Perform validation
#-------------------------------------------------------------------------------
# - Functionality of package 'validate': Confront each data frame with a 'validator' based on the corresponding validation rule set
# - Obtain summary (data frame) of the resulting validation process
#-------------------------------------------------------------------------------

  ValidationReports <- pmap(.l = list(RawDataSet,
                                      ValidationRules),
                            .f = \(dataframe, ruleset) validate::confront(dataframe,
                                                                          validator::validator(.data = ruleset)))

  # ValidationSummaries <- ValidationReports %>%
  #                             map(\(report) summary(report))
  #
  #
  # ValidationReportTables <- ValidationSummaries %>%
  #                               map(\(summary) as.data.frame(summary, check.names = FALSE))

#-------------------------------------------------------------------------------
  return(ValidationReports)
}
