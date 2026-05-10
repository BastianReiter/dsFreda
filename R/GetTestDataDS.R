
#' GetTestDataDS
#'
#' Get randomized test data from servers
#'
#' Server-side AGGREGATE method
#'
#' @param DataSetName.S \code{string} - Name of a \code{list} of \code{data.frames} representing a data set
#' @param TableName.S \code{string} - Name of a specific \code{data.frame}
#'
#' @return A
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GetTestDataDS <- function(DataSetName.S = NA_character_,
                          TableName.S = NA_character_,
                          SampleSize.S,
                          Shuffle.S = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # Table <- AugmentationOutput$AugmentedDataSet$Patient
  # FeaturesNames.S <- c("Sex", "CountDiagnoses")

  # --- Argument Validation ---
  assert_that(is.string(DataSetName.S),
              is.string(TableName.S),
              is.count(SampleSize.S),
              is.flag(Shuffle.S))

#-------------------------------------------------------------------------------

  # Get fixed Freda privacy settings
  PrivacyProfile <- dsFreda::Set.PrivacyProfile.Chosen
  PrivacySettings <- dsFreda::Set.PrivacyProfiles[[PrivacyProfile]]

  # Get local object: Parse expression and evaluate
  DataSet <- eval(parse(text = DataSetName.S), envir = parent.frame())

  # Get local object: Parse expression and evaluate
  # Table <- eval(parse(text = TableName.S), envir = parent.frame())

#-------------------------------------------------------------------------------

  AllPatientIDs <- DataSet$Patient$PatientID
  AvailableNumberPatients <- length(unique(AllPatientIDs))

  # Reduce SampleSize.S if necessary
  if (SampleSize.S > AvailableNumberPatients) { SampleSize.S <- AvailableNumberPatients }

  # Get a random sample of PatientIDs
  SampleIDs <- sample(AllPatientIDs,
                      size = SampleSize.S)

  # Subset RDS tables with sampled PatientIDs
  DataSetSample <- DataSet %>%
                        imap(function(Table, tablename)
                             {
                                if (length(Table) > 0 && nrow(Table) > 0)
                                {
                                    if (Shuffle.S == TRUE)
                                    {
                                        # Randomly shuffle all column vectors
                                        Table <- Table %>% mutate(across(everything(), ~ sample(.x)))
                                    }

                                    return(filter(Table, Table$PatientID %in% SampleIDs))
                                }
                                else { return(NULL) }
                             })

#-------------------------------------------------------------------------------

  if (PrivacyProfile == "Development")
  {
      return(DataSetSample)
  } else {
      return(NULL)
  }
}
