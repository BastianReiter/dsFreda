
#' PrepareRawDataDS
#'
#' Perform basic preparatory transformations on RawDataSet prior to Curation:
#' \itemize{  \item Optionally convert all columns to character type
#'            \item Add ID feature (running number) for tables without primary key feature
#'            \item Try to harmonize feature names employing Fuzzy String Matching and Dictionary look-up }
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{string} - Name of Raw Data Set object (list) on server - Default: 'P21.RawDataSet'
#' @param Module.S \code{string} identifying a defined data set and the corresponding meta data needed for feature name harmonization (Examples: 'CCP' / 'P21')
#' @param FeatureNameDictionary.S Optional \code{list} containing dictionary data for feature name harmonization (Form: \code{list(Department = c(FAB = "Fachabteilung"))})
#' @param AddIDFeature.S \code{list} containing parameters about adding an ID feature to tables:
#'                            \itemize{ \item Do (\code{logical}) - Whether to add an ID feature (running number)
#'                                      \item IDFeatureName (\code{string})
#'                                      \item OverwriteExistingIDFeature (\code{logical}) - Whether to overwrite an existing feature with the same name }
#' @param CompleteCharacterConversion.S \code{logical} - Indicating whether to convert all features in data set tables to character type
#'
#' @return A \code{list} containing
#'          \itemize{ \item The input RawDataSet (\code{list}) with harmonized feature names
#'                    \item The original input RawDataSet (\code{list})
#'                    \item Messages (\code{character vector}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PrepareRawDataDS <- function(RawDataSetName.S,
                             Module.S,
                             FeatureNameDictionary.S = list(),
                             AddIDFeature.S = list(Do = TRUE,
                                                   IDFeatureName = "ID",
                                                   OverwriteExistingIDFeature = FALSE),
                             CompleteCharacterConversion.S = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # RawDataSetName.S <- "P21.RawDataSet"
  # FeatureNameDictionary.S <- list(Department = c(FAB = "Fachabteilung"))

  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName.S),
              is.string(Module.S),
              is.list(FeatureNameDictionary.S),
              is.list(AddIDFeature.S),
              is.flag(AddIDFeature.S$Do),
              is.string(AddIDFeature.S$IDFeatureName),
              is.flag(AddIDFeature.S$OverwriteExistingIDFeature),
              is.flag(CompleteCharacterConversion.S))

#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  RawDataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())
  # Save a 'backup' of the input data set (this will be part of output so changes to feature names can be tracked)
  OriginalRawDataSet <- RawDataSet

#-------------------------------------------------------------------------------

  # Initiate output messaging object
  Messages <- list(RawFeatureNames = character())

  # Get package name from 'Meta.Modules'
  ModulePackageName <- dsFreda::Meta.Modules[[Module.S]]
  # Get package-owned module-specific meta data
  Meta.Features.Module <- eval(parse(text = paste0(ModulePackageName, "::Meta.Features")))


# Performing preparatory operations
#-------------------------------------------------------------------------------
  Preparation <- RawDataSet %>%
                    imap(function(Table, tablename)
                         {
                            # Initiate messaging vector for current 'Table'
                            CurrentMessages <- character()

                            # Optionally convert all columns to character type
                            #---------------------------------------------------
                            if (CompleteCharacterConversion.S == TRUE)
                            {
                                Table <- Table %>%
                                            mutate(across(everything(),
                                                          ~ as.character(.x)))
                                                          #~ vctrs::vec_cast(.x, character())))      # Using vctrs::vec_cast() to avoid implicit conversions performed with base::as.character()

                                Message <- paste0("Table '", tablename, "': Converted all features to character type.")
                                cli::cat_bullet(Message, bullet = "info")
                                CurrentMessages <- c(CurrentMessages, Info = Message)
                            }

                            # Optionally add ID feature (running number) to Table
                            #---------------------------------------------------
                            if (AddIDFeature.S$Do == TRUE)
                            {
                                TableHasIDFeature <- AddIDFeature.S$IDFeatureName %in% names(Table)

                                if (TableHasIDFeature == FALSE || (TableHasIDFeature == TRUE & AddIDFeature.S$OverwriteExistingIDFeature == TRUE))
                                {
                                    Table <- Table %>%
                                                eval(expr = parse(text = paste0("mutate(., '",
                                                                                AddIDFeature.S$IDFeatureName,
                                                                                "' = 1:n(), .before = 1)")))
                                }
                            }

                            # Try to harmonize raw feature names using fuzzy string matching and dictionary data
                            #---------------------------------------------------

                            # Get expected raw feature names from module-specific meta data
                            EligibleFeatureNames <- Meta.Features.Module %>%
                                                        filter(TableName.Curated == tablename) %>%
                                                        pull(FeatureName.Raw)

                            # Get Dictionary (character vector) from passed list
                            FeatureNameDictionary <- FeatureNameDictionary.S[[tablename]]

                            # First, try Fuzzy String Matching (also matching to Dictionary look up values if possible, because these will subsequentially turn into eligible feature names)
                            HarmonizedFeatureNames <- GetFuzzyStringMatches(Vector = names(Table),
                                                                            EligibleStrings = c(EligibleFeatureNames, names(FeatureNameDictionary)),
                                                                            PreferredMethod = "jw",
                                                                            Tolerance = 0.2)

                            # Try Dictionary Look-up, if dictionary data is passed
                            if (length(FeatureNameDictionary) > 0)
                            {
                                HarmonizedFeatureNames <- if_else(is.na(FeatureNameDictionary[HarmonizedFeatureNames]),
                                                                  HarmonizedFeatureNames,
                                                                  FeatureNameDictionary[HarmonizedFeatureNames])
                            }

                            # Create tibble containing transformation tracks of all feature names
                            FeatureNames <- tibble(Original = names(Table),
                                                   IsEligible.Original = (Original %in% EligibleFeatureNames),
                                                   Harmonized = HarmonizedFeatureNames,
                                                   ChosenName = case_when(IsEligible.Original == FALSE ~ Harmonized,
                                                                          .default = Original),
                                                   IsEligible.ChosenName = (ChosenName %in% EligibleFeatureNames),
                                                   Changed = !(Original == ChosenName))

                            # Re-assign names to current 'Table', possibly changing original feature names
                            names(Table) <- FeatureNames$ChosenName

                            # Obtain changed feature names for messaging
                            ChangedNames <- FeatureNames %>%
                                                filter(Changed == TRUE)

                            if (length(ChangedNames) == 0 || nrow(ChangedNames) == 0)
                            {
                                Message <- paste0("Table '", tablename, "': No changes to raw feature names.")
                                cli::cat_bullet(Message, bullet = "info")
                                CurrentMessages <- c(CurrentMessages,
                                                     Info = Message)

                            } else {

                                for (i in 1:nrow(ChangedNames))
                                {
                                    Message <- paste0("Table '", tablename, "': Changed feature name '", ChangedNames$Original[i], "' to '", ChangedNames$ChosenName[i], "'.")
                                    cli::cat_bullet(Message, bullet = "info")
                                    CurrentMessages <- c(CurrentMessages,
                                                         Info = Message)
                                }
                            }

                            # Obtain remaining ineligible feature names
                            RemainingIneligibleNames <- FeatureNames %>%
                                                            filter(IsEligible.ChosenName == FALSE)

                            if (length(RemainingIneligibleNames) > 0 && nrow(RemainingIneligibleNames) > 0)
                            {
                                for (i in 1:nrow(ChangedNames))
                                {
                                    Message <- paste0("Table '", tablename, "': The feature name '", RemainingIneligibleNames$Original[i], "' is ineligible and could not be harmonized!")
                                    cli::cat_bullet(Message, bullet = "warning", bullet_col = "red")
                                    CurrentMessages <- c(CurrentMessages,
                                                         Warning = Message)
                                }
                            }

                            return(list(Table = Table,
                                        Messages = CurrentMessages))
                         })

# Extract RawDataSet and Messages
#-------------------------------------------------------------------------------
  RawDataSet <- Preparation %>% map("Table")
  Messages <- Preparation %>%
                  map("Messages") %>%
                  unlist() %>%
                  set_names(sub(".*\\.", "", names(.)))

#-------------------------------------------------------------------------------
  return(list(RawDataSet = RawDataSet,
              OriginalRawDataSet = OriginalRawDataSet,
              Messages = Messages))
}
