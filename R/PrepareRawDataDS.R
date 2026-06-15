
#' PrepareRawDataDS
#'
#' Perform basic preparatory transformations on RawDataSet prior to Curation:
#' \itemize{  \item Feature name harmonization
#'            \item Add ID feature (running number) for tables without primary key feature
#'            \item Type- or format-conversion of features }
#'
#' Server-side ASSIGN method
#'
#' @param RawDataSetName.S \code{string} - Name of Raw Data Set object (list) on server - Default: 'P21.RawDataSet'
#' @param Module.S \code{string} identifying a defined data set and the corresponding meta data needed for feature name harmonization (Examples: 'CCP' / 'P21')
#' @param FeatureNames.Dictionary.S Optional \code{list} containing dictionary data for raw feature name harmonization (Form: \code{list(Department = c(FAB = "Fachabteilung"))})
#' @param FeatureNames.FuzzyStringMatching.Run.S \code{logical} - Whether to use fuzzy string matching to harmonize raw feature names
#' @param FeatureNames.FuzzyStringMatching.PreferredMethod.S \code{logical} - Selects the method \code{stringdist()} uses (see \code{stringdist} documentation) - Default: 'jw'
#' @param FeatureNames.FuzzyStringMatching.Tolerance.S \code{logical} - Number between 0 and 1 relating to normalized distance between to strings (1 meaning furthest distance, 0 meaning no distance). If a string in 'Vector' is not similar enough to any of the 'EligibleStrings' and its minimal harmonized distance exceeds this number, it is set \code{NA}. - Default: 0.2
#' @param AddIDFeature.Do.S \code{logical} - Whether to add an ID feature (running number) to tables
#' @param AddIDFeature.IDFeatureName.S \code{string} - The name of the new ID feature
#' @param AddIDFeature.OverwriteExistingIDFeature.S \code{logical} - Whether to overwrite an existing feature with the same name
#' @param Conversion.IntoCharacter.S \code{string} - Controls conversion of certain features in data set tables into character type. One of 'None' / 'All' / 'Date'. - Default: 'None'
#' @param Conversion.DateIntoPOSIXct.S \code{string} - An encoded string representing a list with character vectors containing date formats used in argument 'tryFormats' in function \code{base::as.POSIXct}. List element should be either '.All' for all date features or specific date feature names. - Default: \code{NULL}
#' @param CurateFeatureNames.S \code{logical} - Indicating whether (after primary harmonization) feature names should be recoded from 'raw' to 'curated' feature names according to Module-specific meta data
#'
#' @return A \code{list} containing
#'          \itemize{ \item The input RawDataSet (\code{list}) with harmonized (and optionally curated) feature names
#'                    \item The original input RawDataSet (\code{list})
#'                    \item Messages (\code{character vector}) }
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PrepareRawDataDS <- function(RawDataSetName.S,
                             Module.S,
                             FeatureNames.Dictionary.S = list(),
                             FeatureNames.FuzzyStringMatching.Run.S = FALSE,
                             FeatureNames.FuzzyStringMatching.PreferredMethod.S = "jw",
                             FeatureNames.FuzzyStringMatching.Tolerance.S = 0.2,
                             AddIDFeature.Do.S = FALSE,
                             AddIDFeature.IDFeatureName.S = "ID",
                             AddIDFeature.OverwriteExistingIDFeature.S = FALSE,
                             Conversion.IntoCharacter.S = "None",
                             Conversion.DateIntoPOSIXct.S = NULL,
                             CurateFeatureNames.S = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  # RawDataSetName.S <- "P21.RawDataSet"
  # Module.S <- "P21"
  # FeatureNames.Dictionary.S <- list(Department = c(FAB = "Fachabteilung"))
  # FeatureNames.FuzzyStringMatching.Run.S <- TRUE
  # FeatureNames.FuzzyStringMatching.PreferredMethod.S <- "jw"
  # FeatureNames.FuzzyStringMatching.Tolerance.S <- 0.2
  # AddIDFeature.Do.S <- FALSE
  # AddIDFeature.IDFeatureName.S <- "ID"
  # AddIDFeature.OverwriteExistingIDFeature.S <- FALSE
  # Conversion.IntoCharacter.S <- "None"
  # Conversion.DateIntoPOSIXct.S <- .encode_tidy_eval("list('.All' = c('%Y%m%d%H%M', '%Y%m%d', '%Y-%m-%d'))", .get_encode_dictionary())
  # CurateFeatureNames.S <- TRUE

  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName.S),
              is.string(Module.S),
              is.list(FeatureNames.Dictionary.S),
              is.flag(FeatureNames.FuzzyStringMatching.Run.S),
              is.string(FeatureNames.FuzzyStringMatching.PreferredMethod.S),
              is.number(FeatureNames.FuzzyStringMatching.Tolerance.S),
              is.flag(AddIDFeature.Do.S),
              is.string(Conversion.IntoCharacter.S),
              is.flag(CurateFeatureNames.S))
  if (FeatureNames.FuzzyStringMatching.Tolerance.S < 0 | FeatureNames.FuzzyStringMatching.Tolerance.S > 1) { stop("ERROR: Value of argument 'FeatureNames.FuzzyStringMatching.Tolerance.S' must be between 0 and 1.") }
  if (!is.null(AddIDFeature.IDFeatureName.S)) { assert_that(is.string(AddIDFeature.IDFeatureName.S)) }
  if (!is.null(AddIDFeature.OverwriteExistingIDFeature.S)) { assert_that(is.flag(AddIDFeature.OverwriteExistingIDFeature.S)) }
  if (!(Conversion.IntoCharacter.S %in% c("None", "All", "Date"))) { stop("ERROR: Value of argument 'Conversion.IntoCharacter.S' must be one of 'None' / 'All' / 'Date'.") }
  if (!is.null(Conversion.DateIntoPOSIXct.S)) { assert_that(is.string(Conversion.DateIntoPOSIXct.S)) }

#===============================================================================
# - OVERVIEW -
#===============================================================================
#   A)  Optionally add ID feature
#   B)  Try to harmonize raw feature names
#   C)  Optionally curate / recode feature names according to feature meta data
#   D)  Optionally convert features to character type
#   E)  Optionally convert date features to POSIXct type
#-------------------------------------------------------------------------------

  # Get local object: Parse expression and evaluate
  RawDataSet <- eval(parse(text = RawDataSetName.S), envir = parent.frame())
  # Save a 'backup' of the input data set (this will be part of output so changes to feature names can be tracked)
  OriginalRawDataSet <- RawDataSet

  # Decode string 'Conversion.DateIntoPOSIXct.S' and recreate list object from the result
  if (length(Conversion.DateIntoPOSIXct.S) > 0)
  {
      Conversion.DateIntoPOSIXct.S <- eval(parse(text = .decode_tidy_eval(Conversion.DateIntoPOSIXct.S, .get_encode_dictionary())))
      assert_that(is.list(Conversion.DateIntoPOSIXct.S))
  }

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

                            #---------------------------------------------------
                            # A) Optionally add ID feature (running number) to Table
                            #---------------------------------------------------
                            if (AddIDFeature.Do.S == TRUE)
                            {
                                TableHasIDFeature <- AddIDFeature.IDFeatureName.S %in% names(Table)

                                if (TableHasIDFeature == FALSE || (TableHasIDFeature == TRUE & AddIDFeature.OverwriteExistingIDFeature.S == TRUE))
                                {
                                    Table <- Table %>%
                                                eval(expr = parse(text = paste0("mutate(., '",
                                                                                AddIDFeature.IDFeatureName.S,
                                                                                "' = 1:n(), .before = 1)")))
                                }
                            }


                            #---------------------------------------------------
                            # B) Try to harmonize raw feature names using fuzzy string matching and dictionary data
                            #---------------------------------------------------

                            # Get expected feature names from module-specific meta data
                            EligibleFeatureNames <- Meta.Features.Module %>%
                                                        filter(TableName.Curated == tablename) %>%
                                                        select(FeatureName.Raw,
                                                               FeatureName.Curated) %>%
                                                        unlist(use.names = FALSE)

                            # Get Dictionary (character vector) from passed list
                            FeatureNames.Dictionary <- FeatureNames.Dictionary.S[[tablename]]

                            # Initiate 'HarmonizedFeatureNames'
                            HarmonizedFeatureNames <- names(Table)

                            # Optionally try Fuzzy String Matching first (also matching to Dictionary look up values if possible, because these will subsequentially turn into eligible feature names)
                            if (FeatureNames.FuzzyStringMatching.Run.S == TRUE)
                            {
                                HarmonizedFeatureNames <- GetFuzzyStringMatches(Vector = names(Table),
                                                                                EligibleStrings = c(EligibleFeatureNames, names(FeatureNames.Dictionary)),
                                                                                PreferredMethod = FeatureNames.FuzzyStringMatching.PreferredMethod.S,
                                                                                Tolerance = FeatureNames.FuzzyStringMatching.Tolerance.S)
                            }

                            # Try Dictionary Look-up, if dictionary data is passed
                            if (length(FeatureNames.Dictionary) > 0)
                            {
                                HarmonizedFeatureNames <- if_else(is.na(FeatureNames.Dictionary[HarmonizedFeatureNames]),
                                                                  HarmonizedFeatureNames,
                                                                  FeatureNames.Dictionary[HarmonizedFeatureNames])
                            }


                            #---------------------------------------------------
                            # C) Optionally curate feature names according to Module meta data (FeatureName.Raw -> FeatureName.Curated)
                            #---------------------------------------------------

                            # Initiate 'CuratedFeatureNames'
                            CuratedFeatureNames <- NA

                            if (CurateFeatureNames.S == TRUE)
                            {
                                # Create dictionary (named character vector) from Module meta data
                                CurationDictionary <- Meta.Features.Module %>%
                                                          filter(TableName.Curated == tablename) %>%
                                                          select(FeatureName.Raw,
                                                                 FeatureName.Curated) %>%
                                                          tibble::deframe()

                                if (length(CurationDictionary) > 0)
                                {
                                    # Create recoded versions of previously harmonized 'raw' feature names
                                    CuratedFeatureNames <- if_else(is.na(CurationDictionary[HarmonizedFeatureNames]),
                                                                   HarmonizedFeatureNames,
                                                                   CurationDictionary[HarmonizedFeatureNames])

                                    # Also recode feature names stated in element names of list argument 'Conversion.DateIntoPOSIXct.S'
                                    if (length(Conversion.DateIntoPOSIXct.S) > 0)
                                    {
                                        names(Conversion.DateIntoPOSIXct.S) <- if_else(is.na(CurationDictionary[names(Conversion.DateIntoPOSIXct.S)]),
                                                                                       names(Conversion.DateIntoPOSIXct.S),
                                                                                       CurationDictionary[names(Conversion.DateIntoPOSIXct.S)])
                                    }
                                }
                            }


                            #---------------------------------------------------
                            # Create tibble containing transformation tracks of all feature names
                            #---------------------------------------------------

                            FeatureNames <- tibble(Original = names(Table),
                                                   IsEligible.Original = (Original %in% EligibleFeatureNames),
                                                   Harmonized = HarmonizedFeatureNames,
                                                   Curated = CuratedFeatureNames,
                                                   ChosenRawName = case_when(IsEligible.Original == FALSE ~ Harmonized,
                                                                             .default = Original),
                                                   IsEligible.ChosenRawName = (ChosenRawName %in% EligibleFeatureNames),
                                                   HasChangedRawName = !(Original == ChosenRawName))

                            # Obtain changed feature names for messaging
                            ChangedRawNames <- FeatureNames %>%
                                                    filter(HasChangedRawName == TRUE)

                            if (length(ChangedRawNames) == 0 || nrow(ChangedRawNames) == 0)
                            {
                                Message <- paste0("Table '", tablename, "': No changes to raw feature names.")
                                cli::cat_bullet(Message, bullet = "info")
                                CurrentMessages <- c(CurrentMessages,
                                                     Info = Message)

                            } else {

                                for (i in 1:nrow(ChangedRawNames))
                                {
                                    Message <- paste0("Table '", tablename, "': Changed feature name '", ChangedRawNames$Original[i], "' to '", ChangedRawNames$ChosenRawName[i], "'.")
                                    cli::cat_bullet(Message, bullet = "info")
                                    CurrentMessages <- c(CurrentMessages,
                                                         Info = Message)
                                }
                            }

                            # Obtain remaining ineligible feature names
                            RemainingIneligibleNames <- FeatureNames %>%
                                                            filter(IsEligible.ChosenRawName == FALSE)

                            if (length(RemainingIneligibleNames) > 0 && nrow(RemainingIneligibleNames) > 0)
                            {
                                for (i in 1:nrow(RemainingIneligibleNames))
                                {
                                    Message <- paste0("Table '", tablename, "': The feature name '", RemainingIneligibleNames$Original[i], "' is ineligible and could not be harmonized!")
                                    cli::cat_bullet(Message, bullet = "warning", bullet_col = "red")
                                    CurrentMessages <- c(CurrentMessages,
                                                         Warning = Message)
                                }
                            }


                            #---------------------------------------------------
                            # Commit feature renaming
                            #---------------------------------------------------

                            if (CurateFeatureNames.S == TRUE)
                            {
                                names(Table) <- FeatureNames$Curated

                            } else {

                                names(Table) <- FeatureNames$ChosenRawName
                            }


                            #---------------------------------------------------
                            # D) Optionally convert features to character type
                            #---------------------------------------------------

                            if (Conversion.IntoCharacter.S == "All")
                            {
                                Table <- Table %>%
                                            mutate(across(everything(),
                                                          ~ as.character(.x)))
                                                          #~ vctrs::vec_cast(.x, character())))      # Using vctrs::vec_cast() to avoid implicit conversions performed with base::as.character()

                                Message <- paste0("Table '", tablename, "': Converted ALL features into character type.")
                                cli::cat_bullet(Message, bullet = "info")
                                CurrentMessages <- c(CurrentMessages, Info = Message)

                            } else if (Conversion.IntoCharacter.S == "Date") {

                                CountDateFeatures <- Table %>%
                                                        select(where(~ inherits(.x, c("Date", "POSIXct")))) %>%
                                                        ncol()

                                if (CountDateFeatures == 0)
                                {
                                    Message <- paste0("Table '", tablename, "' had no features of class 'Date' or 'POSIXct'.")

                                } else {

                                    Table <- Table %>%
                                                mutate(across(where(~ inherits(.x, c("Date", "POSIXct"))),
                                                              ~ as.character(.x)))

                                    Message <- paste0("Table '", tablename, "': Converted ", CountDateFeatures, " DATE features into character type.")
                                }

                                cli::cat_bullet(Message, bullet = "info")
                                CurrentMessages <- c(CurrentMessages, Info = Message)
                            }


                            #---------------------------------------------------
                            # E) Optionally convert date features (or character features containing date data) into POSIXct
                            #---------------------------------------------------

                            # Get date feature names from module meta data
                            DateFeatureNames <- Meta.Features.Module %>%
                                                    filter(TableName.Curated == tablename,
                                                           Type == "date") %>%
                                                    { if (CurateFeatureNames.S == TRUE) { pull(., FeatureName.Curated) }
                                                      else { pull(., FeatureName.Raw) } }

                            # Only keep data feature names that occur in current table
                            DateFeatureNames <- DateFeatureNames[DateFeatureNames %in% names(Table)]

                            if (length(DateFeatureNames) > 0 && length(Conversion.DateIntoPOSIXct.S) > 0)
                            {
                                # First, check if there is a list element named '.All' and proceed accordingly
                                if (".All" %in% names(Conversion.DateIntoPOSIXct.S))
                                {
                                    Table <- Table %>%
                                                mutate(across(all_of(DateFeatureNames),
                                                              ~ as.POSIXct(.x,
                                                                           tryFormats = Conversion.DateIntoPOSIXct.S[[".All"]],
                                                                           tz = "UTC")))

                                    Message <- paste0("Table '", tablename, "': Converted date features ", paste0("'", DateFeatureNames, "'", collapse = " / "), " into POSIXct type after trying the date format(s) ", paste0("'", Conversion.DateIntoPOSIXct.S[[".All"]], "'", collapse = " / "), ".")
                                    cli::cat_bullet(Message, bullet = "info")
                                    CurrentMessages <- c(CurrentMessages, Info = Message)
                                }

                                # Proceed with other date feature names if there are any other relevant ones
                                for (datefeaturename in names(Conversion.DateIntoPOSIXct.S))
                                {
                                    if (datefeaturename %in% names(Table))
                                    {
                                        Table <- Table %>%
                                                    mutate(across(all_of(datefeaturename),
                                                                  ~ as.POSIXct(.x,
                                                                               tryFormats = Conversion.DateIntoPOSIXct.S[[datefeaturename]],
                                                                               tz = "UTC")))

                                        Message <- paste0("Table '", tablename, "': Converted date feature '", datefeaturename, "' into POSIXct type after trying the date format(s) ", paste0("'", Conversion.DateIntoPOSIXct.S[[datefeaturename]], "'", collapse = " / "), ".")
                                        cli::cat_bullet(Message, bullet = "info")
                                        CurrentMessages <- c(CurrentMessages, Info = Message)
                                    }
                                }
                            }


                            #---------------------------------------------------
                            # Return processed Table and Messages
                            #---------------------------------------------------
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


# Create and display message about curational recoding of feature names
#-------------------------------------------------------------------------------
  if (CurateFeatureNames.S == TRUE)
  {
      Message <- paste0("After primary feature name harmonization: Recoded feature names to their 'curated' version according to meta data!")
      cli::cat_bullet(Message, bullet = "tick", bullet_col = "green")
      Messages <- c(Messages,
                    Success = Message)
  }

#-------------------------------------------------------------------------------
  return(list(RawDataSet = RawDataSet,
              OriginalRawDataSet = OriginalRawDataSet,
              Messages = Messages))
}
