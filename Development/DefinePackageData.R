

library(dplyr)
library(usethis)


#===============================================================================
# Freda Alpha Palettes
#===============================================================================

FredaAlphaPalettes <- list(Levels_2 = c(0.5, 0.9),
                           Levels_3 = c(0.2, 0.5, 0.9),
                           Levels_4 = c(0.3, 0.5, 0.7, 0.9),
                           Levels_5 = c(0.4, 0.5, 0.6, 0.7, 0.8))

# Save data in .rda-file and make it part of package
use_data(FredaAlphaPalettes, overwrite = TRUE)


#===============================================================================
# Freda Colors
#===============================================================================

FredaColors <- list(LightGrey = "#EDEDED",
                    MediumGrey = "#D0D0D0",
                    DarkGrey = "#595959",
                    #---------
                    Primary = "#054996",
                    PrimaryLight = "#05499650",
                    Secondary = "#8e1e39",
                    SecondaryLight = "#8e1e3950",
                    Tertiary = "#2B8C88",
                    TertiaryLight = "#2B8C8850",
                    #---------
                    Accent = "#960551",
                    AccentLight = "#96055150",
                    #---------
                    BlueNice = "#7EA6E0",
                    Green = "#269D27",
                    Orange = "#DE8F02",
                    Red = "#A90939")

# Save data in .rda-file and make it part of package
use_data(FredaColors, overwrite = TRUE)


#===============================================================================
# Privacy Settings
#===============================================================================

MakePrivacyProfile <- function(NThreshold = 5,
                               FilterTable.MinN = 5,
                               GetTestData.Enable = FALSE,
                               GetTestData.Mode = NA,
                               GetTestData.MaxSampleSize = NA,
                               GetReportingObject.AllowedObjectNames = NULL)
{
    list(NThreshold = NThreshold,
         FilterTable.MinN = FilterTable.MinN,
         GetTestData.Enable = GetTestData.Enable,
         GetTestData.Mode = GetTestData.Mode,
         GetTestData.MaxSampleSize = GetTestData.MaxSampleSize,
         GetReportingObject.AllowedObjectNames = GetReportingObject.AllowedObjectNames)
}

Set.PrivacyProfiles <- list("Development" = MakePrivacyProfile(NThreshold = NA,
                                                               FilterTable.MinN = NA,
                                                               GetTestData.Enable = TRUE,
                                                               GetTestData.Mode = "ColumnsConnected",
                                                               GetTestData.MaxSampleSize = NA,
                                                               GetReportingObject.AllowedObjectNames = NULL),
                            "Loose" = MakePrivacyProfile(NThreshold = 10,
                                                         FilterTable.MinN = 5,
                                                         GetTestData.Enable = TRUE,
                                                         GetTestData.Mode = "ColumnsDisconnected",
                                                         GetTestData.MaxSampleSize = 10,
                                                         GetReportingObject.AllowedObjectNames = NULL),
                            "Strict" = MakePrivacyProfile(NThreshold = 5,
                                                          FilterTable.MinN = 5,
                                                          GetTestData.Enable = FALSE,
                                                          GetTestData.Mode = NA,
                                                          GetTestData.MaxSampleSize = NA,
                                                          GetReportingObject.AllowedObjectNames = NULL))

# Save data in .rda-file and make it part of package
use_data(Set.PrivacyProfiles, overwrite = TRUE)

Set.PrivacyProfile.Chosen <- "Loose"
use_data(Set.PrivacyProfile.Chosen, overwrite = TRUE)


#===============================================================================
# Module Register
#===============================================================================

Meta.Modules <- list(CCP = "dsCCPhos",
                     P21 = "dsFredaP21")

# Save data in .rda-file and make it part of package
use_data(Meta.Modules, overwrite = TRUE)

