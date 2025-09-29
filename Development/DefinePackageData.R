
library(readxl)
library(usethis)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in Meta Data and Settings from xlsx-file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sheetnames <- c("Meta.Tables",
                "Meta.Features",
                "Meta.Values",
                "Proc.EventFeatures",
                "Proc.TableNormalization",
                "Set.FeatureObligations",
                "Set.FeatureTracking",
                "Set.DataHarmonizationMethods",
                "Set.TransformativeExpressions",
                "Set.Dictionary",
                "Set.FuzzyStringMatching")

for (sheetname in Sheetnames)
{
    assign(x = sheetname,
           value = read_excel(path = "./Development/MetaDataTemplate.xlsx",
                              sheet = sheetname,
                              skip = 1))

    # Save data in .rda-file and make it part of the package
    do.call(use_data, list(as.name(sheetname), overwrite = TRUE))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Disclosure Settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DisclosureSettings <- list(Profile = "loose",     # Optional: 'strict', 'loose'
                           NThreshold = 5)

# Save data in .rda-file and make it part of package
use_data(DisclosureSettings, overwrite = TRUE)
