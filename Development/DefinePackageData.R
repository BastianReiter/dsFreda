

library(usethis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Disclosure Settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DisclosureSettings <- list(Profile = "loose",     # Optional: 'strict', 'loose'
                           NThreshold = 5)

# Save data in .rda-file and make it part of package
use_data(DisclosureSettings, overwrite = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module Registration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ModuleRegistration <- list(CCP = "dsCCPhos",
                           P21 = "dsFredaP21")

# Save data in .rda-file and make it part of package
use_data(ModuleRegistration, overwrite = TRUE)

