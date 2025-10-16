

library(usethis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Privacy Settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Set.Privacy <- list(Profile = "loose",     # Optional: 'strict', 'loose'
                    NThreshold = 5)

# Save data in .rda-file and make it part of package
use_data(Set.Privacy, overwrite = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Module Register
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Meta.Modules <- list(CCP = "dsCCPhos",
                     P21 = "dsFredaP21")

# Save data in .rda-file and make it part of package
use_data(Meta.Modules, overwrite = TRUE)

