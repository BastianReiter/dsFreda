

library(dplyr)
library(purrr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load CCP test data as raw data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RawDataSet <- readRDS("../Data/CCP/CCPTestData2024.rds")

# Rename tables of RawDataSet (the names are also changed when tables are being loaded into R server sessions)
vc_Lookup <- dsCCPhos::Meta.Tables$TableName.Curated
names(vc_Lookup) <- dsCCPhos::Meta.Tables$TableName.Raw
names(RawDataSet) <- sapply(names(RawDataSet),
                            function(TableName) { vc_Lookup[TableName] })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform preparatory operations prior to curation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSPreparation <- PrepareRawDataDS(RawDataSetName.S = "RawDataSet",
                                   Module.S = "CCP",
                                   Conversion.IntoCharacter.S = "Date",
                                   #Conversion.DateIntoPOSIXct.S = .encode_tidy_eval("list('.All' = c('%Y%m%d%H%M', '%Y%m%d', '%Y-%m-%d'))", .get_encode_dictionary()),
                                   CurateFeatureNames.S = TRUE)

RawDataSet <- RDSPreparation$RawDataSet


# Using test data from servers
# RawDataSet <- readRDS("../dsFreda/Development/Test/FailingTestData.rds") %>% pluck("ServerA")
# TableNameLookup <- dsCCPhos::Meta.Tables %>% select(TableName.Curated, TableName.Raw) %>% tibble::deframe()
# names(TestDataReal) <- TableNameLookup[names(TestDataReal)]

# Test <- RawDataSet$Patient %>%
#             group_by(across(-CausesOfDeath)) %>%
#             summarize(CausesOfDeath = paste0(CausesOfDeath, collapse = " / "))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "RawDataSet",
                                   Module.S = "CCP",
                                   Stage.S = "Raw")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curate data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CurationOutput <- dsFreda::CurateDataDS(RawDataSetName.S = "RawDataSet",
                                        Module.S = "CCP")

CuratedDataSet <- CurationOutput$DataSet


FredaGUI::Widget.CurationReport(CurationReport = CurationOutput$Report)


Rem <- CurationOutput$Report$DataHarmonization$DataRemediation

CDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "CuratedDataSet",
                                   Module.S = "CCP",
                                   Stage.S = "Curated")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Augment data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AugmentationOutput <- dsCCPhos::AugmentDataDS(CuratedDataSetName.S = "CuratedDataSet")

ADS <- AugmentationOutput$AugmentedDataSet

ADSTableCheck <- GetDataSetCheckDS(DataSetName.S = "ADS")


CCP.ADS.Diagnosis <- ADS$Diagnosis


ADS.Patient <- ADS$Patient %>%
                    filter(CountDiagnoses == 1)


Analysis <- JoinTablesDS(TableNameA.S = "ADS$Patient",
                         TableNameB.S = "ADS$Diagnosis",
                         ByStatement.S = "PatientID")

Test <- GetSampleStatisticsDS(TableName.S = "Analysis",
                              FeatureName.S = "PatientAgeAtDiagnosis",
                              GroupingFeatureName.S = "UICCStageCategory",
                              ReturnECDF.S = TRUE)

Test <- GetFrequencyTableDS(TableName.S = "ADS$Diagnosis",
                            FeatureName.S = "DiagnosisID")

#
#
# SampleStatistics <- GetSampleStatisticsDS(TableName.S = "ADS$Patients",
#                                           FeatureName.S = "LastVitalStatus")
# SampleStatistics$Statistics



GetFrequencyTableDS(TableName.S = "CCP.ADS.Diagnosis",
                    FeatureName.S = "ICD10Code")




install.packages("pkgnet")
library(pkgnet)

report <- CreatePackageReport("dsFreda")


