

library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load CCP test data as raw data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RawDataSet <- readRDS(file = "./Development/Test/CCPTestData.rds")

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
                                   CurateFeatureNames.S = TRUE)

RawDataSet <- RDSPreparation$RawDataSet


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Tables for existence and completeness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RDSTableCheck <- GetDataSetCheckDS(DataSetName.S = "RawDataSet",
                                   Module.S = "CCP",
                                   Stage.S = "Raw")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curate data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CurationOutput <- dsCCPhos::CurateDataDS(RawDataSetName.S = "RawDataSet",
                                         Settings.S = list(DataHarmonization = list(Run = TRUE,
                                                                                    Profile = "Default"),
                                                           FeatureObligations = list(Profile = "Default"),
                                                           FeatureTracking = list(Profile = "Default"),
                                                           TableCleaning = list(Run = TRUE)))

CuratedDataSet <- CurationOutput$CuratedDataSet

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






