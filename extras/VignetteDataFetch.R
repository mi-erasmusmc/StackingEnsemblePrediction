# @file VignetteDataFetch.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This code should be used to fetch the data that is used in the vignettes.  
library(SqlRender)
library(DatabaseConnector)
library(PatientLevelPrediction)
setwd("s:/temp")
options(fftempdir = "s:/FFtemp")

pw <- NULL
dbms <- "sql server"
user <- NULL
server <- "RNDUSRDHIT07.jnj.com"
cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- NULL

dbms <- "postgresql"
server <- "localhost/ohdsi"
user <- "postgres"
pw <- "F1r3starter"
cdmDatabaseSchema <- "cdm4_sim"
resultsDatabaseSchema <- "scratch"
port <- NULL

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd_v5.dbo"
resultsDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
port <- 17001
cdmVersion <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)
sql <- SqlRender::loadRenderTranslateSql("HospitalizationCohorts.sql",
                                         packageName = "PatientLevelPrediction",
                                         dbms = dbms,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         resultsDatabaseSchema = resultsDatabaseSchema,
                                         post_time = 30,
                                         pre_time = 365)
connection <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.rehospitalization GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)
DatabaseConnector::disconnect(connection)

covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                             useCovariateDemographicsGender = TRUE,
                                             useCovariateDemographicsRace = TRUE,
                                             useCovariateDemographicsEthnicity = TRUE,
                                             useCovariateDemographicsAge = TRUE,
                                             useCovariateDemographicsYear = TRUE,
                                             useCovariateDemographicsMonth = TRUE,
                                             useCovariateConditionOccurrence = TRUE,
                                             useCovariateConditionOccurrence365d = TRUE,
                                             useCovariateConditionOccurrence30d = TRUE,
                                             useCovariateConditionOccurrenceInpt180d = TRUE,
                                             useCovariateConditionEra = TRUE,
                                             useCovariateConditionEraEver = TRUE,
                                             useCovariateConditionEraOverlap = TRUE,
                                             useCovariateConditionGroup = TRUE,
                                             useCovariateConditionGroupMeddra = TRUE,
                                             useCovariateConditionGroupSnomed = TRUE,
                                             useCovariateDrugExposure = TRUE,
                                             useCovariateDrugExposure365d = TRUE,
                                             useCovariateDrugExposure30d = TRUE,
                                             useCovariateDrugEra = TRUE,
                                             useCovariateDrugEra365d = TRUE,
                                             useCovariateDrugEra30d = TRUE,
                                             useCovariateDrugEraOverlap = TRUE,
                                             useCovariateDrugEraEver = TRUE,
                                             useCovariateDrugGroup = TRUE,
                                             useCovariateProcedureOccurrence = TRUE,
                                             useCovariateProcedureOccurrence365d = TRUE,
                                             useCovariateProcedureOccurrence30d = TRUE,
                                             useCovariateProcedureGroup = TRUE,
                                             useCovariateObservation = TRUE,
                                             useCovariateObservation365d = TRUE,
                                             useCovariateObservation30d = TRUE,
                                             useCovariateObservationCount365d = TRUE,
                                             useCovariateMeasurement = TRUE,
                                             useCovariateMeasurement365d = TRUE,
                                             useCovariateMeasurement30d = TRUE,
                                             useCovariateMeasurementCount365d = TRUE,
                                             useCovariateMeasurementBelow = TRUE,
                                             useCovariateMeasurementAbove = TRUE,
                                             useCovariateConceptCounts = TRUE,
                                             useCovariateRiskScores = TRUE,
                                             useCovariateRiskScoresCharlson = TRUE,
                                             useCovariateRiskScoresDCSI = TRUE,
                                             useCovariateRiskScoresCHADS2 = TRUE,
                                             useCovariateRiskScoresCHADS2VASc = TRUE,
                                             useCovariateInteractionYear = FALSE,
                                             useCovariateInteractionMonth = FALSE,
                                             excludedCovariateConceptIds = c(),
                                             includedCovariateConceptIds = c(),
                                             deleteCovariatesSmallCount = 100)

plpData <- getDbPlpData(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        oracleTempSchema = oracleTempSchema,
                        cohortDatabaseSchema = resultsDatabaseSchema,
                        cohortTable = "rehospitalization",
                        cohortIds = 1,
                        washoutWindow = 183,
                        useCohortEndDate = TRUE,
                        windowPersistence = 0,
                        covariateSettings = covariateSettings,
                        outcomeDatabaseSchema = resultsDatabaseSchema,
                        outcomeTable = "rehospitalization",
                        outcomeIds = 2,
                        firstOutcomeOnly = FALSE,
                        cdmVersion = cdmVersion)

savePlpData(plpData, "s:/temp/PlpVignette/plpData")

# plpData <- loadPlpData('s:/temp/PlpVignette/plpData')

means <- computeCovariateMeans(plpData = plpData, outcomeId = 2)

saveRDS(means, "s:/temp/PlpVignette/means.rds")

# plotCovariateDifferenceOfTopVariables(means)

parts <- splitData(plpData, c(0.75, 0.25))

savePlpData(parts[[1]], "s:/temp/PlpVignette/plpData_train")

savePlpData(parts[[2]], "s:/temp/PlpVignette/plpData_test")

# parts <- list(); parts[[1]] <- loadPlpData('s:/temp/PlpVignette/plpData_train'); parts[[2]] <-
# loadPlpData('s:/temp/PlpVignette/plpData_test')

model <- fitPredictiveModel(parts[[1]],
                            modelType = "logistic",
                            prior = createPrior("laplace",
                                                exclude = c(0),
                                                useCrossValidation = TRUE),
                            control = createControl(noiseLevel = "quiet",
                                                    cvType = "auto",
                                                    startingVariance = 0.001,
                                                    tolerance = 1e-07,
                                                    cvRepetitions = 10,
                                                    seed = 123,
                                                    threads = 30))

saveRDS(model, file = "s:/temp/PlpVignette/model.rds")

# model <- readRDS('s:/temp/PlpVignette/model.rds')



prediction <- predictProbabilities(model, parts[[2]])

saveRDS(prediction, file = "s:/temp/PlpVignette/prediction.rds")

# prediction <- readRDS('s:/temp/PlpVignette/prediction.rds')

computeAuc(prediction, parts[[2]])
plotRoc(prediction, parts[[2]])
plotCalibration(prediction, parts[[2]], numberOfStrata = 10)

modelDetails <- getModelDetails(model, parts[[2]])
head(modelDetails)



#### Datafetch for custom covariate builders #####

createLooCovariateSettings <- function(useLengthOfObs = TRUE) {
  covariateSettings <- list(useLengthOfObs = useLengthOfObs)
  attr(covariateSettings, "fun") <- "getDbLooCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}


getDbLooCovariateData <- function(connection,
                                  oracleTempSchema = NULL,
                                  cdmDatabaseSchema,
                                  cdmVersion = "4",
                                  cohortTempTable = "cohort_person",
                                  rowIdField = "subject_id",
                                  covariateSettings) {
  writeLines("Constructing length of observation covariates")
  if (covariateSettings$useLengthOfObs == FALSE) {
    return(NULL)
  }

  # Temp table names must start with a '#' in SQL Server, our source dialect:
  if (substr(cohortTempTable, 1, 1) != "#") {
    cohortTempTable <- paste("#", cohortTempTable, sep = "")
  }

  # Some SQL to construct the covariate:
  sql <- paste("SELECT @row_id_field AS row_id, 1 AS covariate_id,",
               "DATEDIFF(DAY, cohort_start_date, observation_period_start_date)",
               "AS covariate_value",
               "FROM @cohort_temp_table c",
               "INNER JOIN @cdm_database_schema.observation_period op",
               "ON op.person_id = c.subject_id",
               "WHERE cohort_start_date >= observation_period_start_date",
               "AND cohort_start_date <= observation_period_end_date")
  sql <- SqlRender::renderSql(sql,
                              cohort_temp_table = cohortTempTable,
                              row_id_field = rowIdField,
                              cdm_database_schema = cdmDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql

  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql.ffdf(connection, sql)

  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))

  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = 1,
                             covariateName = "Length of observation",
                             analysisId = 1,
                             conceptId = 0)
  covariateRef <- ff::as.ffdf(covariateRef)

  metaData <- list(sql = sql, call = match.call())
  result <- list(covariates = covariates, covariateRef = covariateRef, metaData = metaData)
  class(result) <- "covariateData"
  return(result)
}

looCovariateSettings <- createLooCovariateSettings(useLengthOfObs = TRUE)

plpData <- getDbPlpData(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        cohortDatabaseSchema = resultsDatabaseSchema,
                        cohortTable = "mschuemi_stroke",
                        cohortIds = 1,
                        useCohortEndDate = TRUE,
                        windowPersistence = 0,
                        covariateSettings = looCovariateSettings,
                        outcomeDatabaseSchema = resultsDatabaseSchema,
                        outcomeTable = "mschuemi_stroke",
                        outcomeIds = 2,
                        firstOutcomeOnly = TRUE,
                        cdmVersion = cdmVersion)

covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                             useCovariateDemographicsGender = TRUE,
                                             useCovariateDemographicsRace = TRUE,
                                             useCovariateDemographicsEthnicity = TRUE,
                                             useCovariateDemographicsAge = TRUE,
                                             useCovariateDemographicsYear = TRUE,
                                             useCovariateDemographicsMonth = TRUE)
looCovariateSettings <- createLooCovariateSettings(useLengthOfObs = TRUE)
covariateSettingsList <- list(covariateSettings, looCovariateSettings)

plpData <- getDbPlpData(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        cohortDatabaseSchema = resultsDatabaseSchema,
                        cohortTable = "mschuemi_stroke",
                        cohortIds = 1,
                        useCohortEndDate = TRUE,
                        windowPersistence = 0,
                        covariateSettings = covariateSettingsList,
                        outcomeDatabaseSchema = resultsDatabaseSchema,
                        outcomeTable = "mschuemi_stroke",
                        outcomeIds = 2,
                        firstOutcomeOnly = TRUE,
                        cdmVersion = cdmVersion)

covariateSettings <- createHdpsCovariateSettings(useCovariateCohortIdIs1 = FALSE,
                                                 useCovariateDemographics = TRUE,
                                                 useCovariateDemographicsGender = TRUE,
                                                 useCovariateDemographicsRace = TRUE,
                                                 useCovariateDemographicsEthnicity = TRUE,
                                                 useCovariateDemographicsAge = TRUE,
                                                 useCovariateDemographicsYear = TRUE,
                                                 useCovariateDemographicsMonth = TRUE,
                                                 useCovariateConditionOccurrence = TRUE,
                                                 useCovariate3DigitIcd9Inpatient180d = TRUE,
                                                 useCovariate3DigitIcd9Inpatient180dMedF = TRUE,
                                                 useCovariate3DigitIcd9Inpatient180d75F = TRUE,
                                                 useCovariate3DigitIcd9Ambulatory180d = TRUE,
                                                 useCovariate3DigitIcd9Ambulatory180dMedF = TRUE,
                                                 useCovariate3DigitIcd9Ambulatory180d75F = TRUE,
                                                 useCovariateDrugExposure = TRUE,
                                                 useCovariateIngredientExposure180d = TRUE,
                                                 useCovariateIngredientExposure180dMedF = TRUE,
                                                 useCovariateIngredientExposure180d75F = TRUE,
                                                 useCovariateProcedureOccurrence = TRUE,
                                                 useCovariateProcedureOccurrenceInpatient180d = TRUE,
                                                 useCovariateProcedureOccurrenceInpatient180dMedF = TRUE,
                                                 useCovariateProcedureOccurrenceInpatient180d75F = TRUE,
                                                 useCovariateProcedureOccurrenceAmbulatory180d = TRUE,
                                                 useCovariateProcedureOccurrenceAmbulatory180dMedF = TRUE,
                                                 useCovariateProcedureOccurrenceAmbulatory180d75F = TRUE,
                                                 excludedCovariateConceptIds = c(),
                                                 includedCovariateConceptIds = c(),
                                                 deleteCovariatesSmallCount = 100)



#### Datafetch for cohort attribute covariate builder #####

library(SqlRender)
library(DatabaseConnector)
library(PatientLevelPrediction)
setwd("s:/temp")
options(fftempdir = "s:/FFtemp")

pw <- NULL
dbms <- "sql server"
user <- NULL
server <- "RNDUSRDHIT07.jnj.com"
cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- NULL

dbms <- "postgresql"
server <- "localhost/ohdsi"
user <- "postgres"
pw <- "F1r3starter"
cdmDatabaseSchema <- "cdm4_sim"
resultsDatabaseSchema <- "scratch"
port <- NULL

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd_v5.dbo"
cohortDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
port <- 17001
cdmVersion <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)
connection <- DatabaseConnector::connect(connectionDetails)

# Build cohorts:
sql <- SqlRender::loadRenderTranslateSql("HospitalizationCohorts.sql",
                                         packageName = "PatientLevelPrediction",
                                         dbms = dbms,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         resultsDatabaseSchema = cohortDatabaseSchema,
                                         post_time = 30,
                                         pre_time = 365)
DatabaseConnector::executeSql(connection, sql)

# Build cohort attributes:
sql <- SqlRender::loadRenderTranslateSql("LengthOfObsCohortAttr.sql",
                                         packageName = "PatientLevelPrediction",
                                         dbms = dbms,
                                         cdm_database_schema = cdmDatabaseSchema,
                                         cohort_database_schema = cohortDatabaseSchema,
                                         cohort_table = "rehospitalization",
                                         cohort_attribute_table = "loo_cohort_attribute",
                                         attribute_definition_table = "loo_attribute_definition",
                                         cohort_definition_ids = c(1, 2))
DatabaseConnector::executeSql(connection, sql)


querySql(connection, "SELECT TOP 100 * FROM scratch.dbo.loo_cohort_attribute")

looCovariateSettings <- createCohortAttrCovariateSettings(attrDatabaseSchema = cohortDatabaseSchema,
                                                          cohortAttrTable = "loo_cohort_attribute",
                                                          attrDefinitionTable = "loo_attribute_definition",
                                                          includeAttrIds = c())

plpData <- getDbPlpData(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        cohortTable = "rehospitalization",
                        cohortIds = 1,
                        useCohortEndDate = TRUE,
                        windowPersistence = 0,
                        covariateSettings = looCovariateSettings,
                        outcomeDatabaseSchema = cohortDatabaseSchema,
                        outcomeTable = "rehospitalization",
                        outcomeIds = 2,
                        firstOutcomeOnly = TRUE,
                        cdmVersion = cdmVersion)
summary(plpData)
plpData$covariates


sql <- "DROP TABLE @cohort_database_schema.rehospitalization"
sql <- SqlRender::renderSql(sql, cohort_database_schema = cohortDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql







looCovariateSettings <- createCohortAttrCovariateSettings(attrDatabaseSchema = cohortDatabaseSchema,
                                                          cohortAttrTable = "loo_cohort_attribute",
                                                          attrDefinitionTable = "loo_attribute_definition",
                                                          includeAttrIds = c())
covariateSettingsList <- list(looCovariateSettings, looCovariateSettings)

plpData <- getDbPlpData(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        cohortTable = "rehospitalization",
                        cohortIds = 1,
                        useCohortEndDate = TRUE,
                        windowPersistence = 0,
                        covariateSettings = covariateSettingsList,
                        outcomeDatabaseSchema = cohortDatabaseSchema,
                        outcomeTable = "rehospitalization",
                        outcomeIds = 2,
                        firstOutcomeOnly = TRUE,
                        cdmVersion = cdmVersion)
