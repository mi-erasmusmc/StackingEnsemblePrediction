########## base learners #################

library(PatientLevelPrediction) 
options(andromedaTempFolder = "location with space to save big data")

outcomeIds = c(5412,5414,5415,5419:5426,5428,5431:5435)
database_name = "MDCD"

loadDirectory = paste0('PlpData_', database_name)
plpData <- loadPlpData(loadDirectory)

#******** EXAMPLE 1 *********
# create the split setting by specifying how you want to
# partition the data into development (train/validation) and evaluation (test or CV)
splitSettings <- createDefaultSplitSetting(testFraction = 0.25,
                                           trainFraction = 0.75,
                                           splitSeed = 42,
                                           nfold=3,
                                           type = 'stratified')

# create the settings specifying any under/over sampling 
# in this example we do not do any
sampleSettings <- createSampleSettings(type = 'none')

# specify any feature engineering that will be applied to the train data
# in this example we do not do any
featureEngineeringSettings <- createFeatureEngineeringSettings(type = 'none')

# specify whether to use normalization and removal of rare features
# preprocessSettings <- ...
preprocessSettings = createPreprocessSettings(
  minFraction = 0.001,
  normalize = T
)

# create population settings (this defines the labels in the data)
#create study population to develop model on
#require minimum of 365 days observation prior to at risk start
#no prior outcome and person must be observed for 365 after index (minTimeAtRisk)
#with risk window from 0 to 365 days after index
populationSettings <- createStudyPopulationSettings(firstExposureOnly = TRUE,
                                                    washoutPeriod = 365,
                                                    removeSubjectsWithPriorOutcome = TRUE,
                                                    priorOutcomeLookback = 99999,
                                                    requireTimeAtRisk = FALSE,
                                                    minTimeAtRisk=364,
                                                    riskWindowStart = 1,
                                                    binary = T,
                                                    includeAllOutcomes = T,
                                                    startAnchor = 'cohort start',
                                                    endAnchor = "cohort start",
                                                    restrictTarToCohortEnd = F,
                                                    # addExposureDaysToStart = FALSE,
                                                    riskWindowEnd = 365)
# addExposureDaysToEnd = FALSE)

# specify how you want the logging for the analysis
# generally this is saved in a file with the results
# but you can define the level of logging
logSettings <- createLogSettings(verbosity = 'DEBUG',
                                 timeStamp = T,
                                 logName = 'runPlp LR Log')

# specify what parts of the analysis to run:
# in this example we run everything
executeSettings <- createExecuteSettings(runSplitData = T,
                                         runSampleData = F,
                                         runfeatureEngineering = F,
                                         runPreprocessData = T,
                                         runModelDevelopment = T,
                                         runCovariateSummary = F)

# create the settings specifying any under/over sampling
# in this example we do not do any
# sampleSettings <- createSampleSettings(type = 'none')

#******** ********** *********
# hyperparameter tuning settings matched to current default values in PLP

method = "lassoCyclops"
modelSettings <- setLassoLogisticRegression(seed = 42)

# method = "decisiontree"
# modelSettings <- setDecisionTree(seed = as.integer(42),
#                                  classWeight = list(NULL),
#                                  maxFeatures = list(100,'sqrt', NULL))

# method = "randomforest"
# modelSettings <- setRandomForest(seed = 42,
#                                  ntrees=list(100),
#                                  maxDepth = list(17),
#                                  minSamplesSplit = list(2, 5),
#                                  minSamplesLeaf = list(1, 10),
#                                  mtries = list('sqrt'),
#                                  maxSamples= list(NULL, 0.9),
#                                  classWeight = list(NULL))

# method = "adaboost"
# modelSettings <- setAdaBoost(seed = 42)

# method = 'xgbModel'
# modelSettings <- setGradientBoostingMachine(seed = 42,
#                                             ntrees = c(100,300),
#                                             nthread = 4,
#                                             earlyStopRound = NULL,
#                                             maxDepth = c(4,6,8),
#                                             minRows = 2,
#                                             learnRate = c(0.01,0.05,0.1,0.3))

# method = "mlp"
# modelSettings <- setMLP(seed = 42,
#                         hiddenLayerSizes = list(c(100), c(20)), 
#                         epsilon = list(0.00000001)) 


for (outcomeId in outcomeIds){
  
  try({
    saveDirectory = paste0('DepressionOutcomesResults/', database_name, "/", outcomeId)
    
    analysisId <- method
    
    finalModel <- runPlp(plpData = plpData,
                         outcomeId = as.numeric(outcomeId),
                         analysisId = analysisId,
                         populationSettings = populationSettings,
                         splitSettings = splitSettings,
                         sampleSettings = sampleSettings,
                         featureEngineeringSettings = featureEngineeringSettings,
                         preprocessSettings = preprocessSettings,
                         modelSettings = modelSettings,
                         logSettings = logSettings,
                         executeSettings = executeSettings,
                         saveDirectory = saveDirectory)
    saveRDS(finalModel$model, paste0(saveDirectory,"/", method, "/plpResult/model.rds"))
  })
}


########## stacking ensembles #################

library(stringr)
library(dplyr)

evalType = "CV"
databases <- c("CCAE", "MDCD", "MDCR", "Germany")
classifiers <- c("lassoCyclops", "decisiontree", "randomforest", "xgbModel", "adaboost", "mlp")

# base learner AUCs on CV
for (database in databases){
  for (outcomeId in outcomeIds){
    res <- list()
    for (classifier in classifiers){
      if (dir.exists(file.path("DepressionOutcomesResults", database, outcomeId, classifier))){
        auc <- readRDS(file.path("DepressionOutcomesResults", database, outcomeId, classifier, "plpResult/runPlp.rds"))$performanceEvaluation$evaluationStatistics
        res[[classifier]] <- auc %>% filter(evaluation == evalType & metric == "AUROC") 
      }
    }
    if (length(res)!=length(classifiers)){
      stop("missing classifier")
    }
    aucs <- do.call(cbind, res)
    aucs <- aucs[,grep('value',colnames(aucs))]
    colnames(aucs) <- gsub('\\.value','', colnames(aucs))
    saveRDS(aucs, file.path("StackingEnsembles", database, outcomeId, "aucs.Rds"))
  }
}

# base learner predictions on CV
for (database in databases){
  for (outcomeId in outcomeIds){
    res <- list()
    for (classifier in classifiers){
      if (dir.exists(file.path("DepressionOutcomesResults", database, outcomeId, classifier))){
        pred <- readRDS(file.path("DepressionOutcomesResults", database, outcomeId, classifier, "plpResult/runPlp.rds"))$prediction
        res[[classifier]] <- pred %>% filter(evaluationType==evalType) %>% select(rowId, value, outcomeCount)
      }
    }
    if (length(res)!=length(classifiers)){
      stop("missing classifier")
    }
    cvPrediction <- do.call(cbind, res)
    cvPrediction <- cbind(rowId = cvPrediction[,c(1,3)],cvPrediction[,grep('value',colnames(cvPrediction))])
    colnames(cvPrediction) <- gsub('\\.value','', colnames(cvPrediction))
    colnames(cvPrediction)[1:2] <- c('rowId', 'outcome')
    dir.create(file.path("StackingEnsembles", database, outcomeId), recursive = TRUE)
    saveRDS(cvPrediction, file.path("StackingEnsembles", database, outcomeId, "cvPrediction.Rds"))
  }
}

# base learner predictions on test set (internal validation)
for (database in databases){
  for (outcomeId in outcomeIds){
    res <- list()
    for (classifier in classifiers){
      if (dir.exists(file.path("DepressionOutcomesResults", database, outcomeId, classifier))){
        pred <- readRDS(file.path("DepressionOutcomesResults", database, outcomeId, classifier, "plpResult/runPlp.rds"))$prediction
        res[[classifier]] <- pred %>% filter(evaluationType=="Test") %>% select(rowId, value, outcomeCount)
      }
    }
    if (length(res)!=length(classifiers)){
      stop("missing classifier")
    }
    testPrediction <- do.call(cbind, res)
    testPrediction <- cbind(rowId = testPrediction[,c(1,3)],testPrediction[,grep('value',colnames(testPrediction))])
    colnames(testPrediction) <- gsub('\\.value','', colnames(testPrediction))
    colnames(testPrediction)[1:2] <- c('rowId', 'outcome')
    dir.create(file.path("StackingEnsembles", database, outcomeId), recursive = TRUE)
    saveRDS(testPrediction, file.path("StackingEnsembles", database, outcomeId, "testPrediction.Rds"))
  }
}

# base learner predictions on external validation
for (database in databases){
  for (outcomeId in outcomeIds){
    for (valDatabase in databases[!databases %in% database]){
      res <- list()
      for (classifier in classifiers){
        if (dir.exists(file.path("DepressionOutcomesResults", database, outcomeId, classifier))){
          if (dir.exists(file.path("DepressionOutcomesResults", database, "externalValidation", valDatabase, outcomeId, classifier))){
            pred <- readRDS(file.path("DepressionOutcomesResults", database, "externalValidation", valDatabase, outcomeId, classifier, "validationResult.Rds"))$prediction
            res[[classifier]] <- pred %>% select(rowId, value, outcomeCount)
          }
        }
      }
      if (length(res)!=length(classifiers)){
        stop("missing classifier")
      }
      prediction <- do.call(cbind, res)
      prediction <- cbind(rowId = prediction[,c(1,3)],prediction[,grep('value',colnames(prediction))])
      colnames(prediction) <- gsub('\\.value','', colnames(prediction))
      colnames(prediction)[1:2] <- c('rowId', 'outcome')
      dir.create(file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId), recursive = TRUE)
      saveRDS(prediction, file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "prediction.Rds"))
    }
  }
}

for (database in databases){
  for (outcomeId in outcomeIds){
    if(dir.exists(file.path("StackingEnsembles", database, outcomeId))){
      testPrediction <- readRDS(file.path("StackingEnsembles", database, outcomeId, "testPrediction.Rds"))
      cvPrediction <- readRDS(file.path("StackingEnsembles", database, outcomeId, "cvPrediction.Rds"))
      
      try({
        dataF <- cvPrediction[,-1]
        lrMod <- glm(outcome ~ ., data = dataF, family = "binomial")
        
        stackingRisk <- data.frame(value = predict(lrMod, testPrediction[,-c(1:2)], type = "response"),
                                   rowId = testPrediction[,1],
                                   outcomeCount = testPrediction[,2])
        attr(stackingRisk, "metaData")$modelType <- "binary"
        stackingRisk$evaluation <- "Test"
        saveRDS(stackingRisk, file.path("StackingEnsembles", database, outcomeId, "stackingRisk.Rds"))
        
        stackingEnsemble <- list(auc = PatientLevelPrediction::computeAuc(stackingRisk, confidenceInterval = T),
                                 brier = PatientLevelPrediction:::brierScore(stackingRisk),
                                 calDF = PatientLevelPrediction::getCalibrationSummary(stackingRisk, predictionType = "binary", numberOfStrata = 100, truncateFraction = 0.01),
                                 calInLarge = PatientLevelPrediction:::calibrationInLarge(stackingRisk),
                                 mod = lrMod)
        saveRDS(stackingEnsemble, file.path("StackingEnsembles", database, outcomeId, "stackingEnsemble.Rds"))
        
        for (valDatabase in databases[!databases %in% database]){
          if(dir.exists(file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId))){
            prediction <- readRDS(file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "prediction.Rds"))
            stackingRisk <- data.frame(value = predict(lrMod, prediction[,-c(1:2)], type = "response"),
                                       rowId = prediction[,1],
                                       outcomeCount = prediction[,2])
            attr(stackingRisk, "metaData")$modelType <- "binary"
            stackingRisk$evaluation <- "Test"
            saveRDS(stackingRisk, file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "stackingRisk.Rds"))
            
            stackingEnsemble <- list(auc = PatientLevelPrediction::computeAuc(stackingRisk, confidenceInterval = T),
                                     brier = PatientLevelPrediction:::brierScore(stackingRisk),
                                     calDF = PatientLevelPrediction::getCalibrationSummary(stackingRisk, predictionType = "binary", numberOfStrata = 100, truncateFraction = 0.01),
                                     calInLarge = PatientLevelPrediction:::calibrationInLarge(stackingRisk),
                                     mod = lrMod)
            saveRDS(stackingEnsemble, file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "stackingEnsemble.Rds"))
          }
        }
      })
    }
  }
}

########## simpler ensembles #################

for (database in databases){
  for (outcomeId in outcomeIds){
    if(dir.exists(file.path("StackingEnsembles", database, outcomeId))){
      testPrediction <- readRDS(file.path("StackingEnsembles", database, outcomeId, "testPrediction.Rds"))
      cvAucs <- readRDS(file.path("StackingEnsembles", database, outcomeId, "aucs.Rds"))
      
      # simple average ensemble:
      simpleAverageRisk <- data.frame(value = rowMeans(testPrediction[,-c(1:2)]),
                                      rowId = testPrediction[,1],
                                      outcomeCount = testPrediction[,2])
      attr(simpleAverageRisk, "metaData")$modelType <- "binary"
      simpleAverageRisk$evaluation <- "Test"
      saveRDS(simpleAverageRisk, file.path("StackingEnsembles", database, outcomeId, "simpleAverageRisk.Rds"))
      
      simpleAverageEnsemble <- list(auc = PatientLevelPrediction::computeAuc(simpleAverageRisk, confidenceInterval = T),
                                    brier = PatientLevelPrediction:::brierScore(simpleAverageRisk),
                                    calDF = PatientLevelPrediction::getCalibrationSummary(simpleAverageRisk, predictionType = "binary", numberOfStrata = 100, truncateFraction = 0.01),
                                    calInLarge = PatientLevelPrediction:::calibrationInLarge(simpleAverageRisk))
      saveRDS(simpleAverageEnsemble, file.path("StackingEnsembles", database, outcomeId, "simpleAverageEnsemble.Rds"))
      
      for (valDatabase in databases[!databases %in% database]){
        if(dir.exists(file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId))){
          prediction <- readRDS(file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "prediction.Rds"))
          simpleAverageRisk <- data.frame(value = rowMeans(prediction[,-c(1:2)]),
                                          rowId = prediction[,1],
                                          outcomeCount = prediction[,2])
          attr(simpleAverageRisk, "metaData")$modelType <- "binary"
          simpleAverageRisk$evaluation <- "Test"
          saveRDS(simpleAverageRisk, file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "simpleAverageRisk.Rds"))
          
          simpleAverageEnsemble <- list(auc = PatientLevelPrediction::computeAuc(simpleAverageRisk, confidenceInterval = T),
                                        brier = PatientLevelPrediction:::brierScore(simpleAverageRisk),
                                        calDF = PatientLevelPrediction::getCalibrationSummary(simpleAverageRisk, predictionType = "binary", numberOfStrata = 100, truncateFraction = 0.01),
                                        calInLarge = PatientLevelPrediction:::calibrationInLarge(simpleAverageRisk))
          saveRDS(simpleAverageEnsemble, file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "simpleAverageEnsemble.Rds"))
        }
      }
      
      # AUC weighted ensemble:
      tauc <- as.numeric(unlist(cvAucs[names(cvAucs)%in%colnames(testPrediction)]))
      names(tauc) = names(cvAucs)
      aucWeightedRisk <- data.frame(value = matrix(as.numeric(unlist(testPrediction[,-c(1:2)])), ncol = length(classifiers), byrow = FALSE)%*%(tauc)/sum(tauc),
                                    rowId = testPrediction[,1],
                                    outcomeCount = testPrediction[,2])
      attr(aucWeightedRisk, "metaData")$modelType <- "binary"
      aucWeightedRisk$evaluation <- "Test"
      saveRDS(aucWeightedRisk, file.path("StackingEnsembles", database, outcomeId, "aucWeightedRisk.Rds"))
      
      aucWeightedEnsemble <- list(auc = PatientLevelPrediction::computeAuc(aucWeightedRisk, confidenceInterval = T),
                                  brier = PatientLevelPrediction:::brierScore(aucWeightedRisk),
                                  calDF = PatientLevelPrediction::getCalibrationSummary(aucWeightedRisk, predictionType = "binary", numberOfStrata = 100, truncateFraction = 0.01),
                                  calInLarge = PatientLevelPrediction:::calibrationInLarge(aucWeightedRisk),
                                  weights = (tauc)/sum(tauc))
      saveRDS(aucWeightedEnsemble, file.path("StackingEnsembles", database, outcomeId, "aucWeightedEnsemble.Rds"))
      
      for (valDatabase in databases[!databases %in% database]){
        if(dir.exists(file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId))){
          prediction <- readRDS(file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "prediction.Rds"))
          aucWeightedRisk <- data.frame(value = matrix(as.numeric(unlist(prediction[,-c(1:2)])), ncol = length(classifiers), byrow = FALSE)%*%(tauc)/sum(tauc),
                                        rowId = prediction[,1],
                                        outcomeCount = prediction[,2])
          attr(aucWeightedRisk, "metaData")$modelType <- "binary"
          aucWeightedRisk$evaluation <- "Test"
          saveRDS(aucWeightedRisk, file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "aucWeightedRisk.Rds"))
          
          aucWeightedEnsemble <- list(auc = PatientLevelPrediction::computeAuc(aucWeightedRisk, confidenceInterval = T),
                                      brier = PatientLevelPrediction:::brierScore(aucWeightedRisk),
                                      calDF = PatientLevelPrediction::getCalibrationSummary(aucWeightedRisk, predictionType = "binary", numberOfStrata = 100, truncateFraction = 0.01),
                                      calInLarge = PatientLevelPrediction:::calibrationInLarge(aucWeightedRisk))
          saveRDS(aucWeightedEnsemble, file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "aucWeightedEnsemble.Rds"))
        }
      }
      
      # base learner selection (best cross-validated AUC)
      bestCvLearnerRisk <- data.frame(value = testPrediction[,names(which.max(unlist(cvAucs)))],
                                      rowId = testPrediction[,1],
                                      outcomeCount = testPrediction[,2])
      attr(bestCvLearnerRisk, "metaData")$modelType <- "binary"
      bestCvLearnerRisk$evaluation <- "Test"
      saveRDS(bestCvLearnerRisk, file.path("StackingEnsembles", database, outcomeId, "bestCvLearnerRisk.Rds"))
      
      bestCvLearnerEnsemble <- list(auc = PatientLevelPrediction::computeAuc(bestCvLearnerRisk, confidenceInterval = T),
                                    brier = PatientLevelPrediction:::brierScore(bestCvLearnerRisk),
                                    calDF = PatientLevelPrediction::getCalibrationSummary(bestCvLearnerRisk, predictionType = "binary", numberOfStrata = 100, truncateFraction = 0.01),
                                    calInLarge = PatientLevelPrediction:::calibrationInLarge(bestCvLearnerRisk),
                                    bestCvLearner = names(which.max(unlist(cvAucs))))
      saveRDS(bestCvLearnerEnsemble, file.path("StackingEnsembles", database, outcomeId, "bestCvLearnerEnsemble.Rds"))
      
      for (valDatabase in databases[!databases %in% database]){
        if(dir.exists(file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId))){
          prediction <- readRDS(file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "prediction.Rds"))
          bestCvLearnerRisk <- data.frame(value = prediction[,names(which.max(unlist(cvAucs)))],
                                          rowId = prediction[,1],
                                          outcomeCount = prediction[,2])
          attr(bestCvLearnerRisk, "metaData")$modelType <- "binary"
          bestCvLearnerRisk$evaluation <- "Test"
          saveRDS(bestCvLearnerRisk, file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "bestCvLearnerRisk.Rds"))
          
          bestCvLearnerEnsemble <- list(auc = PatientLevelPrediction::computeAuc(bestCvLearnerRisk, confidenceInterval = T),
                                        brier = PatientLevelPrediction:::brierScore(bestCvLearnerRisk),
                                        calDF = PatientLevelPrediction::getCalibrationSummary(bestCvLearnerRisk, predictionType = "binary", numberOfStrata = 100, truncateFraction = 0.01),
                                        calInLarge = PatientLevelPrediction:::calibrationInLarge(bestCvLearnerRisk))
          saveRDS(bestCvLearnerEnsemble, file.path("StackingEnsembles", database, "externalValidation", valDatabase, outcomeId, "bestCvLearnerEnsemble.Rds"))
        }
      }
    }
  }
}

