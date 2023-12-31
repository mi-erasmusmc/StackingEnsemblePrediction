# @file gradientBoostingMachine.R
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

#' Create setting for gradient boosting machine model using gbm_xgboost implementation
#'
#' @param ntrees     The number of trees to build 
#' @param nthread   The number of computer threads to (how many cores do you have?)
#' @param earlyStopRound   If the performance does not increase over earlyStopRound number of interactions then training stops (this prevents overfitting)
#' @param maxDepth  Maximum number of interactions - a large value will lead to slow model training
#' @param minRows   The minimum number of rows required at each end node of the tree
#' @param learnRate The boosting learn rate
#' @param seed       An option to add a seed when training the final model
#'
#' @examples
#' model.gbm <- setGradientBoostingMachine(ntrees=c(10,100), nthread=20,
#'                            maxDepth=c(4,6), learnRate=c(0.1,0.3))
#'
#' @export
setGradientBoostingMachine <- function(ntrees=c(100, 1000), nthread=20, earlyStopRound = 25,
                                  maxDepth=c(4,6,17), minRows=2, learnRate=c(0.005, 0.01,0.1),
                                  seed= sample(10000000,1)){
  
  ensure_installed("xgboost")
  
  checkIsClass(seed, c('numeric', 'integer'))
  
  if(length(nthread)>1)
    stop(paste('nthreads must be length 1'))
  if(!class(seed)%in%c('numeric','NULL', 'integer'))
    stop('Invalid seed')
  if(!class(ntrees) %in% c("numeric", "integer"))
    stop('ntrees must be a numeric value >0 ')
  if(sum(ntrees < 1)>0)
    stop('ntrees must be greater that 0 or -1')
  if(!class(maxDepth) %in% c("numeric", "integer"))
    stop('maxDepth must be a numeric value >0')
  if(sum(maxDepth < 1)>0)
    stop('maxDepth must be greater that 0')
  if(!class(minRows) %in% c("numeric", "integer"))
    stop('minRows must be a numeric value >1')
  if(sum(minRows < 2)>0)
    stop('minRows must be greater that 1')
  if(class(learnRate)!='numeric')
    stop('learnRate must be a numeric value >0 and <= 1')
  if(sum(learnRate <= 0)>0)
    stop('learnRate must be greater that 0')
  if(sum(learnRate > 1)>0)
    stop('learnRate must be less that or equal to 1')
  if(!class(earlyStopRound) %in% c("numeric", "integer", "NULL"))
    stop('incorrect class for earlyStopRound')
  
  param <- split(
    expand.grid(
      ntrees=ntrees, 
      # earlyStopRound=earlyStopRound,
      maxDepth=maxDepth, 
      minRows=minRows, 
      learnRate=learnRate
      ),
    1:(length(ntrees)*length(maxDepth)*length(minRows)*length(learnRate))
  )
  
  attr(param, 'settings') <- list(
    modelType = 'Xgboost',
    earlyStopRound = earlyStopRound,
    seed = seed[[1]],
    modelName = "Gradient Boosting Machine",
    threads = nthread[1],
    varImpRFunction = 'varImpXgboost',
    trainRFunction = 'fitXgboost',
    predictRFunction = 'predictXgboost'
  )
  
  attr(param, 'saveType') <- 'xgboost'
  
  result <- list(
    fitFunction = "fitRclassifier",
    param = param
  )

  class(result) <- 'modelSettings' 
  
  return(result)
}



varImpXgboost <- function(
  model,
  covariateMap
  ){
  
  varImp <- xgboost::xgb.importance(model = model)
  
  varImp$Feature <- as.numeric(varImp$Feature)+1 # adding +1 as xgboost index starts at 0
  varImp <- merge(covariateMap, varImp, by.x='columnId', by.y='Feature')
  varImp <- varImp %>% 
    dplyr::mutate(included = 1) %>%
    dplyr::rename(covariateValue = .data$Gain) %>% 
    dplyr::select(.data$covariateId, .data$covariateValue, .data$included)
  
  return(varImp)
  
}

predictXgboost <- function(
  plpModel, 
  data, 
  cohort
  ){
  
  if(class(data) == 'plpData'){
    # convert
    matrixObjects <- toSparseM(
      plpData = data, 
      cohort = cohort,
      map = plpModel$covariateImportance %>% 
        dplyr::select(.data$columnId, .data$covariateId)
    )
    
    # use the include??
    
    newData <- matrixObjects$dataMatrix
    cohort <- matrixObjects$labels
    
  }else{
    newData <- data
  }
  
  if(class(plpModel) == 'plpModel'){
    model <- plpModel$model
  } else{
    model <- plpModel
  }
    
  pred <- data.frame(value = stats::predict(model, newData))
  prediction <- cohort
  prediction$value <- pred$value
  
  # fix the rowIds to be the old ones?
  # now use the originalRowId and remove the matrix rowId
  prediction <- prediction %>% 
    dplyr::select(-.data$rowId) %>%
    dplyr::rename(rowId = .data$originalRowId)

  attr(prediction, "metaData") <- list(modelType = attr(plpModel, "modelType"))
  
  return(prediction)
}

fitXgboost <- function(
  dataMatrix,
  labels,
  hyperParameters,
  settings
  ){
  
  if(!is.null(settings$earlyStopRound)){
    trainInd <- sample(nrow(dataMatrix), nrow(dataMatrix)*0.9)
    train <- xgboost::xgb.DMatrix(
      data = dataMatrix[trainInd,, drop = F], 
      label = labels$outcomeCount[trainInd]
      )
    test <- xgboost::xgb.DMatrix(
      data = dataMatrix[-trainInd,, drop = F], 
      label = labels$outcomeCount[-trainInd]
      )
    watchlist <- list(train=train, test=test)
    
  } else{
    train <- xgboost::xgb.DMatrix(
      data = dataMatrix, 
      label = labels$outcomeCount
    )
    watchlist <- list(train = train)
  }
  
  outcomes <- sum(labels$outcomeCount>0)
  N <- nrow(labels)
  outcomeProportion <- outcomes/N
  
  # # adding weights
  # weights <- labels$outcomeCount*(N/outcomes)
  # weights[weights == 0] <- 1
  
  model <- xgboost::xgb.train(
    data = train, 
    params = list(
      booster =  'gbtree',
      max_depth = hyperParameters$maxDepth,
      eta = hyperParameters$learnRate, 
      min_child_weight = hyperParameters$minRows,
      objective = "binary:logistic",
      #eval.metric = "logloss"
      # base_score = outcomeProportion,
      eval_metric = "auc"
    ),
    nthread = settings$threads, #?
    nrounds = hyperParameters$ntrees,
    watchlist = watchlist,
    print_every_n = 10,
    early_stopping_rounds = settings$earlyStopRound,
    maximize = T
    # weight = weights # add weights to improve model
    )
  
  return(model)
}
