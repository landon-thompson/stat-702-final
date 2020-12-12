OTHER_SUBJECT_STR = "0"

####
#
# Create confussion matrix for binary outcome
#
####
binaryConfMatrixHelper = function(observed, predicted) {
  conf.mat = table(observed, predicted)
  
  list(
    matrix = conf.mat,
    TP = tryCatch(conf.mat[1,1], error = function(err) NA),
    FP = tryCatch(conf.mat[1,2], error = function(err) NA),
    FN = tryCatch(conf.mat[2,1], error = function(err) NA),
    TN = tryCatch(conf.mat[2,2], error = function(err) NA),
    error = mean(observed != predicted)
  )
}

####
#
# Score a given model
#
####
binaryScoreHelper = function(observed, predicted, model, variousCutoffs = NULL, predictedProbs = NULL) {
  conf.mat = table(observed, predicted)
  
  list(
    confusionMatrix = binaryConfMatrixHelper(observed, predicted),
    model = model,
    variousCutoffs = variousCutoffs,
    predictedProbs = predictedProbs
  )
}

####
#
# Helper function for Validation set approach
#
####
VSA.helper = function(data, modeler, trainPercent){
  
  nobs = nrow(data)
  
  set.seed(702)
  randomised.indexes = sample(nobs)
  
  train.indexes = 1:(nobs*trainPercent)
  
  train = data[randomised.indexes[train.indexes], ]
  validation = data[randomised.indexes[-train.indexes], ]
  
  modeler(train, validation)
}

####
#
# Helper function for Leave One Out Cross Validation
#
####
LOOCV.helper = function(data, modeler, testIndex){
  train = data[-testIndex, ]
  validation = data[testIndex,]
  
  modeler(train, validation)
}

####
#
# Helper function for K-Fold Cross Validation
#
####
KFold.helper = function(data, modeler, testIndex, numPerFold){
  start = (testIndex-1)*numPerFold + 1
  stop = ifelse(testIndex == 6, nrow(data), testIndex*numPerFold)
  
  train = data[-(start:stop), ]
  validation = data[start:stop, ]
  
  modeler(train, validation)
}

####
#
# Helper function for extracting data for one Vs other
#
####
one.VS.other.DataHelper = function(originalData, subjectLabel, splitRatio = NaN, repeatSubject = T) {
  require(dplyr)
  
  currentSubjectIndexes = originalData$subject == subjectLabel
  countOfCurrentSubjects = sum(currentSubjectIndexes)
  
  currentSubjects = originalData[currentSubjectIndexes, ]
  otherSubjects = originalData[!currentSubjectIndexes, ]
  
  if(repeatSubject) {
    ## Repeat to reach just about the size of others
    currentSubjects = currentSubjects[rep(
      1:countOfCurrentSubjects, 
      floor(nrow(otherSubjects)/countOfCurrentSubjects)), ]
  }
  
  full = rbind(currentSubjects, otherSubjects) %>% 
    mutate(subject.old = subject) %>% 
    mutate(subject = ifelse(subject == subjectLabel, subjectLabel, OTHER_SUBJECT_STR)) %>% 
    mutate(subject = factor(subject, c(OTHER_SUBJECT_STR, subjectLabel)))
  
  retVal = list(full = full)
  
  # full = originalData %>% 
  #   mutate(subject.old = subject) %>% 
  #   mutate(subject = ifelse(subject == subjectLabel, subjectLabel, OTHER_SUBJECT_STR)) %>% 
  #   mutate(subject = factor(subject, c(OTHER_SUBJECT_STR, subjectLabel)))
  # 
  # retVal = list(full = full)
  
  if(!is.nan(splitRatio)){
    nobsFull = nrow(full)
    set.seed(702)
    trainIndex = sample(nobsFull, nobsFull*splitRatio)
    
    retVal$train = full[trainIndex, ]
    retVal$test = full[-trainIndex, ]
  }
  
  retVal
}

####
#
# Perform softmax, using the coefficients of the model
#
####
softmaxFunction = function(allModels, testData) {
  ## Store coeffcients as weigths
  modelCoefficients = data.frame(sapply(allModels, function(model){
    coef(model$model)
  }))
  
  ## Fill NA with 0
  modelCoefficients[is.na(modelCoefficients)] = 0
  
  ## Add (Intercept); making it possible for matrix multiplication
  testData_ = data.frame("(Intercept)" = 1, testData, check.names = F)
  
  ## Select in order of coefficients
  testData_ = testData_[, row.names(modelCoefficients)]
  
  require(parallel)
  
  numCores = detectCores() - 1
  cluster = makeCluster(numCores)
  
  clusterExport(cluster, "testData_", envir = environment())
  clusterExport(cluster, "modelCoefficients", envir = environment())
  
  ### Softmax
  ### https://en.wikipedia.org/wiki/Softmax_function
  
  test.SoftmaxScore = parSapply(cluster, 1:nrow(testData_), function(rowIndex){
      currentSample = as.matrix(testData_[rowIndex, ])
      
      ## To improve performance, pre-calculate denominator
      ## SIGMA_1_K(e^(X_T*W_k))
      denominator = sum(
        sapply(1:51, function(index){
          exp(currentSample %*% as.matrix(modelCoefficients[, index]))
        })
      )
      
      ## Run softmax for all 51 classes
      ## (e^(X_T*W_j))/(DENOMINATOR)
      softmax.Res = sapply(1:51, function(class_index){
        numerator = currentSample %*% as.matrix(modelCoefficients[, class_index])
        exp(numerator)/denominator
      })
      
    # softmax.Res
    which.max(softmax.Res)
  })
  
  stopCluster(cluster)
  
  test.SoftmaxScore
}

####
#
# Perform softmax, but using the prediction [0, 1] from model
#
####
softmaxFunction.Predict = function(allModels, testData, probabilityResponseType = "response") {
  ## Store coeffcients as weigths
  allModels = lapply(allModels, function(model){
    model$model
  })
  
  require(parallel)
  
  numCores = detectCores() - 1
  cluster = makeCluster(numCores)
  
  clusterExport(cluster, "testData", envir = environment())
  clusterExport(cluster, "probabilityResponseType", envir = environment())
  
  ### Softmax
  ### https://en.wikipedia.org/wiki/Softmax_function
  
  test.SoftmaxScore = parSapply(cluster, 1:nrow(testData), function(rowIndex){
    currentSample = testData[rowIndex, ]
    
    ## To improve performance, pre-calculate denominator
    ## SIGMA_1_K(e^(X_T*W_k))
    denominator = sum(
      sapply(1:51, function(index){
        exp(predict(allModels[[index]], currentSample, type = probabilityResponseType))
      })
    )
    
    ## Run softmax for all 51 classes
    ## (e^(X_T*W_j))/(DENOMINATOR)
    softmax.Res = sapply(1:51, function(class_index){
      numerator = predict(allModels[[class_index]], currentSample, type = probabilityResponseType)
      exp(numerator)/denominator
    })
    
    # softmax.Res
    which.max(softmax.Res)
  })
  
  stopCluster(cluster)
  
  test.SoftmaxScore
}


serializeObjectToFile = function(obj, file) {
  conn = file(file, open = "wb")
  serialize(
    object = tune.RPart,
    connection = conn
  )
  flush(conn)
}


deserializeObjectFromFile = function(file) {
  unserialize(file(file, open = "rb"))
}












