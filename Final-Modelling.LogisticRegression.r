
source('Final.r')



####
#
# Logistic Regression
#
####
modeller.LogisticRegression = function(train, test, formula_, subject) {
  model = glm(
    formula_, 
    data = train,
    family = binomial())
  
  predictions = predict(model, test, type = "response")
  predictions.classes =  ifelse(predictions >= 0.5, subject, OTHER_SUBJECT_STR)
  
  various.cutoffs = lapply(seq(0.1,0.9, 0.1), function(cutOff){
    binaryConfMatrixHelper(
      test$subject, 
      ifelse(predictions >= cutOff, subject, OTHER_SUBJECT_STR)
    )
  })
  
  # colnames(various.cutoffs) = paste(">=", seq(0.1,0.9, 0.1))

  score = binaryScoreHelper(
    observed = test$subject, 
    predicted = predictions.classes, 
    model = model, 
    variousCutoffs = various.cutoffs,
    predictedProbs = predictions)
  
  score
}

####
#
# Fit logistic regression model with DD variables
#
####
log.reg.DDModels = lapply(simplifiedSubjectLabels, function(subjectLabel){
  data = one.VS.other.DataHelper(
    originalData = known.Train,
    subjectLabel = subjectLabel, 
    splitRatio = 0.7,
    repeatSubject = T)
  
  modeller.LogisticRegression(
    train = data$train, 
    test = data$test, 
    formula_ = subject.FullFormula.DD,
    subject = subjectLabel)
})

####
#
# Fit logistic regression model with UD variables
#
####
log.reg.UDModels = lapply(simplifiedSubjectLabels, function(subjectLabel){
  data = one.VS.other.DataHelper(
    originalData = known.Train,
    subjectLabel = subjectLabel, 
    splitRatio = 0.7,
    repeatSubject = T)
  
  modeller.LogisticRegression(
    train = data$train, 
    test = data$test, 
    formula_ = subject.FullFormula.UD,
    subject = subjectLabel)
})


####
#
# Iterate through models, viewing contents
#
####
sapply(1:51, function(class_index){
  print(summary(log.reg.UDModels[[class_index]]$model))
  readline("Enter...")
})


####
#
# Calculate error rate, using softmax to scale between [0, 1]
#
###
log.reg.DDErrorRate = mean(softmaxFunction.Predict(log.reg.DDModels, known.Test) != as.integer(known.Test$subject))
log.reg.UDErrorRate = mean(softmaxFunction.Predict(log.reg.UDModels, known.Test) != as.integer(known.Test$subject))
