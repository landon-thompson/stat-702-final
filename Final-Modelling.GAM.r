
subject.FullFormula.DD.Spline = subject ~ s(H.period) + 
  s(DD.period.t) + 
  s(H.t) + 
  s(DD.t.i) +  
  s(H.i) + 
  s(DD.i.e) + 
  s(H.e) + 
  s(DD.e.five) +  
  s(H.five) + 
  s(DD.five.Shift.r) + 
  s(H.Shift.r) + 
  s(DD.Shift.r.o) + 
  s(H.o) + 
  s(DD.o.a) + 
  s(H.a) + 
  s(DD.a.n) + 
  s(H.n) + 
  s(DD.n.l) + 
  s(H.l) + 
  s(DD.l.Return) + 
  s(H.Return)

subject.FullFormula.UD.Spline = subject ~ s(H.period) +
  s(UD.period.t) +
  s(H.t) +
  s(UD.t.i) +
  s(H.i) +
  s(UD.i.e) +
  s(H.e) +
  s(UD.e.five) +
  s(H.five) +
  s(UD.five.Shift.r) +
  s(H.Shift.r) +
  s(UD.Shift.r.o) +
  s(H.o) +
  s(UD.o.a) +
  s(H.a) +
  s(UD.a.n) +
  s(H.n) +
  s(UD.n.l) +
  s(H.l) +
  s(UD.l.Return) +
  s(H.Return)



####
#
# Logistic Regression
#
####
modeller.GAM = function(train, test, formula_, subject) {
  require(gam)
  
  model = gam(
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


GAM_DD_MODELS = "obj/gam.dd.models"
GAM_UD_MODELS = "obj/gam.ud.models"


require(parallel)

numCores = detectCores() - 1
cluster = makeCluster(numCores)

clusterExport(cluster, "OTHER_SUBJECT_STR", envir = environment())
clusterExport(cluster, "known.Train", envir = environment())
clusterExport(cluster, "modeller.GAM", envir = environment())
clusterExport(cluster, "binaryConfMatrixHelper", envir = environment())
clusterExport(cluster, "binaryScoreHelper", envir = environment())
clusterExport(cluster, "one.VS.other.DataHelper", envir = environment())
clusterExport(cluster, "subject.FullFormula.DD.Spline", envir = environment())
clusterExport(cluster, "subject.FullFormula.UD.Spline", envir = environment())

if(file.exists(GAM_DD_MODELS) == FALSE){
  ####
  #
  # Fit GAM model with DD variables
  #
  ####
  GAM.DDModels = parLapply(
    cl = cluster,
    X = simplifiedSubjectLabels, 
    fun = function(subjectLabel) {
      
      data = one.VS.other.DataHelper(
        originalData = known.Train,
        subjectLabel = subjectLabel, 
        splitRatio = 0.7,
        repeatSubject = T)
      
      modeller.GAM(
        train = data$train, 
        test = data$test, 
        formula_ = subject.FullFormula.DD.Spline,
        subject = subjectLabel)
    })
  
  serializeObjectToFile(GAM.DDModels, GAM_DD_MODELS)
}

if(file.exists(GAM_UD_MODELS) == FALSE){
  ####
  #
  # Fit GAM regression model with UD variables
  #
  ####
  GAM.UDModels = parLapply(
    cl = cluster,
    X = simplifiedSubjectLabels, 
    fun = function(subjectLabel){
      
      data = one.VS.other.DataHelper(
        originalData = known.Train,
        subjectLabel = subjectLabel, 
        splitRatio = 0.7,
        repeatSubject = T)
      
      modeller.GAM(
        train = data$train, 
        test = data$test, 
        formula_ = subject.FullFormula.UD.Spline,
        subject = subjectLabel)
    })
  
  serializeObjectToFile(GAM.UDModels, GAM.UDModels)
}

stopCluster(cluster)


#### ERROR: NA/NaN/Inf in foreign function call (arg 5)

GAM.DDModels = deserializeObjectFromFile(GAM_DD_MODELS)
GAM.UDModels = deserializeObjectFromFile(GAM_UD_MODELS)



####
#
# Iterate through models, viewing contents
#
####
sapply(1:51, function(class_index){
  print(summary(GAM.DDModels[[class_index]]$model))
  print(summary(GAM.UDModels[[class_index]]$model))
  readline("Enter...")
})


####
#
# Calculate error rate, using softmax to scale between [0, 1]
#
###
log.reg.DDErrorRate = mean(softmaxFunction.Predict(log.reg.DDModels, known.Test) != as.integer(known.Test$subject))
log.reg.UDErrorRate = mean(softmaxFunction.Predict(log.reg.UDModels, known.Test) != as.integer(known.Test$subject))
