
####
#
# RandomForest Helper function
#
####
RF.Helper = function(mtry, ntree, formula, train, test) {
  require(randomForest)
  
  set.seed(702)
  numPerFold = floor(nrow(train)/6)
  
  RF.KFoldErrors = sapply(1:6, function(index){
    KFold.helper(rbind(train, test), function(train_, test_){
      
      model = randomForest(
        formula, 
        data = train_, 
        mtry = mtry,
        ntree = ntree
      )
      
      error.OOB = model$err.rate[[ntree, "OOB"]]
      
      predictions = predict(model, test_, type = "response")
      test.Error = mean(test_$subject != predictions)
      
      c(oob = error.OOB, test = test.Error)
    }, index, numPerFold)
  })
  
  set.seed(702)
  model = randomForest(
    formula,
    data = train, 
    mtry = mtry, 
    ntree = ntree
  )
  
  error.OOB = model$err.rate[[ntree, "OOB"]] 
  
  predictions = predict(model, test, type = "response")
  test.Error = mean(test$subject != predictions)
  
  c(
    CV.OOB = mean(RF.KFoldErrors["oob", ]),
    CV.Test = mean(RF.KFoldErrors["test", ]),
    OOB = error.OOB,
    Test = test.Error
  )
}

####
#
# Helper function for plotting errors from hyper parameter tunning
#
####

RF.PlotHelper = function(errors.500, errors.1000, subtitle = "") {
  require(ggplot2)
  
  plotData = rbind(
    data.frame(
      ntree = "ntree = 500",
      mtry = predictorVarSequence,
      errors.500), 
    data.frame(
      errors.1000,
      mtry = predictorVarSequence,
      ntree = "ntree = 1000")
  ) %>% 
    gather(key = "error", ... = -c(ntree, mtry)) %>% 
    group_by(ntree, error) %>% 
    do({
      data.frame(
        ., 
        is.optimal = .$mtry == which.min(.$value),
        error.source = sapply(.$error, function(x) ifelse(grepl("(^CV.)", x), "CV", "No-CV")),
        error.type = str_replace(.$error, "(CV.)", ""),
        stringsAsFactors = F)
    }) 
  
  plotData %>% 
    ggplot(aes(mtry, value)) + 
    geom_line(aes(color = error.type), size = 1.25) +
    geom_point(aes(alpha = is.optimal), size = 2.5) + 
    facet_wrap(~ ntree + error.source, ncol = 2) + 
    scale_x_continuous(breaks = seq(0, max(predictorVarSequence), 2)) +
    labs(
      title = "Error-rates across mtry(# of considered variables per split)",
      y = "Error rate",
      x = "mtry",
      color = "Error Type",
      alpha = "Is Optimal?"
    )
}

####
#
# Load library for parallele processing; should speed things up a bit
#
####
require(parallel)

cluster = makeCluster(detectCores() - 1)
clusterExport(cluster, "KFold.helper")

## Sequence for iterating all variables
predictorVarSequence = 1:fullNumberOfPredVars

####
#
# Perform hyper parameter tuning, recording both OOB and Test error; 
# these are obtained by doing a 6 fold CV, then averaging
#
####

OBJ_RF_500 = "obj/rf.500.tune.csv"
OBJ_RF_1000 = "obj/rf.1000.tune.csv"

if(file.exists(OBJ_RF_500) == FALSE) {
  ####
  #
  # Tune mtry, for 500 trees
  #
  ####
  start.time = Sys.time()
  
  RF.errors.500 = parSapply(
    cl = cluster, 
    X = predictorVarSequence,
    FUN = RF.Helper, 
    ntree = 500, 
    formula = subject.FullFormula, 
    train = known.Train, 
    test = known.Test)
  
  write.csv(x = data.frame(t(RF.errors.500)), file = OBJ_RF_500)
  
  end.time = Sys.time()
  end.time - start.time
}

if(file.exists(OBJ_RF_1000) == FALSE) {
  ####
  #
  # Tune mtry, for 1000 trees
  #
  ####
  start.time = Sys.time()
  
  RF.errors.1000 = parSapply(
    cl = cluster, 
    X = predictorVarSequence,
    FUN = RF.Helper, 
    ntree = 1000, 
    formula = subject.FullFormula, 
    train = known.Train, 
    test = known.Test)
  
  write.csv(x = data.frame(t(RF.errors.1000)), file = OBJ_RF_1000)
  
  end.time = Sys.time()
  end.time - start.time
}

stopCluster(cluster)

RF.errors.500 = read.csv(OBJ_RF_500, row.names = 1)
RF.errors.1000 = read.csv(OBJ_RF_1000, row.names = 1)

####
#
# Plot errors from hyper parameter tunning
#
####
RF.PlotHelper(RF.errors.500, RF.errors.1000)



####
#
# Using the optimal hyper-parameter, fit a new model; then calculate error rate
#
####
require(randomForest)

optimalMTRY = which.min(RF.errors.1000$OOB)

# https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr

set.seed(702)
RF.model.1000 = randomForest(
  subject.FullFormula, 
  data = known.Train, 
  mtry = optimalMTRY,
  ntree = 1000
)

RF.TestError = mean(predict(RF.model.1000, known.Test) != known.Test$subject)

varImpPlot(RF.model.1000, main = "Variable importance for 1000 tree Random forest")

importance(RF.model.1000)

# View(data.frame(
#   variable = rownames(RF.model.1000$importance),
#   RF.model.1000$importance) %>% 
#     arrange(desc(MeanDecreaseGini)))


#### Calculate test error rate
####











# ## DD variables
# RF.errors.500.DD = parSapply(
#   cl = cluster, 
#   X = predictorVarSequence,
#   FUN = RF.Helper, 
#   ntree = 500, 
#   formula = subject.FullFormula.DD, 
#   train = known.Train, 
#   test = known.Test)
# 
# RF.errors.1000.DD = parSapply(
#   cl = cluster, 
#   X = predictorVarSequence,
#   FUN = RF.Helper, 
#   ntree = 1000, 
#   formula = subject.FullFormula.DD, 
#   train = known.Train, 
#   test = known.Test)
# 
# ## UD variables
# RF.errors.500.UD = parSapply(
#   cl = cluster, 
#   X = predictorVarSequence,
#   FUN = RF.Helper, 
#   ntree = 500, 
#   formula = subject.FullFormula.UD, 
#   train = known.Train, 
#   test = known.Test)
# 
# RF.errors.1000.UD = parSapply(
#   cl = cluster, 
#   X = predictorVarSequence,
#   FUN = RF.Helper, 
#   ntree = 1000, 
#   formula = subject.FullFormula.UD, 
#   train = known.Train, 
#   test = known.Test)
# 
# x11()
# RF.PlotHelper(RF.errors.500.DD, RF.errors.1000.DD, "DD vairables")
# 
# x11()
# RF.PlotHelper(RF.errors.500.UD, RF.errors.1000.UD, "UD vairables")



