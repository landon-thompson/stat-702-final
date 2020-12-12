
####
#
# Combine unknown and know data, to be used to retrain a new model
#
####
combinedData.Train = rbind(
  data.frame(known.Train, source = "known.csv"),
  data.frame(unknown, source = "unknown.csv"))

known.Test$source = "known.csv"
  

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

OBJ_FINAL_RF_500 = "obj/final.rf.500.tune.csv"
OBJ_FINAL_RF_1000 = "obj/final.rf.1000.tune.csv"

if(file.exists(OBJ_FINAL_RF_500) == FALSE) {
  ####
  #
  # Tune mtry, for 500 trees
  #
  ####
  start.time = Sys.time()
  
  Final.RF.errors.500 = parSapply(
    cl = cluster, 
    X = predictorVarSequence,
    FUN = RF.Helper, 
    ntree = 500, 
    formula = subject.FullFormula, 
    train = combinedData.Train, 
    test = known.Test)
  
  write.csv(x = data.frame(t(Final.RF.errors.500)), file = OBJ_FINAL_RF_500)
  
  end.time = Sys.time()
  end.time - start.time
}

if(file.exists(OBJ_FINAL_RF_1000) == FALSE) {
  ####
  #
  # Tune mtry, for 1000 trees
  #
  ####
  start.time = Sys.time()
  
  Final.RF.errors.1000 = parSapply(
    cl = cluster, 
    X = predictorVarSequence,
    FUN = RF.Helper, 
    ntree = 1000, 
    formula = subject.FullFormula, 
    train = combinedData.Train, 
    test = known.Test)
  
  write.csv(x = data.frame(t(Final.RF.errors.1000)), file = OBJ_FINAL_RF_1000)
  
  end.time = Sys.time()
  end.time - start.time
}

stopCluster(cluster)

Final.RF.errors.500 = read.csv(OBJ_FINAL_RF_500, row.names = 1)
Final.RF.errors.1000 = read.csv(OBJ_FINAL_RF_1000, row.names = 1)

####
#
# Plot errors from hyper parameter tunning
#
####
RF.PlotHelper(Final.RF.errors.500, Final.RF.errors.1000)



####
#
# Using the optimal hyper-parameter, fit a new model; then calculate error rate
#
####
require(randomForest)

Final.OptimalMTRY = which.min(Final.RF.errors.1000$OOB)

# https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr

set.seed(702)
Final.RF.model.1000 = randomForest(
  subject.FullFormula, 
  data = combinedData.Train, 
  mtry = Final.OptimalMTRY,
  ntree = 1000
)

Final.RF.TestError = mean(predict(Final.RF.model.1000, known.Test) != known.Test$subject)
Final.RF.TestError

varImpPlot(Final.RF.model.1000, main = "Variable importance for 1000 tree Random forest")

importance(Final.RF.model.1000)
