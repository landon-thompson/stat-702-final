
library(rpart)
library(rpart.plot)

####
# 
# Tune hyper parameters, cp, minbucket, and maxdepth
#
###
RPART_MULTI_TREE_PARAMS_FILE = "obj/rpart.multi.class.params.tune"

if(file.exists(RPART_MULTI_TREE_PARAMS_FILE) == FALSE) {
  
  start.time = Sys.time()
  
  require(e1071)
  
  set.seed(702)
  tune.RPart = tune(
    method = rpart,
    train.x = subject.FullFormula,
    data = known.Train %>% mutate(subject = as.integer(subject)),
    ranges = list(
      cp = 10^seq(-5, -2, length = 20),
      minbucket = c(4, 6, 8, 10, 12),
      maxdepth = 30),
    tunecontrol = tune.control(cross = 10, sampling = "cross")
  )
  
  end.time = Sys.time()
  end.time-start.time
  
  serializeObjectToFile(tune.RPart, RPART_MULTI_TREE_PARAMS_FILE)
}

## Load serialized tune object
tune.RPart = deserializeObjectFromFile(RPART_MULTI_TREE_PARAMS_FILE)


####
# 
# Fit a multi-class classification tree; then perform 6-fold CV to obtain test error
#
####
numPerFold = floor(nrow(known.Scrambled)/6)

Rpart.KFoldErrors = sapply(1:6, function(index){
  KFold.helper(known.Scrambled, function(train_, test_){
    model = rpart(
      formula = subject.FullFormula,
      data =  train_, 
      control = rpart.control(
        cp = tune.RPart$best.parameters$cp,
        minbucket = tune.RPart$best.parameters$minbucket,
        maxdepth = tune.RPart$best.parameters$maxdepth
      ))
    
    mean(predict(model, test_, type = "class") != test_$subject)
    
  }, index, numPerFold)
})

Rpart.testError = mean(Rpart.KFoldErrors)


####
#
# Tune parameters for individual rpart trees
#
####

RPART_1_VS_ALL_PARAMS_FILE = "obj/rpart.params.csv"

if(!file.exists(RPART_1_VS_ALL_PARAMS_FILE)) {
  ####
  #
  # Helper function to tune parameters for 1vsAll trees
  #
  ####
  rpart.helper = function(subjectLabel, formular_, data_){
    
    require(dplyr)
    require(rpart)
    require(e1071)
    
    ## Prep data for 1vAll, but no split
    train = one.VS.other.DataHelper(data_, subjectLabel, repeatSubject = T)$full %>% 
      mutate(subject = as.integer(subject))
    
    ## Use the full data tofor tunning parameters
    set.seed(702)
    tune.RPart = tune(
      method = rpart,
      train.x = formular_,
      data = train,
      ranges = list(
        cp = 10^seq(-10, -2, length = 50),
        minbucket = c(4, 6, 8, 10, 12),
        maxdepth = 30),
      tunecontrol = tune.control(cross = 10, sampling = "cross")
    )
    
    c(
      cp = tune.RPart$best.parameters$cp,
      minbucket = tune.RPart$best.parameters$minbucket,
      maxdepth = tune.RPart$best.parameters$maxdepth
    )
  }
  
  ####
  #
  # Load library for parallele processing; should speed things up a bit
  #
  ####
  require(parallel)
  
  cluster = makeCluster(detectCores() - 1)
  clusterExport(cluster, "KFold.helper", envir = globalenv())
  clusterExport(cluster, "OTHER_SUBJECT_STR", envir = globalenv())
  clusterExport(cluster, "one.VS.other.DataHelper", envir = globalenv())
  
  ####
  #
  # Perform hyper parameter tuning, recording both OOB and Test error;
  # these are obtained by doing a 6 fold CV, then averaging
  #
  ####
  start.time = Sys.time()
  Rpart.1vsAll = parSapply(
    cl = cluster,
    X = simplifiedSubjectLabels,
    FUN = rpart.helper,
    formular_ = subject.FullFormula,
    data_ = known.Train)
  
  end.time = Sys.time()
  end.time-start.time
  
  stopCluster(cluster)
  
  write.csv(
    x = data.frame(t(Rpart.1vsAll)), 
    file = RPART_1_VS_ALL_PARAMS_FILE)
  
  rm(Rpart.1vsAll)
  # Time difference of 1.716466 hours
}

Rpart.1vsAll.OptimalHyperParams = read.csv(RPART_1_VS_ALL_PARAMS_FILE, row.names = 1)

####
#
# Using the optimal parameters, fit 51 classification trees
#
####
rpart.1vsAll.TreeModels = lapply(simplifiedSubjectLabels, function(subjectLabel) {
  ## Prep data for 1vAll
  train = one.VS.other.DataHelper(known.Train, subjectLabel, repeatSubject = T)$full
  params = Rpart.1vsAll.OptimalHyperParams[subjectLabel, ]
  
  set.seed(702)
  model = rpart(
    formula = subject.FullFormula,
    data =  train,
    control = rpart.control(
      cp = params$cp,
      minbucket = params$minbucket,
      maxdepth = params$maxdepth
    ))
  
  list(
    owner = subjectLabel,
    model = model
  )
})

####
#
# Calculate test errors for all 51 classification trees
#
####
rpart.1vsAll.TreeModels.TestErrors = sapply(rpart.1vsAll.TreeModels, function(entry) {
  # rpart.plot(entry$model, main = paste("Tree model plot for -", entry$owner, "VS Others"))
  # readline("Enter...")

  # browser()
  test = one.VS.other.DataHelper(known.Test, entry$owner, repeatSubject = F)$full
  test.Error = binaryConfMatrixHelper(
    observed = test$subject,
    predicted = predict(entry$model, test, type = "class"))$error
  
  test.Error
})

####
#
# Plot test errors for all 51 trees
#
####
# require(ggplot2)
# require(tidyr)
# 
# data.frame(
#   subject = simplifiedSubjectLabels,
#   data.frame(t(rpart.1vsAll.TreeModels.TestErrors))
# ) %>% 
#   gather(... = -subject) %>% 
#   mutate(key = ifelse(key == "error", "Error Rate", "# of variables (variable.importance)")) %>% 
#   ggplot(aes(x = subject, y = value, group = key)) + 
#   geom_line() + 
#   geom_point() + 
#   facet_wrap(~ key, scales = "free_y", ncol = 1)+ 
#   scale_x_discrete(labels = simplifiedSubjectLabels) + 
#   labs(
#     title = "Error rates for each 51 rpart classification trees",
#     subtitle = "",
#     y = "Error rate",
#     x = "Subject"
#   )

data.frame(
  subject = simplifiedSubjectLabels,
  error = rpart.1vsAll.TreeModels.TestErrors
) %>% 
  ggplot(aes(x = subject, y = error, group = 1)) + 
  geom_line() + 
  geom_point(shape = 21, fill = "white") + 
  scale_x_discrete(limits = simplifiedSubjectLabels) + 
  labs(
    title = "Error rates for each 51 rpart classification trees",
    subtitle = "",
    y = "Error rate",
    x = "Subject"
  )





sapply(rpart.1vsAll.TreeModels, function(entry) {
  rpart.plot(entry$model, main = paste("Tree model plot for -", entry$owner, "VS Others"))
  # print(entry$model$variable.importance)
  # readline("Enter...")
})






