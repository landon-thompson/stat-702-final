source('Final.r')

library(glmnet)
require(parallel)

####
#
# LASSO Regression
#
####


modeller.Ridge = function(train, test, subject, formula) {
  train.mat <- model.matrix(formula, data = train)
  train.y <- train$subject
  
  test.mat <- model.matrix(formula, data = test)
  test.y <- as.factor(test$subject)
  
  tune <- cv.glmnet(train.mat,train.y, alpha = 1, family = "binomial",parallel = TRUE)
  
  model = glmnet(train.mat, 
                 train.y,
                 alpha = 1, 
                 lambda = tune$lambda.min,
                 family ="binomial")
  
  predictions = predict(model, test.mat, type = 'response')
  predictions.classes =  ifelse(predictions >= 0.5, subject, OTHER_SUBJECT_STR)
  
  score = binaryScoreHelper(
    observed = test$subject, 
    predicted = predictions.classes, 
    model = model, 
    predictedProbs = predictions)
  
  score
}


all.Ridge.regression.models = lapply(simplifiedSubjectLabels, function(subjectLabel){
  data = one.VS.other.DataHelper(known, subjectLabel, 0.7)
  modeller.Ridge(data$train, data$test, subjectLabel,subject.FullFormula.UD)
})


for(i in 1:51){
  #readline(prompt="Press [enter] to continue")
  print(all.Ridge.regression.models[[i]][2]$model$lambda)
}

for(i in 1:51){
  #readline(prompt="Press [enter] to continue")
  print(all.Ridge.regression.models[[i]][1]$confusionMatrix$error)
}

for(i in 1:51){
  #readline(prompt="Press [enter] to continue")
  print(all.Ridge.regression.models[[i]][1]$confusionMatrix$matrix)
}

Lambda <- NULL
for(i in 1:51){
  Lambda[i] <- all.Ridge.regression.models[[i]][2]$model$lambda
}

plot(Lambda)

for(i in 1:51){
  print(all.Ridge.regression.models[[i]][2]$model$beta)
}





