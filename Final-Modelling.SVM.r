

library(e1071)

### Comment on scaling
### https://stats.stackexchange.com/questions/37669/libsvm-reaching-max-number-of-iterations-warning-and-cross-validation


SVM.CostRange = 10^(seq(-2, 2, length.out = 20))


OBJ_SVM_LINEAR = "obj/svm.linear.tune"
OBJ_SVM_POLYNOMIAL = "obj/svm.polynomial.tune"
OBJ_SVM_RADIAL = "obj/svm.radial.tune"

if(file.exists(OBJ_SVM_LINEAR) == FALSE) {
  ####
  #
  # Tune linear hyperparameters
  #
  ####
  start.time = Sys.time()
  
  set.seed(702)
  tune.SVM.Linear = tune(
    method = svm,
    train.x = subject.FullFormula,
    data = known.Train,
    kernel = "linear",
    ranges = list(cost = SVM.CostRange),
    tunecontrol = tune.control(cross = 10, sampling = "cross")
  )
  
  serializeObjectToFile(tune.SVM.Linear, OBJ_SVM_LINEAR)
  
  end.time = Sys.time()
  end.time-start.time
}

if(file.exists(OBJ_SVM_POLYNOMIAL) == FALSE) {
  ####
  #
  # Tune polynomials hyperparameters
  #
  ####
  start.time = Sys.time()
  
  set.seed(702)
  tune.SVM.Polynomial = tune(
    method = svm,
    train.x = subject.FullFormula,
    data = known.Train,
    kernel = "polynomial",
    ranges = list(
      cost = SVM.CostRange, 
      degree = 2:4),
    tunecontrol = tune.control(cross = 10, sampling = "cross")
  )
  
  serializeObjectToFile(tune.SVM.Polynomial, OBJ_SVM_POLYNOMIAL)
  
  end.time = Sys.time()
  end.time-start.time
}

if(file.exists(OBJ_SVM_RADIAL) == FALSE) {
  ####
  #
  # Tune radial hyperparameters
  #
  ####
  start.time = Sys.time()
  
  set.seed(702)
  tune.SVM.Radial = tune(
    method = svm,
    train.x = subject.FullFormula,
    data = known.Train,
    kernel = "radial",
    ranges = list(
      cost = SVM.CostRange, 
      gamma = 10^(-2:2)),
    tunecontrol = tune.control(cross = 10, sampling = "cross")
  )
  
  serializeObjectToFile(tune.SVM.Radial, OBJ_SVM_RADIAL)
  
  end.time = Sys.time()
  end.time-start.time
}


tune.SVM.Linear = deserializeObjectFromFile(OBJ_SVM_LINEAR)
print(tune.SVM.Linear)

tune.SVM.Polynomial = deserializeObjectFromFile(OBJ_SVM_POLYNOMIAL)
print(tune.SVM.Polynomial)

tune.SVM.Radial = deserializeObjectFromFile(OBJ_SVM_RADIAL)
print(tune.SVM.Radial)

### d

## Fit SVM, with linear kernel
svm.Linear = svm(
  subject.FullFormula, 
  data = known.Train, 
  kernel = "linear", 
  cost = tune.SVM.Linear$best.parameters$cost)
summary(svm.Linear)

## Fit SVM, with polynomial kernel
svm.Polynomial = svm(
  subject.FullFormula, 
  data = known.Train, 
  kernel = "polynomial", 
  cost = tune.SVM.Polynomial$best.parameters$cost, 
  degree = tune.SVM.Polynomial$best.parameters$degree)
summary(svm.Polynomial)

## Fit SVM, with radial kernel
svm.Radial = svm(
  subject.FullFormula, 
  data = known.Train, 
  kernel = "radial", 
  cost = tune.SVM.Radial$best.parameters$cost, 
  gamma = tune.SVM.Radial$best.parameters$gamma)
summary(svm.Radial)




svm.Linear.TestError = mean(predict(svm.Linear, known.Test) != known.Test$subject)
svm.Polynomial.TestError = mean(predict(svm.Polynomial, known.Test) != known.Test$subject)
svm.Radial.TestError = mean(predict(svm.Radial, known.Test) != known.Test$subject)




