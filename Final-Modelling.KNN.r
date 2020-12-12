

library(class)

numPerFold = floor(nrow(known.Scrambled)/10)

### Try K from 1 to 20
knnMeanErrors = sapply(1:20, function(k){
  ### For each K, do a 10 fold CV to obtain mean error
  error.Rates = sapply(1:10, function(index){
    ### Perform modelling with current fold
    KFold.helper(known.Scrambled, function(train, test){
      mean(
        knn(
          train = model.frame(subject.FullFormula.PredictorVars, train),
          test = model.frame(subject.FullFormula.PredictorVars, test),
          cl = train$subject,
          k = k
        ) != test$subject
      )
    }, index, numPerFold)
  })
  
  mean(error.Rates)
})

require(ggplot2)
require(dplyr)

plot.KNN <- data.frame(
  x = 1:20,
  y = knnMeanErrors
) %>% 
  ggplot(aes(x, y)) + 
  geom_line() + 
  geom_point(shape = 21, size = 2, fill = "blue") + 
  labs(
    title = "KNN - Mean 10-fold CV error across different k's",
    y = "Error rate",
    x = "K"
  )


min.error <- min(knnMeanErrors)
