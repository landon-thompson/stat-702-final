
unknown_ = read_csv("unknown.csv")


unknown = unknown_ %>% 
  ## Rename session.id
  rename(sessionIndex = session.id) %>% 
  ## Perform prediction
  mutate(subject = predict(RF.model.1000, newdata = unknown_)) %>%
  ## Create a unique ID for each row (useful for data transforms)
  mutate(row.id = paste(subject, sessionIndex, rep, sep = "-"))

unknown.Long.PerSubject = unknown %>% 
  ## Transform to long form
  gather(key = "variable", ... = -c(subject, row.id)) %>% 
  ## Remove rep and sessionIndex
  filter(variable != "rep", variable != "sessionIndex") %>% 
  ## Transform and Order variable column
  mutate(variable = factor(variable, names(keyPressVariableNamesSimplified))) %>% 
  ## Extract action
  mutate(Key.Action = extractKeyAction(variable)) %>% 
  ## Extract Key
  mutate(Key.Press = extractKey(variable, KeyPress.Sequence))

rm(unknown_)


## Plot heatmap for the predictions
plotHeatMap(unknown.Long.PerSubject, main = "Password entry average key press/transitions (unknown.csv)")

