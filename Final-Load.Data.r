library(readr)

### Load dplyr and tidyr, useful for data wrangling
library(dplyr)
library(tidyr)
library(stringr)

KEY_ACTION_HOLD = "Hold"
KEY_ACTION_DOWN_DOWN = "Down-Down"
KEY_ACTION_UP_DOWN = "Up-Down"

keyPress.Actions = c(
  KEY_ACTION_HOLD, 
  KEY_ACTION_DOWN_DOWN, 
  KEY_ACTION_UP_DOWN)

keyPressVariableNamesSimplified = list(
  'DD.period.t' = '.-t',
  'DD.t.i' = 't-i',
  'DD.i.e' = 'i-e',
  'DD.e.five' = 'e-5',
  'DD.five.Shift.r' = '5-R',
  'DD.Shift.r.o' = 'R-o',
  'DD.o.a' = 'o-a',
  'DD.a.n' = 'a-n',
  'DD.n.l' = 'n-l',
  'DD.l.Return' = 'l-Return',
  
  'H.period' = '.',
  'H.t' = 't',
  'H.i' = 'i',
  'H.e' = 'e',
  'H.five' = '5',
  'H.Shift.r' = 'R',
  'H.o' = 'o',
  'H.a' = 'a',
  'H.n' = 'n',
  'H.l' = 'l',
  'H.Return' = 'Return',
  
  'UD.period.t' = '.-t',
  'UD.t.i' = 't-i',
  'UD.i.e' = 'i-e',
  'UD.e.five' = 'e-5',
  'UD.five.Shift.r' = '5-R',
  'UD.Shift.r.o' = 'R-o',
  'UD.o.a' = 'o-a',
  'UD.a.n' = 'a-n',
  'UD.n.l' = 'n-l',
  'UD.l.Return' = 'l-Return'
)

KeyPress.Keys = c(
  ".", 
  "t", 
  "i", 
  "e", 
  "5", 
  "R", 
  "o", 
  "a", 
  "n", 
  "l", 
  "Return")

KeyPress.Key.Transition = c(
  '.-t', 
  't-i', 
  'i-e', 
  'e-5', 
  '5-R', 
  'R-o', 
  'o-a', 
  'a-n', 
  'n-l', 
  'l-Return')

KeyPress.Sequence = c(
  ".", 
  '.-t', 
  "t", 
  't-i', 
  "i", 
  'i-e', 
  "e", 
  'e-5', 
  "5", 
  '5-R', 
  "R", 
  'R-o', 
  "o", 
  'o-a', 
  "a",  
  'a-n', 
  "n", 
  'n-l', 
  "l",
  'l-Return', 
  "Return")

simplifiedSubjectLabels = c(letters, LETTERS)[-52]

extractKeyAction = function(variable){

  actions = c(
    "H" = KEY_ACTION_HOLD, 
    "DD" = KEY_ACTION_DOWN_DOWN, 
    "UD" = KEY_ACTION_UP_DOWN)
  
  firstColumns = str_split(variable, "\\.", simplify = T)[, 1]
  
  factor(
    sapply(firstColumns, function(str) actions[str]), 
    c(KEY_ACTION_HOLD, KEY_ACTION_DOWN_DOWN, KEY_ACTION_UP_DOWN)
  )
}

extractKey = function(variables, levels) {
  values = sapply(variables, function(str){
    keyPressVariableNamesSimplified[[str]]
  })
  
  factor(values, levels)
}

replaceSubjectLabel = function(subject) {
  sortedSubjects = sort(unique(subject))
  
  mapped = Map(
    function(subject, index) 
      simplifiedSubjectLabels[index],
    sortedSubjects,
    seq_along(sortedSubjects))
  
  factor(
    sapply(subject, function(subj) mapped[subj]),
    levels = simplifiedSubjectLabels
  )
}

### Prepare data frame that contains the data dictionary for the dataset
source("Final-Prep.Data.Dictionary.r")
# known.DataDictionary

known = read_csv("known.csv")

### Drop ID column
known = known %>% 
  select(-X1)

### Extract new/shortened name for subject
known.ShortenedSubjectMap = known %>% 
  mutate(subject.s = replaceSubjectLabel(subject)) %>%
  select(subject, subject.s) %>% 
  distinct(subject, subject.s) %>% 
  rename("Old Subject" = subject, "New Subject" = subject.s)

### Replace subject labels to make them easier to print in plots and hopefully in modelling
known = known %>%
  mutate(row.id = paste(subject, sessionIndex, rep, sep = "-")) %>% 
  mutate(subject = replaceSubjectLabel(subject))


### Transform to long format, this should make it easier to plot in GGPlot
known.Long.PerSubject = known %>% 
  gather(key = "variable", ... = -c(subject, row.id)) %>% 
  mutate(variable = factor(variable, names(keyPressVariableNamesSimplified))) %>% 
  filter(variable != "rep", variable != "sessionIndex") %>% 
  mutate(Key.Action = extractKeyAction(variable)) %>% 
  mutate(Key.Press = extractKey(variable, KeyPress.Sequence))

### Extract Key Presses
known.Long.KeyHold = known.Long.PerSubject %>% 
  filter(Key.Action == KEY_ACTION_HOLD)

### Extract Down to Down
known.Long.KeyDownDown = known.Long.PerSubject %>% 
  filter(Key.Action == KEY_ACTION_DOWN_DOWN)

### Extract Up to Down
known.Long.KeyUpDown = known.Long.PerSubject %>% 
  filter(Key.Action == KEY_ACTION_UP_DOWN)


### Calculate summary statistics for numeric predictor variables
known.NumericPredictorSummary = known.Long.PerSubject %>% 
  group_by(variable) %>% 
  do({
    data.frame(
      "Count" = nrow(.),
      "Min" = min(.$value),
      "1st Quantile" = quantile(.$value, 0.25),
      "Median" = median(.$value),
      "Mean" = mean(.$value),
      "3rd Quantile" = quantile(.$value, 0.75),
      "Max" = max(.$value),
      "Standard Deviation" = sd(.$value),
      check.names = F
    )
  })

### Calculate summary statistics for numeric predictor variables (per subject)
known.NumericPredictorSummaryPerSubject = known.Long.PerSubject %>% 
  group_by(subject, variable) %>% 
  do({
    data.frame(
      "Count" = nrow(.),
      "Min" = min(.$value),
      "1st Quantile" = quantile(.$value, 0.25),
      "Median" = median(.$value),
      "Mean" = mean(.$value),
      "3rd Quantile" = quantile(.$value, 0.75),
      "Max" = max(.$value),
      "Standard Deviation" = sd(.$value),
      check.names = F
    )
  })

# known.NumericPredictorSummaryPerSubject.Wide = known.NumericPredictorSummaryPerSubject.Long %>% 
#   select(-Count) %>%   
#   gather(key = "stat", ... = - c(Action, KeyPress, variable, subject)) %>%  
#   spread(subject, value)



rm(replaceSubjectLabel)

