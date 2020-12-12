
### Load know data, as well as perform some data transformation
source("Final-Load.Data.r")
source("Final-Modelling.Core.r")

set.seed(702)

trainIndex = sample(nrow(known), nrow(known)*0.7)

known.Train = known[trainIndex, ]
known.Test = known[-trainIndex, ]

known.Scrambled = known[sample(nrow(known)), ]

subject.FullFormula = subject ~ H.period + 
  DD.period.t + 
  UD.period.t + 
  H.t + 
  DD.t.i + 
  UD.t.i + 
  H.i + 
  DD.i.e + 
  UD.i.e + 
  H.e + 
  DD.e.five + 
  UD.e.five + 
  H.five + 
  DD.five.Shift.r + 
  UD.five.Shift.r + 
  H.Shift.r + 
  DD.Shift.r.o + 
  UD.Shift.r.o + 
  H.o + 
  DD.o.a + 
  UD.o.a + 
  H.a + 
  DD.a.n + 
  UD.a.n + 
  H.n + 
  DD.n.l + 
  UD.n.l + 
  H.l + 
  DD.l.Return + 
  UD.l.Return + 
  H.Return

subject.FullFormula.PredictorVars = ~ H.period + 
  DD.period.t + 
  UD.period.t + 
  H.t + 
  DD.t.i + 
  UD.t.i + 
  H.i + 
  DD.i.e + 
  UD.i.e + 
  H.e + 
  DD.e.five + 
  UD.e.five + 
  H.five + 
  DD.five.Shift.r + 
  UD.five.Shift.r + 
  H.Shift.r + 
  DD.Shift.r.o + 
  UD.Shift.r.o + 
  H.o + 
  DD.o.a + 
  UD.o.a + 
  H.a + 
  DD.a.n + 
  UD.a.n + 
  H.n + 
  DD.n.l + 
  UD.n.l + 
  H.l + 
  DD.l.Return + 
  UD.l.Return + 
  H.Return

subject.FullFormula.DD = subject ~ H.period + 
  DD.period.t + 
  H.t + 
  DD.t.i +  
  H.i + 
  DD.i.e + 
  H.e + 
  DD.e.five +  
  H.five + 
  DD.five.Shift.r + 
  H.Shift.r + 
  DD.Shift.r.o + 
  H.o + 
  DD.o.a + 
  H.a + 
  DD.a.n + 
  H.n + 
  DD.n.l + 
  H.l + 
  DD.l.Return + 
  H.Return

subject.FullFormula.DD.PredictorVars = ~ H.period + 
  DD.period.t + 
  H.t + 
  DD.t.i +  
  H.i + 
  DD.i.e + 
  H.e + 
  DD.e.five +  
  H.five + 
  DD.five.Shift.r + 
  H.Shift.r + 
  DD.Shift.r.o + 
  H.o + 
  DD.o.a + 
  H.a + 
  DD.a.n + 
  H.n + 
  DD.n.l + 
  H.l + 
  DD.l.Return + 
  H.Return

subject.FullFormula.UD = subject ~ H.period +
  UD.period.t +
  H.t +
  UD.t.i +
  H.i +
  UD.i.e +
  H.e +
  UD.e.five +
  H.five +
  UD.five.Shift.r +
  H.Shift.r +
  UD.Shift.r.o +
  H.o +
  UD.o.a +
  H.a +
  UD.a.n +
  H.n +
  UD.n.l +
  H.l +
  UD.l.Return +
  H.Return

subject.FullFormula.UD.PredictorVars = ~ H.period +
  UD.period.t +
  H.t +
  UD.t.i +
  H.i +
  UD.i.e +
  H.e +
  UD.e.five +
  H.five +
  UD.five.Shift.r +
  H.Shift.r +
  UD.Shift.r.o +
  H.o +
  UD.o.a +
  H.a +
  UD.a.n +
  H.n +
  UD.n.l +
  H.l +
  UD.l.Return +
  H.Return


fullNumberOfPredVars = length(attr(terms(subject.FullFormula), "term.labels"))
halfNumberOfPredVars = length(attr(terms(subject.FullFormula.UD), "term.labels"))

# Random Forest
# R-Part
# SVM
# LDA
# GAM
# Logistic Regression
# 
# 
# EDA - Visuals, veyr tricky
