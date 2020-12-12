known.DataDictionary = matrix(
  c(
    'H.period', 
    'The amount of time that the "." is held down.',
    'DD.period.t', 
    'The time between pressing down the "." key to the time to press down the "t" key.',
    'UD.period.t', 
    'The time between the "." key coming up to the time to press down the "t" key.',
    'H.t', 
    'The amount of time that the "t" is held down.',
    'DD.t.i', 
    'The time between pressing down the "t" key to the time to press down the "i" key.',
    'UD.t.i', 
    'The time between the "t" key coming up to the time to press down the "i" key. ',
    'H.i', 
    'The amount of time that the "i" is held down.',
    'DD.i.e', 
    'The time between pressing down the "i" key to the time to press down the "e" key.',
    'UD.i.e', 
    'The time between the "i" key coming up to the time to press down the "e" key. ',
    'H.e', 
    'The amount of time that the "e" is held down.',
    'DD.e.five', 
    'The time between pressing down the "e" key to the time to press down the "5" key.',
    'UD.e.five', 
    'The time between the "e" key coming up to the time to press down the "5" key.',
    'H.five', 
    'The amount of time that the "5" is held down.',
    'DD.five.Shift.r', 
    'The time between pressing down the "5" key to the time to press down the "shift+r" key combination.',
    'UD.five.Shift.r', 
    'The time between the "5" key coming up to the time to press down the "shift+r" key combination.',
    'H.Shift.r', 
    'The amount of time that the "shift+r" key combination is held down.',
    'DD.Shift.r.o', 
    'The time between pressing down the "shift+r" key combination to the time to press down the "o" key.',
    'UD.Shift.r.o', 
    'The time between the "shift+r" key combination coming up to the time to press down the "o" key.',
    'H.o', 
    'The amount of time that the "o" is held down.',
    'DD.o.a', 
    'The time between pressing down the "o" key to the time to press down the "a" key.',
    'UD.o.a', 
    'The time between the "o" key coming up to the time to press down the "a" key.',
    'H.a', 
    'The amount of time that the "a" is held down.',
    'DD.a.n', 
    'The time between pressing down the "a" key to the time to press down the "n" key.',
    'UD.a.n', 
    'The time between the "a" key coming up to the time to press down the "n" key.',
    'H.n', 
    'The amount of time that the "n" is held down.',
    'DD.n.l', 
    'The time between pressing down the "n" key to the time to press down the',
    'UD.n.l', 
    'The time between the "n" key coming up to the time to press down the "l" key.',
    'H.l', 
    'The amount of time that the "l" is held down.',
    'DD.l.Return', 
    'The time between pressing down the "l" key to the time to press down the "return" key.',
    'UD.l.Return', 
    'The time between the "l" key coming up to the time to press down the "return" key.',
    'H.Return', 
    'The amount of time that the "return" is held down.',
    'session', 
    'A session is a block of time where an individual has had access to the system over a continuous block of time.',
    'rep', 
    'The individual passcode entries within a session are referred to as a repetition-within-session of the passcode entry.',
    'subject', 
    'IDs for all 51 unique individuals.'
  ), 
  ncol = 2, 
  byrow = T)


known.DataDictionary = data.frame(known.DataDictionary)
names(known.DataDictionary) = c("Variable", "Description")

# write_csv(known.DataDictionary, "data.dictionary.csv")





# "H.period", 
# "H.t", 
# "H.i", 
# "H.e", 
# "H.five", 
# "H.Shift.r", 
# "H.o", 
# "H.a", 
# "H.n", 
# "H.l", 
# "H.Return", 
# "DD.period.t", 
# "DD.t.i", 
# "DD.i.e", 
# "DD.e.five", 
# "DD.five.Shift.r", 
# "DD.Shift.r.o", 
# "DD.o.a", 
# "DD.a.n", 
# "DD.n.l", 
# "DD.l.Return", 
# "UD.period.t", 
# "UD.t.i", 
# "UD.i.e", 
# "UD.e.five", 
# "UD.five.Shift.r", 
# "UD.Shift.r.o", 
# "UD.o.a", 
# "UD.a.n", 
# "UD.n.l", 
# "UD.l.Return"









