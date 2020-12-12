
### Hold - Transform to wide format, makeing it easier to explore correlation
keyHold_wide = known.Long.KeyHold %>% 
  select(-variable) %>% 
  spread(Key.Press, value) %>% 
  select(-subject, -Key.Action, -row.id)

### Up Down - Transform to wide format, makeing it easier to explore correlation
keyupdown_wide = known.Long.KeyUpDown %>% 
  select(-variable) %>% 
  spread(Key.Press, value) %>% 
  select(-subject, -Key.Action, -row.id)

### Down Down - Transform to wide format, makeing it easier to explore correlation
keydowndown_wide = known.Long.KeyDownDown %>% 
  select(-variable) %>% 
  spread(Key.Press, value) %>% 
  select(-subject, -Key.Action, -row.id)

### Helper funtion for performing ocrrelation
corrHelper = function(x,y) {
  # corr = cor(x, y)
  # CORRELATION_THRESHOLD = 0.5
  # ifelse(cor(x, y) > CORRELATION_THRESHOLD, "X", NA)
  
  corr = apply(cor(x, y), c(1,2), function(num){
    ifelse(num >= 0.9, "++", 
           ifelse(num >= 0.7, "+",
                  ifelse(num <= -0.9, "--", 
                         ifelse(num <= -0.7, "-", NA))))
  })
  corr
}

corrHelper(keyHold_wide, keyupdown_wide)
corrHelper(keyHold_wide, keydowndown_wide)
corrHelper(keydowndown_wide, keyupdown_wide)

corrHelper(keyHold_wide, keyHold_wide)
corrHelper(keydowndown_wide, keydowndown_wide)
corrHelper(keyupdown_wide, keyupdown_wide)


require(ggplot2)

plotData = cbind(
  known.Long.KeyUpDown %>% 
    select(-variable, -subject) %>% 
    spread(Key.Action, value) %>% 
    select(-row.id) %>% 
    rename(x = "Up-Down", key = "Key.Press"),
  known.Long.KeyDownDown %>% 
    select(-variable, -subject) %>% 
    spread(Key.Action, value) %>% 
    select(-row.id, -Key.Press) %>% 
    rename(y = "Down-Down")
) %>% 
  group_by(key) %>% 
  do({
    cor_ = round(cor(.$x, .$y), 4)
    cor_ = paste(.$key[1], cor_, sep = " = ")
    
    data.frame(
      ., 
      correlationLabel = cor_, 
      sortIndex = which(!is.na(match(KeyPress.Key.Transition, .$key[1]))), 
      stringsAsFactors = F)
  }) %>%
  ungroup() %>% 
  mutate(correlationLabel = factor(correlationLabel, unique(correlationLabel)))

# labeller.helper = function(keys, ..) {
#   correlations = lapply(as.character(keys$key), function(key_){
#     xy = plotData %>% 
#       filter(key == key_) %>% 
#       select(-key)
#     
#     # list(key_, round(cor(xy$x, xy$y), 4))
#     round(cor(xy$x, xy$y), 4)
#   })
#   k = data.frame(key = paste(keys$key, correlations, sep = " = "))
#   browser()
#   
# }


plotData %>% 
  ggplot(aes(x, y)) + 
  geom_smooth(method = "lm", se=FALSE, formula = y ~ x) + 
  geom_point() + 
  facet_wrap(~ correlationLabel, scales = "free") + 
  labs(
    title = "Exploring correlation between, Down-Down vs Up-Down",
    x = "Up-Down",
    y = "Down-Down"
  )


dodgePosition = position_dodge(0.25)

plotData = data.frame(subject = simplifiedSubjectLabels) %>% 
  group_by(subject) %>% 
  do({
    temp = known.Long.PerSubject %>%
      mutate(subject.new = ifelse(subject == .$subject[1], "1", "0")) %>% 
      group_by(subject.new, Key.Press) %>% 
      summarise(
        min = min(value),
        max = max(value),
        mean = mean(value)
      )
    
    data.frame(temp)
  })

plotData %>% 
  ggplot(aes(
    x = Key.Press, 
    y = mean, 
    group = subject.new, 
    color = subject.new)) + 
  geom_errorbar(aes(y = max, ymin = min, ymax = max), width = .25, position = dodgePosition) + 
  geom_line(position = dodgePosition) + 
  geom_point(size = 2, shape = 21, fill = "white", position = dodgePosition) + 
  facet_wrap(~ subject, scales = "free_y") + 
  labs(
    title = "Exploring distribution of time for each known subject",
    x = "Key Sequence",
    y = "Time"
  )



rm(
  keyHold_wide, 
  keyupdown_wide, 
  keydowndown_wide, 
  corrHelper,
  plotData)







