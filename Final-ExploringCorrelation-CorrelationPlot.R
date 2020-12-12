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


corr.plot <- plotData %>% 
  ggplot(aes(x, y)) + 
  geom_smooth(method = "lm", se=FALSE, formula = y ~ x) + 
  geom_point() + 
  facet_wrap(~ correlationLabel, scales = "free") + 
  labs(
    title = "Exploring correlation between, Down-Down vs Up-Down",
    x = "Up-Down",
    y = "Down-Down"
  )


rm(plotData)