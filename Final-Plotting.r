
library(ggplot2)
library(gridExtra)

## https://github.com/clauswilke/ggridges
library(ggridges)


####
#
# Heatmap prep function
#
####
heatMapHelper = function(plotData, act){
  
  facetLabeller = function(groups_){
    c(
      KEY_ACTION_HOLD = "Key hold", 
      KEY_ACTION_DOWN_DOWN = "Key Down-Down", 
      KEY_ACTION_UP_DOWN = "Key Up-Down")[groups_$Key.Action]
  }
  
  plot = ggplot(plotData, aes(y = Key.Press, x = subject)) + 
    geom_tile(aes(fill = avg), color = "white") +
    facet_wrap(~ Key.Action, labeller = facetLabeller) +
    scale_fill_gradient(low = "white", high = "red")
  
  ## Pad y-axis labels so plots aligns
  plot = plot + scale_y_discrete(labels = str_pad(plotData$Key.Press, 10)) 
  
  ## Hide legend
  plot = plot + theme(legend.position = "none")
  
  ## Hide x-axis title, tick, and label; as well as y-axis title
  plot = plot + theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
  
  ## Show x-axis title, label and tick for bottom plot
  if(act == KEY_ACTION_HOLD) {
    plot = plot + 
      theme(axis.text.x = element_text())
  }
  
  plot
}

plotHeatMap = function(data, main = "Password entry average key press/transitions") {
  ## Prep heat map
  passwordEntriesHeatMap = lapply(
    unique(known.Long.PerSubject$Key.Action), 
    function(act) {
      ## Prep plot data
      plotData = data %>% 
        filter(Key.Action == act) %>% 
        group_by(subject, Key.Action, Key.Press) %>% 
        summarise(avg = mean(value))
      
      heatMapHelper(plotData, act)
    })
  
  ## Plot heat map
  do.call(grid.arrange, c(
    rev(passwordEntriesHeatMap), 
    ncol = 1, 
    top = main, 
    left = "Key Press/Transition") )
}

plotHeatMap(known.Long.PerSubject)

## Errobar plot for different numeric variables
known.NumericPredictorSummaryPerSubject %>% 
  mutate(group = 1) %>% 
  ggplot(aes(x = subject)) + 
  geom_errorbar(aes(ymin = Min, ymax = Max), width = 0.625) + 
  geom_point(aes(y = Mean), color = "blue") + 
  geom_line(aes(y = Mean, group = group), color = "blue") + 
  geom_point(aes(y = Median), color = "red") + 
  geom_line(aes(y = Median, group = group), color = "red") + 
  theme(axis.text.x = element_text(angle = -90, hjust = 1)) + 
  facet_wrap(~ variable, scales = "free_y", ncol = 4) + 
  scale_x_discrete(breaks = 1:51, labels= c(letters, LETTERS)[1:51])

## Ridge plot for individual password characters, per subject
ggplot(known.Long.KeyHold, aes(y = subject, x = value)) +
  geom_density_ridges(scale = 0.85) + 
  facet_wrap(~ Key.Press, ncol = 11, scales = "free_x") + 
  scale_x_continuous(breaks = seq(0, 0.4, 0.1)) + 
  labs(
    title = "Distribution of key press of password characters, per subject",
    y = "Subject",
    x = "Time"
  )

## Ridge plot for Key transition (Down-Down), per subject
ggplot(known.Long.KeyDownDown, aes(y = subject, x = value)) +
  geom_density_ridges(scale = 0.85) + 
  facet_wrap(~ Key.Press, ncol = 11, scales = "free_x") + 
  scale_x_continuous(breaks = seq(0, 5, 1)) + 
  labs(
    title = "Distribution of Key transition per subject - Down-Down",
    y = "Subject",
    x = "Time"
  )

## Ridge plot for key transition (Up-Down), per subject
ggplot(known.Long.KeyUpDown, aes(y = subject, x = value)) +
  geom_density_ridges(scale = 0.85) + 
  facet_wrap(~ Key.Press, ncol = 11, scales = "free_x") + 
  scale_x_continuous(breaks = seq(0, 5, 1)) + 
  labs(
    title = "Distribution of Key transition per subject - Up-Down",
    y = "Subject",
    x = "Time"
  )


### Summary plot for all 3 key actions
plotKeyActionSummary = function(data_, mainLabel){
  ### Ridge plot
  ridge.plot = ggplot(data_, aes(y = Key.Press, x = value, fill = Key.Press)) + 
    geom_density_ridges(scale = 0.85) + 
    theme(legend.position = "none") + 
    labs(y = "Key",
         x = "Value (Time in MilliSeconds)")
  
  ### Violin plot
  violin.plot = ggplot(data_, aes(x = Key.Press, y = value, fill = Key.Press)) + 
    geom_violin() + 
    theme(
      axis.title.x = element_blank(),
      legend.position = "none") + 
    labs(y = "Value (Time in MilliSeconds)")
  
  ### Boxplot
  box.plot = ggplot(data_, aes(x = Key.Press, y = value)) + 
    geom_boxplot() + 
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank())
  
  ### https://www.r-bloggers.com/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
  grid.arrange(
    ridge.plot,
    arrangeGrob(violin.plot, box.plot, ncol = 2),
    nrow = 2,
    top = mainLabel
  )
}

x11()
plotKeyActionSummary(known.Long.PerSubject %>% filter(Key.Action == KEY_ACTION_HOLD), "Key Action - Hold")

x11()
plotKeyActionSummary(known.Long.PerSubject %>% filter(Key.Action == KEY_ACTION_DOWN_DOWN), "Key Action - Down-Down")

x11()
plotKeyActionSummary(known.Long.PerSubject %>% filter(Key.Action == KEY_ACTION_UP_DOWN), "Key Action - Up-Down")




rbind(
  known %>% 
    select(subject) %>% 
    mutate(source = "All"),
  known.Test %>%  
    select(subject) %>% 
    mutate(source = "Test"),
  known.Train %>%  
    select(subject) %>% 
    mutate(source = "Train")
) %>% 
  ggplot(aes(x = subject)) + 
  geom_bar() + 
  facet_wrap(~ source, ncol = 1) + 
  labs(
    title = "Distibution of subjects across known data set",
    y = "Count",
    x = "subject"
  )
















