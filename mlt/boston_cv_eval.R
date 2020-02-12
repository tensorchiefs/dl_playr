load('runs/boston_cv_history_15000.Rdata')
#history = read.table('runs/boston_cv_history15000.csv')
library(ggplot2)
  
history$step = as.integer(history$step)
history$fold = as.integer(history$fold)
history$nll_train = as.numeric(history$nll_train)
history$nll_test = as.numeric(history$nll_test)
history$OK = NULL# = as.numeric(history$OK)

# Adding mlt for plot
# to_add = history[history$method == 'model_3',]
# to_add$step = max(history$step, na.rm = TRUE)
# history = rbind(history, to_add)

library(tidyr)
h = gather(history, 'sample', 'loss', nll_train:nll_test)
h$loss = as.numeric(h$loss)
h$sample = as.factor(h$sample)
h$fold = as.factor(h$fold)
hh =h[!is.na(h$loss),] 

ggplot(hh, aes(x=step,y=loss, color=sample, linetype=fold)) + 
  ylim(-1,15)+ geom_hline(yintercept=2.5)+  geom_line() + facet_grid( ~ method)



