if (FALSE){
  hist_1 = history
  load('runs/boston_cv_history_12000.Rdata')
}
#load('runs/boston_cv_history_15000.Rdata')
#history = read.table('runs/boston_cv_history15000.csv')
library(ggplot2)
  
history$step = as.integer(history$step)
history$fold = as.integer(history$fold)
history$nll_train = as.numeric(history$nll_train)
history$nll_test = as.numeric(history$nll_test)
history$OK = NULL# = as.numeric(history$OK)

library(tidyr)
h = gather(history, 'sample', 'loss', nll_train:nll_test)
h$loss = as.numeric(h$loss)
h$sample = as.factor(h$sample)
h$fold = as.factor(h$fold)
hh =h[!is.na(h$loss),] 

ggplot(hh, aes(x=step,y=loss, color=sample, linetype=fold)) +
geom_hline(yintercept=2.48)+  geom_line()  + ylim(2.2,3.2) + facet_grid(. ~ method)
ggsave('boston_cv_eval_12000_5fold.pdf', width = 14, height = 7)

library(dplyr)
#dd = hh %>% filter(step == 12000) %>% filter(sample == 'nll_test') 
ddd = history %>% filter(step == 71000)
mean(ddd$nll_test) 
sd(ddd$nll_test) / sqrt(5)


if (FALSE){
  history = history[2:nrow(history),]
  ggplot(history) + ylim(-2, 2) + geom_line(aes(x = step, y=nll_test, col=method))
}

if (FALSE){
  #Wrong naming in third run (should be model_7_reg_0.05)
  history[201:300,'method'] = 'model_7_reg_0.05'
  history[501:600,'method'] = 'model_7_reg_0.05'
  history[801:900,'method'] = 'model_7_reg_0.05'
  history[1101:1200,'method'] = 'model_7_reg_0.05'
  history[1401:1500,'method'] = 'model_7_reg_0.05'
}

