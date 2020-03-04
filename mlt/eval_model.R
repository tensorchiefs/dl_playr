if (FALSE){
  hist_1 = history
  hist_select = "c:/Users/sick/dl Dropbox/beate sick/IDP_Projekte/DL_Projekte/shared_Oliver_Beate/mlt/UCI_Datasets/bostonHousing/2020-03-0435.574_grid_search/history_7000.Rdata"
  path_result = "c:/Users/sick/dl Dropbox/beate sick/IDP_Projekte/DL_Projekte/shared_Oliver_Beate/mlt/UCI_Datasets/bostonHousing/2020-03-0435.574_grid_search/"
  load(hist_select)
}
#load('runs/boston_cv_history_15000.Rdata')
#history = read.table('runs/boston_cv_history15000.csv')
library(ggplot2)

history = hist_grid
summary(history)
  
# history$step = as.integer(history$step)
# history$fold = as.integer(history$fold)
# history$nll_train = as.numeric(history$nll_train)
# history$nll_test = as.numeric(history$nll_test)
# history$OK = NULL# = as.numeric(history$OK)

library(tidyr)
h = gather(history, 'sample', 'loss', nll_train:nll_test)
# h$loss = as.numeric(h$loss)
h$sample = as.factor(h$sample)  # test or validation/test
# h$fold = as.factor(h$fold)
# hh =h[!is.na(h$loss),] 
summary(h)
str(h)


for(i in 1:length(levels(h1$x_scale)) ){
  for( j in 1:length(levels(h1$spatz))){
    for( k in 1:length(levels(h1$regularization))){
      # i=1
      # j=2
      # k=3
      h1 = h
      
      x_scale_pick = levels(h1$x_scale)[i]
      h1= h1[h1$x_scale==x_scale_pick,]

      spatz_pick = levels(h1$spatz)[j]
      h1= h1[h1$spatz==spatz_pick,]
      
      regularization_pick = levels(h1$regularization)[k]
      h1= h1[h1$regularization==regularization_pick,]
      
      main = h1$method[1] 
      sub = paste("x_scale_", x_scale_pick,
                  "--spatz_", spatz_pick ,
                  "--regularization_", regularization_pick, sep=""  ) 
 
      ggplot(data=h1, aes(x = step, y = loss, color=fold)) + 
        geom_line() +
        geom_hline(yintercept=2.48) +
        ylim(2.0,4.5) +
        ggtitle(main, subtitle=sub) +
        facet_wrap(. ~ sample, ncol=2)
      
      ggsave(paste0(path_result, "loss_",main,sub,".png"), 
             plot = last_plot(), width = 14, height = 14)
      
    }
  }
}



# library(dplyr)
# #dd = hh %>% filter(step == 12000) %>% filter(sample == 'nll_test') 
# ddd = history %>% filter(step == 71000)
# mean(ddd$nll_test) 
# sd(ddd$nll_test) / sqrt(5)


